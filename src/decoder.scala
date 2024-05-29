// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

// TODO:
//   - decode main entry point should decide which specific parsing to do.
//   - need to limit amount of info that can be pushed into us, to protect against infinite input
//   - probably change option to Eithers with specific error information
//   - output should return remaining input that has not being parsed. this will allow composition.
//     And also allows to replace the size check of the parse string.
//   - Use other encoding besides Lists...
//   - replace all of it by cats parser or any other custom parser combinators logic... (?)

package butorrent4s

import cats.data.NonEmptyList

import scala.annotation.tailrec
import butorrent4s.Bencode
import butorrent4s.Bencode.*

import java.nio.charset.Charset

type ParseResult[+A] = Option[(A, List[Char])]

def decode(rawInput: String): ParseResult[Bencode] = {
  parserChoice(rawInput.toList)
}

// choice / alternative by peeking on first next char without consuming it.
def parserChoice(input: List[Char]): ParseResult[Bencode] = {
  input match
    case 'i' :: _            => parserInteger(input)
    case 'l' :: _            => parserList(input)
    case 'd' :: _            => parserDictionary(input)
    case c :: _ if c.isDigit => parserByteString(input)
    case _                   => None
}

def parserByteString(
    input: List[Char]
): ParseResult[BString] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Byte_Strings
  //
  // Byte strings are encoded as follows: <string length encoded in base ten ASCII>:<string data>
  // Note that there is no constant beginning delimiter, and no ending delimiter.
  //
  //    Example: 4: spam represents the string "spam"
  //    Example: 0: represents the empty string ""

  @tailrec
  def loop(
      in: List[Char],
      digitsSeen: List[Char]
  ): ParseResult[BString] = {
    in match {
      // Reached the ':' and found some digits.
      case ':' :: xs if digitsSeen.nonEmpty =>
        // doesn't fail, since we checked it's digits we are parsing.
        // parsing to Int, since max size of string in jvm is Integer.MAX_VALUE.
        val strlen = digitsSeen.reverse.toArray.mkString.toIntOption

        // check that we have at least the amount of data requested in the input.
        // strlen
        //   .filter(len => xs.size >= len)
        //   .map(len => xs.take(len).toArray.mkString)

        strlen
          .filter(len => xs.size >= len)
          .map { len =>
            val (parsed, remaining) = xs.splitAt(len)

            (
              Bencode.BString(String(parsed.toArray)),
              remaining
            )
          }

      // Next char is a digit, accumulate it, check next.
      case x :: xs if x.isDigit =>
        loop(in = xs, digitsSeen = x :: digitsSeen)

      // No delimiter ':' and no digits, bad input
      case _ => None
    }
  }

  loop(in = input, digitsSeen = List.empty)
}

def parserInteger(
    input: List[Char]
): ParseResult[BInteger] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Integers
  //
  // Integers are encoded as follows: i<integer encoded in base ten ASCII>e
  // The initial i and trailing e are beginning and ending delimiters.
  //
  //    Example: i3e represents the integer "3"
  //    Example: i-3e represents the integer "-3"
  //
  // i-0e is invalid. All encodings with a leading zero, such as i03e, are invalid, other than i0e,
  // which of course corresponds to the integer "0".
  //
  //    NOTE: The maximum number of bit of this integer is unspecified, but to handle it as a signed 64bit
  //    integer is mandatory to handle "large files" aka .torrent for more that 4Gbyte.

  // notes / tldr:
  // 1. number must be parsed to Long (64 bits) at least
  // 2. leading 0 is bad
  // 3. determine if negative number

  @tailrec
  def loop(
      isNegative: Boolean,
      in: List[Char],
      digitsSeen: NonEmptyList[Char]
  ): ParseResult[BInteger] = {
    in match {
      case 'e' :: unparsed =>
        val digits = digitsSeen.reverse

        // we need to recover the negative encoding since the acc only has digits,
        // not the negative sign.
        val numberToParse =
          if isNegative then '-' :: digits
          else digits

        String(numberToParse.toList.toArray).toLongOption
          .map(p => (Bencode.BInteger(p), unparsed))

      case x :: xs if x.isDigit =>
        loop(
          isNegative = isNegative,
          in = xs,
          digitsSeen = x :: digitsSeen
        )

      case _ =>
        None
    }
  }

  input match {
    // unroll specific cases:
    //  1. exactly zero encoded.
    //  2. anything else starting with zero, invalid.
    //  2. -0  -> no negative zero concept.
    case 'i' :: '0' :: 'e' :: xs => Some((BInteger(0L), xs))
    case 'i' :: '0' :: x :: xs   => None
    case 'i' :: '-' :: '0' :: xs => None

    // looping cases:
    case 'i' :: '-' :: x :: xs if x.isDigit =>
      loop(
        isNegative = true,
        in = xs,
        digitsSeen = NonEmptyList.one(x)
      )

    case 'i' :: x :: xs if x.isDigit =>
      loop(
        isNegative = false,
        in = xs,
        digitsSeen = NonEmptyList.one(x)
      )

    case _ =>
      None
  }
}

def parserList(input: List[Char]): ParseResult[BList] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Lists
  //
  // Lists are encoded as follows: l<bencoded values>e
  // The initial l and trailing e are beginning and ending delimiters.
  // Lists may contain any bencoded type, including integers, strings, dictionaries,
  // and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []Lists are encoded as follows: l<bencoded values>e
  //
  // The initial l and trailing e are beginning and ending delimiters.
  // Lists may contain any bencoded type, including integers, strings, dictionaries, and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []

  // notes:
  // 1. it seems nothing enforces that a list should have all elements be the same type.
  //    by the codecrafters info, possible to have a list of a string and a number. in scala this would be List[Any]

  @tailrec
  def loop(
      in: List[Char],
      elems: List[Bencode]
  ): ParseResult[BList] = {
    in match
      case 'e' :: unparsed =>
        Some((BList(elems.reverse), unparsed))

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: cannot use flatmap method because cmompiler errors out with "not in tail position"

        parserChoice(in) match {
          case Some((parsed, unparsed)) =>
            loop(in = unparsed, elems = parsed :: elems)

          case None => None
        }
  }

  input match
    case 'l' :: xs => loop(in = xs, elems = List.empty)
    case _         => None
}

def parserDictionary(input: List[Char]): ParseResult[BDictionary] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Dictionaries
  //
  // Dictionaries are encoded as follows: d<bencoded string><bencoded element>e
  // The initial d and trailing e are the beginning and ending delimiters.
  // Note that the keys must be bencoded strings. The values may be any bencoded type, including integers,
  // strings, lists, and other dictionaries.
  // Keys must be strings and appear in sorted order (sorted as raw strings, not alphanumerics).
  // The strings should be compared using a binary comparison, not a culture-specific "natural" comparison.
  //
  //    Example: d3:cow3:moo4:spam4:eggse represents the dictionary { "cow" => "moo", "spam" => "eggs" }
  //    Example: d4:spaml1:a1:bee represents the dictionary { "spam" => [ "a", "b" ] }
  //    Example: d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee represents { "publisher" => "bob", "publisher-webpage" => "www.example.com", "publisher.location" => "home" }
  //    Example: de represents an empty dictionary {}

  @tailrec
  def loop(
      in: List[Char],
      elems: List[(BString, Bencode)]
  ): ParseResult[BDictionary] = {
    in match
      case 'e' :: unparsed =>
        Some((BDictionary(elems.reverse), unparsed))

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: manually unroll since compiler can't work out the flatmaps

        parserByteString(in) match {
          case Some((parsedKey, unparsed)) =>
            // enforce ordering (lexicographic) of the dict keys, also no duplicates:
            val isNewKeyValid =
              elems.headOption
                .map { (prevKey, _) =>
                  parsedKey.v > prevKey.v
                }
                .getOrElse(true)

            if isNewKeyValid then
              // read the value:
              parserChoice(unparsed) match {
                case Some((parsedValue, unparsed)) =>
                  loop(in = unparsed, elems = (parsedKey, parsedValue) :: elems)

                case None => None
              }
            else None

          case None => None
        }
  }

  input match
    case 'd' :: xs => loop(in = xs, elems = List.empty)
    case _         => None
}
