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

import cats.data.{NonEmptyList, NonEmptyMap}

import scala.annotation.tailrec

type Parsed = String | Long
type Remaining = List[Char]
type ParseResult = Option[(Parsed, Remaining)]

def decode(rawInput: String) = {
  val r = rawInput.charAt(0) match
    case 'i' => parseInteger(rawInput.toList)
    case 'l' => ???
    case 'd' => ???
    case _   => parseByteString(rawInput.toList)

  println(r)
}

def parseByteString(
    input: List[Char]
): ParseResult = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Byte_Strings
  //
  // Byte strings are encoded as follows: <string length encoded in base ten ASCII>:<string data>
  // Note that there is no constant beginning delimiter, and no ending delimiter.
  //
  //    Example: 4: spam represents the string "spam"
  //    Example: 0: represents the empty string ""

  @tailrec
  def parseInnerLoop(
      input: List[Char],
      digitsSeen: List[Char]
  ): ParseResult = {
    input match {
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

            (String(parsed.toArray), remaining)
          }

      // Next char is a digit, accumulate it, check next.
      case x :: xs if x.isDigit =>
        parseInnerLoop(input = xs, digitsSeen = x :: digitsSeen)

      // No delimiter ':' and no digits, bad input
      case _ => None
    }
  }

  parseInnerLoop(input = input, digitsSeen = List.empty)
}

def parseInteger(
    input: List[Char]
): ParseResult = {
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
  def parseInnerLoop(
      isNegative: Boolean,
      input: List[Char],
      digitsSeen: NonEmptyList[Char]
  ): ParseResult = {
    input match {
      case 'e' :: unparsed =>
        val digits = digitsSeen.reverse

        // we need to recover the negative encoding since the acc only has digits,
        // not the negative sign.
        val numberToParse =
          if isNegative then '-' :: digits
          else digits

        String(numberToParse.toList.toArray).toLongOption
          .map(p => (p, unparsed))

      case x :: xs if x.isDigit =>
        parseInnerLoop(
          isNegative = isNegative,
          input = xs,
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
    case 'i' :: '0' :: 'e' :: xs => Some((0L, xs))
    case 'i' :: '0' :: x :: xs   => None
    case 'i' :: '-' :: '0' :: xs => None

    // looping cases:
    case 'i' :: '-' :: x :: xs if x.isDigit =>
      parseInnerLoop(
        isNegative = true,
        input = xs,
        digitsSeen = NonEmptyList.one(x)
      )

    case 'i' :: x :: xs if x.isDigit =>
      parseInnerLoop(
        isNegative = false,
        input = xs,
        digitsSeen = NonEmptyList.one(x)
      )

    case _ =>
      None
  }
}

def parseList() = {
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
  // 2. ...

  ???
}

def parseDictionary() = {
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

  ???
}
