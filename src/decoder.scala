// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

// TODO:
//   - probably change option to Eithers with specific error information
//   - replace all of it by cats parser or any other custom parser combinators logic... (?)

package butorrent4s

import scala.math.Ordering.Implicits._
import scala.annotation.tailrec
import butorrent4s.Bencode
import butorrent4s.Bencode.*

type ParseResult[+A] = Option[(A, Array[Byte])]

def decode(input: String): ParseResult[Bencode] =
  decode(input.getBytes("UTF-8"))

def decode(input: Array[Byte]): ParseResult[Bencode] =
  choiceP(input)

// -------- Internals ----------

// choice / alternative by peeking on first next char without consuming it.
private[butorrent4s] def choiceP(
    input: Array[Byte]
): ParseResult[Bencode] = {
  input match
    case Array(`i`, _*)                  => integerP(input)
    case Array(`l`, _*)                  => listP(input)
    case Array(`d`, _*)                  => dictionaryP(input)
    case Array(b, _*) if isASCIIDigit(b) => byteStringP(input)
    case _                               => None
}

private[butorrent4s] def byteStringP(
    input: Array[Byte]
): ParseResult[BString] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Byte_Strings
  //
  // Byte strings are encoded as follows: <string length encoded in base ten ASCII>:<string data>
  // Note that there is no constant beginning delimiter, and no ending delimiter.
  //
  //    Example: 4:spam represents the string "spam"
  //    Example: 0: represents the empty string ""

  @tailrec
  def loop(
      in: Array[Byte],
      digitsSeen: List[Byte]
  ): ParseResult[BString] = {
    in match {
      // Reached the ':' and found some digits.
      case Array(`colon`, xs*) if digitsSeen.nonEmpty =>
        // doesn't fail, since we checked it's digits we are parsing.
        // parsing to Int, since max size of string in jvm is Integer.MAX_VALUE.
        val strLen = String(digitsSeen.reverse.toArray, "UTF-8").toIntOption
        val strData = xs.toArray

        // check that we have at least the amount of data requested in the input.
        strLen
          .filter { len => strData.length >= len }
          .map { len =>
            val (parsed, remaining) = strData.splitAt(len)

            (bstring(parsed), remaining)
          }

      // Next char is a digit, accumulate it, check next.
      case Array(x, xs*) if isASCIIDigit(x) =>
        loop(in = xs.toArray, digitsSeen = x :: digitsSeen)

      // No delimiter ':' and no digits, bad input
      case _ => None
    }
  }

  loop(in = input, digitsSeen = List.empty)
}

private[butorrent4s] def integerP(
    input: Array[Byte]
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

  // notes:
  // 1. number must be parsed to Long (64 bits) at least
  // 2. leading 0 is bad
  // 3. determine if negative number

  @tailrec
  def loop(
      isNegative: Boolean,
      in: Array[Byte],
      digitsSeen: List[Byte]
  ): ParseResult[BInteger] = {
    in match {
      case Array(`e`, unparsed*) =>
        val digits = digitsSeen.reverse.toArray

        // we need to recover the negative encoding for the number
        def negate(l: Long) = if isNegative then -l else l

        String(digits, "UTF-8").toLongOption
          .map(p => (binteger(negate(p)), unparsed.toArray))

      case Array(x, xs*) if isASCIIDigit(x) =>
        loop(
          isNegative = isNegative,
          in = xs.toArray,
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
    case Array(`i`, `zero`, `e`, xs*)    => Some((BInteger(0L), xs.toArray))
    case Array(`i`, `zero`, _, _*)       => None
    case Array(`i`, `minus`, `zero`, _*) => None

    // looping cases:
    case Array(`i`, `minus`, x, xs*) if isASCIIDigit(x) =>
      loop(
        isNegative = true,
        in = xs.toArray,
        digitsSeen = x :: Nil
      )

    case Array(`i`, x, xs*) if isASCIIDigit(x) =>
      loop(
        isNegative = false,
        in = xs.toArray,
        digitsSeen = x :: Nil
      )

    case _ =>
      None
  }
}

private[butorrent4s] def listP(
    input: Array[Byte]
): ParseResult[BList] = {
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
  // 1. List can have any type, it's an HList as more common in scala.

  @tailrec
  def loop(
      in: Array[Byte],
      elems: List[Bencode]
  ): ParseResult[BList] = {
    in match
      case Array(`e`, unparsed*) =>
        Some((BList(elems.reverse), unparsed.toArray))

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: cannot use flatmap method because cmompiler errors out with "not in tail position"

        choiceP(in) match {
          case Some((parsed, unparsed)) =>
            loop(in = unparsed, elems = parsed :: elems)

          case None => None
        }
  }

  input match
    case Array(`l`, xs*) => loop(in = xs.toArray, elems = List.empty)
    case _               => None
}

private[butorrent4s] def dictionaryP(
    input: Array[Byte]
): ParseResult[BDictionary] = {
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
      in: Array[Byte],
      elems: List[(BString, Bencode)]
  ): ParseResult[BDictionary] = {
    in match
      case Array(`e`, unparsed*) =>
        Some((bdictionary(elems.reverse), unparsed.toArray))

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: manually unrolled since compiler can't work out the flatmaps

        byteStringP(in) match {
          case Some((parsedKey, unparsed)) =>
            // evaluate the keys as Strings to
            // enforce ordering (lexicographic) of the dict keys
            // and also no duplicates:
            val isNewKeyValid =
              elems.headOption
                .map { (prevKey, _) =>
                  parsedKey > prevKey
                }
                .getOrElse(true)

            if isNewKeyValid then
              // read the value:
              choiceP(unparsed) match {
                case Some((parsedValue, unparsed)) =>
                  loop(in = unparsed, elems = (parsedKey, parsedValue) :: elems)

                case None => None
              }
            else None

          case None => None
        }
  }

  input match
    case Array(`d`, xs*) => loop(in = xs.toArray, elems = List.empty)
    case _               => None
}

private val i: Byte = 0x69 // 'i'
private val l: Byte = 0x6c // 'l'
private val d: Byte = 0x64 // 'd'
private val e: Byte = 0x65 // 'e'
private val zero: Byte = 0x30 // '0'
private val nine: Byte = 0x39 // '9'
private val minus: Byte = 0x2d // '-'
private val colon: Byte = 0x3a // ':'

private def isASCIIDigit(c: Byte) = zero <= c && c <= nine
