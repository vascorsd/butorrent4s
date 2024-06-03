// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

// TODO:
//   - replace all of it by cats parser or any other custom parser combinators logic... (?)

package butorrent4s

import scala.math.Ordering.Implicits.*

import cats.syntax.either.*
import scodec.bits.*

import butorrent4s.Bencode
import butorrent4s.Bencode.*
import ParseError.*

import scala.annotation.tailrec

type ParseResult[+A] = Either[ParseError, (A, ByteVector)]

def decode(input: String): ParseResult[Bencode] =
  decode(input.getBytes("UTF-8"))

def decode(input: Array[Byte]): ParseResult[Bencode] =
  decode(ByteVector(input))

def decode(input: ByteVector): ParseResult[Bencode] =
  oneOfP(input)

// -------- Internals ----------

// choice / alternative by peeking on first next char without consuming it.
private[butorrent4s] def oneOfP(
    input: ByteVector
): ParseResult[Bencode] = {

  input.headOption match {
    case Some(`i`)                  => integerP(input)
    case Some(`l`)                  => listP(input)
    case Some(`d`)                  => dictionaryP(input)
    case Some(b) if isASCIIDigit(b) => byteStringP(input)

    case Some(b) =>
      unexpected(
        ParseContext.OneOf,
        b,
        ExpectedToken.I,
        ExpectedToken.L,
        ExpectedToken.D,
        ExpectedToken.Digit
      ).asLeft

    case None =>
      unexpectedEOI(
        ParseContext.OneOf,
        ExpectedToken.I,
        ExpectedToken.L,
        ExpectedToken.D,
        ExpectedToken.Digit
      ).asLeft
  }
}

private[butorrent4s] def byteStringP(
    input: ByteVector
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
      in: ByteVector,
      digitsSeen: List[Byte]
  ): ParseResult[BString] = {
    in.headOption match {
      // Reached the ':' and found some digits.
      case Some(`colon`) if digitsSeen.nonEmpty =>
        val strLenBytes = digitsSeen.reverse.toArray

        // - valid to parse since we are sure only digits are here.
        // - using Int since max size of string in jvm is Integer.MAX_VALUE.
        val strLen = String(strLenBytes, "UTF-8").toIntOption

        for {
          len   <- strLen.toRight(InvalidString(StringErrDetail.ParsingLen(ByteVector(strLenBytes))))
          value <- in.tail
                     .consume(len) { data =>
                       bstring(data).asRight
                     }
                     .leftMap(_ => InvalidString(StringErrDetail.ParsingDataInsuficient(len)))
        } yield value.swap

      // Next char is a digit, accumulate it, check next.
      case Some(d) if isASCIIDigit(d)           =>
        loop(in = in.tail, digitsSeen = d :: digitsSeen)

      // No delimiter ':' and no digits, bad input
      case Some(b)                              =>
        val expected =
          if digitsSeen.isEmpty
          then { ExpectedToken.Digit :: Nil }
          else { ExpectedToken.Digit :: ExpectedToken.Colon :: Nil }

        unexpected(
          ParseContext.BString,
          b,
          expected*
        ).asLeft

      // No more input available
      case None                                 =>
        val expected =
          if digitsSeen.isEmpty
          then { ExpectedToken.Digit :: Nil }
          else { ExpectedToken.Digit :: ExpectedToken.Colon :: Nil }

        unexpectedEOI(
          ParseContext.BString,
          expected*
        ).asLeft

    }
  }

  loop(in = input, digitsSeen = List.empty)
}

private[butorrent4s] def integerP(
    input: ByteVector
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
      in: ByteVector,
      digitsSeen: List[Byte]
  ): ParseResult[BInteger] = {
    in match {
      case Array(`e`, unparsed*) =>
        val digits = digitsSeen.reverse.toArray

        String(digits, "UTF-8").toLongOption
          .map { p =>
            // we need to recover the negative encoding for the number
            val n = if isNegative then -p else p

            (binteger(n), unparsed.toArray).asRight
          }
          .getOrElse(
            InvalidInteger.parsing(digits).asLeft
          )

      case Array(x, xs*) if isASCIIDigit(x) =>
        loop(
          isNegative = isNegative,
          in = xs.toArray,
          digitsSeen = x :: digitsSeen
        )

      case Array(x, _*) =>
        unexpected(
          ParseContext.BInteger,
          x,
          ExpectedToken.Digit,
          ExpectedToken.End
        ).asLeft

      case Array(_*) =>
        unexpectedEOI(
          ParseContext.BInteger,
          ExpectedToken.Digit,
          ExpectedToken.End
        ).asLeft

    }
  }

  input match {
    // unroll specific cases:
    //  1. exactly zero encoded.
    //  2. anything else starting with zero, invalid.
    //  2. -0  -> no negative zero concept.
    case Array(`i`, `zero`, `e`, xs*) =>
      (binteger(0L), xs.toArray).asRight

    case Array(`i`, `zero`, _*) =>
      InvalidInteger.leadingZero().asLeft

    case Array(`i`, `minus`, `zero`, _*)                =>
      InvalidInteger.negativeZero().asLeft

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

    case Array(`i`, xs*) =>
      unexpectedEOI(ParseContext.BInteger, ExpectedToken.Digit).asLeft

    case Array(_*) =>
      unexpectedEOI(ParseContext.BInteger, ExpectedToken.I).asLeft
  }
}

private[butorrent4s] def listP(
    input: ByteVector
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

  @tailrec
  def loop(
      in: ByteVector,
      elems: List[Bencode]
  ): ParseResult[BList] = {
    in match {
      case Array(`e`, unparsed*) =>
        (blist(elems.reverse), unparsed.toArray).asRight

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: cannot use flatmap method because compiler errors out with "not in tail position"

        oneOfP(in) match {
          case err @ Left(_)             => err.rightCast
          case Right((parsed, unparsed)) =>
            loop(in = unparsed, elems = parsed :: elems)
        }
    }
  }

  input match {
    case Array(`l`, xs*) =>
      loop(in = xs.toArray, elems = List.empty)

    case Array(x, _*) =>
      unexpected(ParseContext.BList, x, ExpectedToken.L).asLeft

    case Array(_*) =>
      unexpectedEOI(ParseContext.BList, ExpectedToken.L).asLeft
  }
}

private[butorrent4s] def dictionaryP(
    input: ByteVector
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
      in: ByteVector,
      elems: List[(BString, Bencode)]
  ): ParseResult[BDictionary] = {
    in match {
      case Array(`e`, unparsed*) =>
        (bdictionary(elems.reverse), unparsed.toArray).asRight

      case _ =>
        // composition step, one parser after the next, monadic bind, flatmap, etc
        // note: manually unrolled since compiler can't work out the flatmaps

        byteStringP(in) match {
          case err @ Left(_)                => err.rightCast
          case Right((parsedKey, unparsed)) =>
            // enforce ordering (lexicographic) of keys an no duplicates,
            val isNewKeyValid =
              elems.headOption
                .map { (prevKey, _) =>
                  parsedKey > prevKey
                }
                .getOrElse(true)

            if isNewKeyValid then {
              // read the value:
              oneOfP(unparsed) match {
                case err @ Left(_)                  => err.rightCast
                case Right((parsedValue, unparsed)) =>
                  loop(in = unparsed, elems = (parsedKey, parsedValue) :: elems)
              }
            } else {
              InvalidDictionary
                .unorderedOrEqualKeys(elems.head._1, parsedKey)
                .asLeft
            }
        }
    }
  }

  input match {
    case Array(`d`, xs*) =>
      loop(in = xs.toArray, elems = List.empty)

    case Array(x, _*) =>
      unexpected(ParseContext.BDictionary, x, ExpectedToken.D).asLeft
  }
}

private val i: Byte     = 0x69 // 'i'
private val l: Byte     = 0x6c // 'l'
private val d: Byte     = 0x64 // 'd'
private val e: Byte     = 0x65 // 'e'
private val zero: Byte  = 0x30 // '0'
private val nine: Byte  = 0x39 // '9'
private val minus: Byte = 0x2d // '-'
private val colon: Byte = 0x3a // ':'

private def isASCIIDigit(c: Byte) = zero <= c && c <= nine

// ------ Failure Details ------:

enum ParseError {
  case Unexpected(ctx: ParseContext, found: Byte, expected: List[ExpectedToken])
  case UnexpectedEOI(ctx: ParseContext, expected: List[ExpectedToken])

  case InvalidString(detail: StringErrDetail)
  case InvalidInteger(detail: IntegerErrDetail)
  case InvalidDictionary(detail: DictErrDetail)
}

object ParseError {
  def unexpected(ctx: ParseContext, found: Byte, expected: ExpectedToken*) =
    Unexpected(ctx, found, expected.toList)

  def unexpectedEOI(ctx: ParseContext, expected: ExpectedToken*) =
    UnexpectedEOI(ctx, expected.toList)

  enum ParseContext {
    case BInteger, BList, BDictionary, BString, OneOf
  }

  enum ExpectedToken {
    case I, L, D, Digit, End, Colon
  }

  enum StringErrDetail {
    case ParsingLen(givenLen: ByteVector)
    case ParsingDataInsuficient(wanted: Int)
  }

  enum IntegerErrDetail {
    case Parsing(found: ByteVector)
    case LeadingZero
    case NegativeZero
  }

  enum DictErrDetail {
    case UnorderedOrEqualKeys(prev: BString, next: BString)
  }

  // ----- helpers to build errors easier -----
  extension (ii: InvalidInteger.type) {
    def parsing(found: ByteVector) = InvalidInteger(IntegerErrDetail.Parsing(found))
    def leadingZero()              = InvalidInteger(IntegerErrDetail.LeadingZero)
    def negativeZero()             = InvalidInteger(IntegerErrDetail.NegativeZero)
  }

  extension (ii: InvalidDictionary.type) {
    def unorderedOrEqualKeys(prev: BString, next: BString) =
      InvalidDictionary(
        DictErrDetail.UnorderedOrEqualKeys(prev, next)
      )
  }
}
