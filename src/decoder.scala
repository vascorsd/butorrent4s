// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scala.annotation.tailrec

import cats.syntax.either.*
import scodec.bits.*

import butorrent4s.Bencode
import butorrent4s.Bencode.*
import butorrent4s.ParseError.{Context, *}
import butorrent4s.ParseError.DictErrDetail.InvalidKeyEncoding
import butorrent4s.ParseError.Expected.*

type ParseResult[+A] = Either[ParseError, (Long, ByteVector, A)]

def decode(input: String): ParseResult[Bencode]     = decode(ByteVector.view(input.getBytes("UTF-8")))
def decode(input: ByteVector): ParseResult[Bencode] = oneOfP(input)

// -------- Internals ----------

// Peeks on first char without consuming it to decide on the next parser to use.
def oneOfP(
    input: ByteVector,
    idx: Long = 0
): ParseResult[Bencode] = {
   val expected = I :: L :: D :: Digit :: Nil

   input.headOption match {
      case Some(b) if isASCIIDigit(b) => byteStringP(input, idx)
      case Some(`i`)                  => integerP(input, idx)
      case Some(`l`)                  => listP(input, idx)
      case Some(`d`)                  => dictionaryP(input, idx)
      case Some(b)                    => unexpected(Context.OneOf, idx, b, expected*).asLeft
      case None                       => unexpectedEnd(Context.OneOf, idx, expected*).asLeft
   }
}

def byteStringP(
    input: ByteVector,
    idx: Long = 0
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
       i: Long,
       in: ByteVector,
       digits: List[Byte]
   ): ParseResult[BString] = {
      in.headOption match {
         // Reached the ':' and found some digits.
         case Some(`colon`) if digits.nonEmpty =>
            val strLenBytes = digits.reverse.toArray

            // - valid to parse since we are sure only digits are here.
            // - using Int since max size of string in jvm is Integer.MAX_VALUE.
            val strLen = String(strLenBytes, "UTF-8").toIntOption

            for {
               len      <- strLen.toRight(InvalidString(StringErrDetail.ParsingLen(ByteVector(strLenBytes))))
               consumed <- in.tail
                              .consume(len) { data =>
                                 bstring(data).asRight
                              }
                              .leftMap(_ => InvalidString(StringErrDetail.ParsingDataInsuficient(len)))
            } yield {
               val (unparsed, value) = consumed

               (i + 1 + len, unparsed, value)
            }

         // Next char is a digit, accumulate it, check next.
         case Some(d) if isASCIIDigit(d)       =>
            loop(i + 1, in.tail, d :: digits)

         // No delimiter ':' and no digits, bad input
         case Some(b)                          =>
            val expected =
               if digits.isEmpty
               then { Digit :: Nil }
               else { Digit :: Colon :: Nil }

            unexpected(Context.BString, i, b, expected*).asLeft

         // No more input available
         case None                             =>
            val expected =
               if digits.isEmpty
               then { Digit :: Nil }
               else { Digit :: Colon :: Nil }

            unexpectedEnd(Context.BString, i, expected*).asLeft
      }
   }

   loop(idx, input, List.empty)
}

def integerP(
    input: ByteVector,
    idx: Long = 0
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
       i: Long,
       in: ByteVector,
       negative: Boolean,
       digits: List[Byte] // by design, always non-empty
   ): ParseResult[BInteger] = {
      in.headOption match {
         case Some(`e`) =>
            val digitsBytes = digits.reverse.toArray

            String(digitsBytes, "UTF-8").toLongOption
               .map { p =>
                  // we need to recover the negative encoding for the number
                  val n = if negative then -p else p

                  (i + 1, in.tail, binteger(n)).asRight
               }
               .getOrElse(InvalidInteger.parsing(ByteVector.view(digitsBytes)).asLeft)

         case Some(d) if isASCIIDigit(d) => loop(i + 1, in.tail, negative, d :: digits)
         case Some(b)                    => unexpected(Context.BInteger, i, b, Digit, End).asLeft
         case None                       => unexpectedEnd(Context.BInteger, i, Digit, End).asLeft
      }
   }

   input.headOption match {
      case Some(`i`) =>
         // unroll specific cases:
         //  1. exactly zero encoded.
         //  2. anything else starting with zero, invalid.
         //  2. -0  -> no negative zero concept.

         input.drop(1).headOption match {
            case Some(`zero`) =>
               input.drop(2).headOption match {
                  case Some(`e`) => (idx + 3, input.drop(3), binteger(0L)).asRight
                  case Some(b)   => unexpected(Context.BInteger, idx + 2, b, End).asLeft
                  case None      => unexpectedEnd(Context.BInteger, idx + 2, End).asLeft
               }

            case Some(`minus`) =>
               input.drop(2).headOption match {
                  case Some(b @ `zero`)           => InvalidInteger.negativeZero().asLeft
                  case Some(d) if isASCIIDigit(d) => loop(idx + 3, input.drop(3), negative = true, d :: Nil)
                  case Some(b)                    => unexpected(Context.BInteger, idx + 2, b, Digit).asLeft
                  case None                       => unexpectedEnd(Context.BInteger, idx + 2, Digit).asLeft
               }

            case Some(d) if isASCIIDigit(d) => loop(idx + 2, input.drop(2), negative = false, d :: Nil)
            case Some(b)                    => unexpected(Context.BInteger, idx + 1, b, Digit, Minus).asLeft
            case None                       => unexpectedEnd(Context.BInteger, idx + 1, Digit, Minus).asLeft
         }

      case Some(b) => unexpected(Context.BInteger, idx, b, I).asLeft
      case None    => unexpectedEnd(Context.BInteger, idx, I).asLeft
   }
}

def listP(
    input: ByteVector,
    idx: Long = 0
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
       i: Long,
       in: ByteVector,
       elems: List[Bencode]
   ): ParseResult[BList] = {
      in.headOption match {
         case Some(`e`) => (i + 1, in.tail, blist(elems.reverse)).asRight
         case Some(_)   =>
            oneOfP(in, i) match {
               case err @ Left(_)                    => err.rightCast
               case Right((elemL, unparsed, parsed)) =>
                  loop(elemL, unparsed, parsed :: elems)
            }
         case None      => unexpectedEnd(Context.BList, i, End, I, L, D, Digit).asLeft
      }
   }

   input.headOption match {
      case Some(`l`) => loop(idx + 1, input.tail, List.empty)
      case Some(b)   => unexpected(Context.BList, idx, b, L).asLeft
      case None      => unexpectedEnd(Context.BList, idx, L).asLeft
   }
}

def dictionaryP(
    input: ByteVector,
    idx: Long = 0
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
   //    Example: d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee represents
   //       { "publisher" => "bob", "publisher-webpage" => "www.example.com", "publisher.location" => "home" }
   //    Example: de represents an empty dictionary {}

   @tailrec
   def loop(
       i: Long,
       in: ByteVector,
       elems: List[(BString, Bencode)]
   ): ParseResult[BDictionary] = {
      in.headOption match {
         case Some(`e`) => (i + 1, in.tail, bdictionary(elems.reverse)).asRight
         case Some(_)   =>
            byteStringP(in, i) match {
               case err @ Left(_)                      => err.rightCast
               case Right((keyL, unparsed, parsedKey)) =>
                  // enforcing ordering (lexicographic) of keys and no duplicates.

                  // trying to get the new key in a textual representation.
                  parsedKey.tryIntoBStringOfString match {
                     // couldn't get a Text / String key from the Bstring bytes.
                     case None => InvalidDictionary(InvalidKeyEncoding(parsedKey)).asLeft

                     // new valid Text / String key.
                     case Some(nk) =>
                        // either we are on first key, so no previous key,
                        // or we have a previous key which we know is valid text and
                        // make sure is strictly after the previous one.
                        val maybePrevKey = elems.headOption.map(_._1.getStringUnsafe)
                        val validNewKey  = maybePrevKey.fold(true) { prevKey => nk.getStringUnsafe > prevKey }

                        if validNewKey
                        then {
                           // read the value.
                           oneOfP(unparsed, keyL) match {
                              case err @ Left(_)                          => err.rightCast
                              case Right((valueL, unparsed, parsedValue)) =>
                                 loop(valueL, unparsed, (nk, parsedValue) :: elems)
                           }
                        } else { InvalidDictionary.unorderedOrEqualKeys(elems.head._1, nk).asLeft }
                  }
            }
         case None      => unexpectedEnd(Context.BDictionary, i, End, Digit).asLeft
      }
   }

   input.headOption match {
      case Some(`d`) => loop(idx + 1, input.tail, List.empty)
      case Some(b)   => unexpected(Context.BDictionary, idx, b, D).asLeft
      case None      => unexpectedEnd(Context.BDictionary, idx, D).asLeft
   }
}

// ------ Failure Details ------:

enum ParseError {
   case Unexpected(ctx: Context, position: Long, found: Found, expected: List[Expected])
   case Invalid2(ctx: Context, position: Long)

   case InvalidString(detail: StringErrDetail)
   case InvalidInteger(detail: IntegerErrDetail)
   case InvalidDictionary(detail: DictErrDetail)
}

object ParseError {
   enum Context {
      case OneOf, BString, BInteger, BList, BDictionary
   }

   enum Found {
      case EOI // End Of Input
      case Token(is: ByteVector)
   }

   enum Expected {
      case I, L, D, Digit, Minus, End, Colon
   }

   enum StringErrDetail {
      case ParsingLen(givenLen: ByteVector)
      case ParsingDataInsuficient(wanted: Int)
   }

   enum IntegerErrDetail {
      case Parsing(found: ByteVector)
      case NegativeZero
   }

   enum DictErrDetail {
      case InvalidKeyEncoding(key: BString)
      case UnorderedOrEqualKeys(prev: BString, next: BString)
   }

   // ----- helpers to build errors easier -----
   def unexpected(ctx: Context, pos: Long, found: Byte, expected: Expected*) =
      Unexpected(ctx, pos, Found.Token(ByteVector(found)), expected.toList)

   def unexpectedEnd(ctx: Context, pos: Long, expected: Expected*) =
      Unexpected(ctx, pos, Found.EOI, expected.toList)

   extension (ii: InvalidInteger.type) {
      def parsing(found: ByteVector) = InvalidInteger(IntegerErrDetail.Parsing(found))
      def negativeZero()             = InvalidInteger(IntegerErrDetail.NegativeZero)
   }

   extension (ii: InvalidDictionary.type) {
      def unorderedOrEqualKeys(prev: BString, next: BString) = InvalidDictionary(
        DictErrDetail.UnorderedOrEqualKeys(prev, next)
      )
   }
}
