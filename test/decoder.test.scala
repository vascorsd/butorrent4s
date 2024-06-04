package butorrent4s

import Bencode.*
import ParseError.{StringErrDetail, *}
import ParseError.ExpectedToken.*
import butorrent4s.ParseError.IntegerErrDetail.NegativeZero
import butorrent4s.ParseError.StringErrDetail.{ParsingDataInsuficient, ParsingLen}
import butorrent4s.ParseError.DictErrDetail.UnorderedOrEqualKeys
import munit.{Compare, Location}
import scodec.bits.ByteVector
import scodec.bits.*

class DecoderTests extends munit.FunSuite {

   def expectBad[A](
       parser: ByteVector => ParseResult[A],
       input: String,
       result: ParseError,
       unparsedInput: String = "" // todo ?
   )(using Location) = {
      val in: ByteVector = ByteVector.view(input.getBytes("UTF-8"))

      parser(in) match {
         case Left(value) => assertEquals(value, result)
         case Right(_)    => fail("expected to test a Left value, got a Right on the parser")
      }
   }

   def expectOk[A, B](
       parser: ByteVector => ParseResult[A],
       input: String,
       result: B,
       unparsedInput: String
   )(using Location, Compare[A, B]) = {
      val in: ByteVector = ByteVector.view(input.getBytes("UTF-8"))

      parser(in) match {
         case Left(value)               => fail("expected to test a Right value, got a Left on the parser")
         case Right((parsed, unparsed)) =>
            assertEquals(parsed, result)
            assertEquals(unparsed, ByteVector.view(unparsedInput.getBytes("UTF-8")))
      }
   }

   test("byteStringP ❌ - invalid inputs") {
      expectBad(
        byteStringP,
        "",
        UnexpectedEOI(ParseContext.BString, List(Digit))
      )

      expectBad(
        byteStringP,
        ":",
        Unexpected(ParseContext.BString, ByteVector(58), List(Digit))
      )

      expectBad(
        byteStringP,
        "s",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "ss",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "s:",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "ss:",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "1",
        UnexpectedEOI(ParseContext.BString, List(Digit, Colon))
      )

      expectBad(
        byteStringP,
        "1s",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit, Colon))
      )

      expectBad(
        byteStringP,
        "1s:",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit, Colon))
      )

      expectBad(
        byteStringP,
        "s1",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "s1:",
        Unexpected(ParseContext.BString, ByteVector(115), List(Digit))
      )

      expectBad(
        byteStringP,
        "01:",
        InvalidString(ParsingDataInsuficient(1))
      )

      // making sure the only valid numbers are ascii decimal digits:
      // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
      // from: https://www.unicode.org/terminology/digits.html
      expectBad(
        byteStringP,
        "١:s",
        Unexpected(ParseContext.BString, ByteVector(-39), List(Digit))
      )
   }

   test("byteStringP ✔ - valid inputs") {
      expectOk(
        byteStringP,
        "1:s",
        bstring("s"),
        ""
      )

      expectOk(
        byteStringP,
        "0:",
        bstring(""),
        ""
      )

      expectOk(
        byteStringP,
        "2:ss",
        bstring("ss"),
        ""
      )

      expectOk(
        byteStringP,
        "10:ssssssssss",
        bstring("ssssssssss"),
        ""
      )

      // prove parser not too eager and allows extra bytes as unparsed
      expectOk(
        byteStringP,
        "0:ss",
        bstring(""),
        "ss"
      )

      expectOk(
        byteStringP,
        "1:ss",
        bstring("s"),
        "s"
      )

      // todo: test limits of Ints. As is currently represented I can't even
      //  create a string of the max size in the test.
      // parseByteString("2147483647:")
   }

   test("integerP ❌ - invalid inputs") {
      expectBad(
        integerP,
        "i010e",
        Unexpected(ParseContext.BInteger, ByteVector(49), List(End))
      )

      expectBad(
        integerP,
        "iss1e",
        Unexpected(ParseContext.BInteger, ByteVector(115), List(Digit, Minus))
      )

      expectBad(
        integerP,
        "i5ie",
        Unexpected(ParseContext.BInteger, ByteVector(105), List(Digit, End))
      )

      expectBad(
        integerP,
        "i0101010101010000000000000",
        Unexpected(ParseContext.BInteger, ByteVector(49), List(End))
      )

      expectBad(
        integerP,
        "i-0e",
        InvalidInteger(NegativeZero)
      )

      expectBad(
        integerP,
        "i-s5e",
        Unexpected(ParseContext.BInteger, ByteVector(115), List(Digit))
      )

      expectBad(
        integerP,
        "i-i1e",
        Unexpected(ParseContext.BInteger, ByteVector(105), List(Digit))
      )

      expectBad(
        integerP,
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e",
        Unexpected(ParseContext.BInteger, ByteVector(48), List(End))
      )

      // making sure the only valid numbers are ascii decimal digits:
      // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
      // from: https://www.unicode.org/terminology/digits.html
      expectBad(
        integerP,
        "i١٢e",
        Unexpected(ParseContext.BInteger, utf8Bytes"١".take(1), List(Digit, Minus))
      )
   }

   test("integerP ✔ - valid inputs") {
      expectOk(
        integerP,
        "i1e",
        binteger(1),
        ""
      )

      expectOk(
        integerP,
        "i10e",
        binteger(10),
        ""
      )

      expectOk(
        integerP,
        "i9999999999e",
        binteger(9999999999L),
        ""
      )

      expectOk(
        integerP,
        "i0e",
        binteger(0),
        ""
      )

      expectOk(
        integerP,
        "i-50e",
        binteger(-50L),
        ""
      )

      expectOk(
        integerP,
        "i-9999999999e:",
        binteger(-9999999999L),
        ":"
      )
   }

   test("listP ❌ - invalid inputs") {
      expectBad(
        listP,
        "",
        UnexpectedEOI(ParseContext.BList, List(L))
      )

      expectBad(
        listP,
        "e",
        Unexpected(ParseContext.BList, ByteVector(101), List(L))
      )

      expectBad(
        listP,
        "l",
        UnexpectedEOI(ParseContext.OneOf, List(I, L, D, Digit))
      )

      expectBad(
        listP,
        "l:e",
        Unexpected(ParseContext.OneOf, ByteVector(58), List(I, L, D, Digit))
      )

      expectBad(
        listP,
        "l1e",
        Unexpected(ParseContext.BString, ByteVector(101), List(Digit, Colon))
      )

      expectBad(
        listP,
        "l1:e",
        UnexpectedEOI(ParseContext.OneOf, List(I, L, D, Digit))
      )

      expectBad(
        listP,
        "lie",
        Unexpected(ParseContext.BInteger, ByteVector(101), List(Digit, Minus))
      )

      expectBad(
        listP,
        "li-ee",
        Unexpected(ParseContext.BInteger, ByteVector(101), List(Digit))
      )

      expectBad(
        listP,
        "li10e5e",
        Unexpected(ParseContext.BString, ByteVector(101), List(Digit, Colon))
      )

      expectBad(
        listP,
        "lle",
        UnexpectedEOI(ParseContext.OneOf, List(I, L, D, Digit))
      )

      expectBad(
        listP,
        "ll5e",
        Unexpected(ParseContext.BString, ByteVector(101), List(Digit, Colon))
      )
   }

   test("listP ✔ - valid inputs") {
      expectOk(
        listP,
        "le",
        blist(),
        ""
      )

      expectOk(
        listP,
        "lee",
        blist(),
        "e"
      )

      expectOk(
        listP,
        "l0:e",
        blist(
          bstring("")
        ),
        ""
      )

      expectOk(
        listP,
        "l0:2:ssi1elee",
        blist(
          bstring(""),
          bstring("ss"),
          binteger(1L),
          blist()
        ),
        ""
      )
   }

   test("dictionaryP ❌ - invalid inputs") {
      expectBad(
        dictionaryP,
        "d",
        UnexpectedEOI(ParseContext.BString, List(Digit))
      )

      expectBad(
        dictionaryP,
        "e",
        Unexpected(ParseContext.BDictionary, ByteVector(101), List(D))
      )

      expectBad(
        dictionaryP,
        "die",
        Unexpected(ParseContext.BString, ByteVector(105), List(Digit))
      )

      expectBad(
        dictionaryP,
        "did",
        Unexpected(ParseContext.BString, ByteVector(105), List(Digit))
      )

      expectBad(
        dictionaryP,
        "dld",
        Unexpected(ParseContext.BString, ByteVector(108), List(Digit))
      )

      expectBad(
        dictionaryP,
        "dl5d",
        Unexpected(ParseContext.BString, ByteVector(108), List(Digit))
      )

      expectBad(
        dictionaryP,
        "dlelel",
        Unexpected(ParseContext.BString, ByteVector(108), List(Digit))
      )

      expectBad(
        dictionaryP,
        "d5",
        UnexpectedEOI(ParseContext.BString, List(Digit, Colon))
      )

      expectBad(
        dictionaryP,
        "d5d",
        Unexpected(ParseContext.BString, ByteVector(100), List(Digit, Colon))
      )

      expectBad(
        dictionaryP,
        "d1:e",
        UnexpectedEOI(ParseContext.OneOf, List(I, L, D, Digit))
      )

      expectBad(
        dictionaryP,
        "d1:s",
        UnexpectedEOI(ParseContext.OneOf, List(I, L, D, Digit))
      )

      expectBad(
        dictionaryP,
        "d1:sle",
        UnexpectedEOI(ParseContext.BString, List(Digit))
      )

      // todo: fix
      expectBad(
        dictionaryP,
        "d1:sli0",
        UnexpectedEOI(ParseContext.BInteger, List(End))
      )

      // almost valid encoding, problem b2 key comes before b1, wich is unordered
      expectBad(
        dictionaryP,
        "d1:a1:a2:b2i2e2:b1i1e1:c1:ce",
        InvalidDictionary(UnorderedOrEqualKeys(bstring("b2"), bstring("b1")))
      )

      // duplicated key problem
      expectBad(
        dictionaryP,
        "d0:0:0:0:e",
        InvalidDictionary(UnorderedOrEqualKeys(bstring(""), bstring("")))
      )

      // duplicated key problem not immediately after
      expectBad(
        dictionaryP,
        "d0:0:1:s0:0:1:se",
        InvalidDictionary(UnorderedOrEqualKeys(bstring("s"), bstring("")))
      )
   }

   test("dictionaryP ✔ - valid inputs") {
      expectOk(
        dictionaryP,
        "d0:lee",
        bdictionary(
          bstring("") -> blist()
        ),
        ""
      )

      expectOk(
        dictionaryP,
        "d0:i0eefuuu",
        bdictionary(
          bstring("") -> binteger(0)
        ),
        "fuuu"
      )

      expectOk(
        dictionaryP,
        "d1:a3:hey2:b1i0e2:b2le1:cl3:mome1:dd2:fu3:baree...",
        bdictionary(
          bstring("a")  -> bstring("hey"),
          bstring("b1") -> binteger(0),
          bstring("b2") -> blist(),
          bstring("c")  -> blist(bstring("mom")),
          bstring("d")  -> bdictionary(
            bstring("fu") -> bstring("bar")
          )
        ),
        "..."
      )

      // from spec examples:
      expectOk(
        dictionaryP,
        "d3:cow3:moo4:spam4:eggse",
        bdictionary(
          bstring("cow")  -> bstring("moo"),
          bstring("spam") -> bstring("eggs")
        ),
        ""
      )

      expectOk(
        dictionaryP,
        "d4:spaml1:a1:bee",
        bdictionary(
          bstring("spam") -> blist(bstring("a"), bstring("b"))
        ),
        ""
      )

      expectOk(
        dictionaryP,
        "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee",
        bdictionary(
          bstring("publisher")          -> bstring("bob"),
          bstring("publisher-webpage")  -> bstring("www.example.com"),
          bstring("publisher.location") -> bstring("home")
        ),
        ""
      )

      expectOk(
        dictionaryP,
        "de",
        bdictionary(),
        ""
      )
   }
}
