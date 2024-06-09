package butorrent4s

import scodec.bits.*
import scodec.bits.ByteVector

import munit.{Compare, Location}

import Bencode.*
import ParseError.*
import ParseError.Expected.*
import butorrent4s.ParseError.DictErrDetail.UnorderedOrEqualKeys
import butorrent4s.ParseError.IntegerErrDetail.NegativeZero
import butorrent4s.ParseError.StringErrDetail.{ParsingDataInsuficient, ParsingLen}

class DecoderTests extends munit.FunSuite {

   def expectBad[A](
       parser: ByteVector => Long => ParseResult[A],
       input: String,
       result: ParseError
   )(using Location) = {
      val in: ByteVector = ByteVector.view(input.getBytes("UTF-8"))

      parser(in)(0) match {
         case Left(value) => assertEquals(value, result)
         case Right(_)    => fail("expected to test a Left value, got a Right on the parser")
      }
   }

   def expectOk[A, B](
       parser: ByteVector => Long => ParseResult[A],
       input: String,
       result: B,
       unparsedInput: String,
       pos: Long = 0
   )(using Location, Compare[A, B]) = {
      val in: ByteVector = ByteVector.view(input.getBytes("UTF-8"))

      parser(in)(0) match {
         case Left(value)                         => fail("expected to test a Right value, got a Left on the parser")
         case Right((parsed, unparsed, position)) =>
            assertEquals(parsed, result)
            assertEquals(unparsed, ByteVector.view(unparsedInput.getBytes("UTF-8")))
            assertEquals(position, pos)
      }
   }

   test("byteStringP ❌ - invalid inputs") {
      expectBad(
        byteStringP.curried,
        "",
        Unexpected(Context.BString, 0, Found.EOI, List(Digit))
      )

      expectBad(
        byteStringP.curried,
        ":",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes":"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "s",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "ss",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "s:",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "ss:",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "1",
        Unexpected(Context.BString, 1, Found.EOI, List(Digit, Colon))
      )

      expectBad(
        byteStringP.curried,
        "1s",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"s"), List(Digit, Colon))
      )

      expectBad(
        byteStringP.curried,
        "1s:",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"s"), List(Digit, Colon))
      )

      expectBad(
        byteStringP.curried,
        "s1",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "s1:",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        byteStringP.curried,
        "01:",
        InvalidString(ParsingDataInsuficient(1))
      )

      // making sure the only valid numbers are ascii decimal digits:
      // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
      // from: https://www.unicode.org/terminology/digits.html
      expectBad(
        byteStringP.curried,
        "١:s",
        Unexpected(Context.BString, 0, Found.Token(utf8Bytes"١".take(1)), List(Digit))
      )

      // testing limits: max limit Int allowed.
      val Left(err) =
         byteStringP(
           utf8Bytes"2147483648:" ++ ByteVector.fill(Int.MaxValue)(20)
         ): @unchecked
      assertEquals(err, InvalidString(ParsingLen(utf8Bytes"2147483648")))
   }

   test("byteStringP ✔ - valid inputs") {
      expectOk(
        byteStringP.curried,
        "1:s",
        bstring("s"),
        "",
        3
      )

      expectOk(
        byteStringP.curried,
        "0:",
        bstring(""),
        "",
        2
      )

      expectOk(
        byteStringP.curried,
        "2:ss",
        bstring("ss"),
        "",
        4
      )

      expectOk(
        byteStringP.curried,
        "10:ssssssssss",
        bstring("ssssssssss"),
        "",
        13
      )

      // prove parser not too eager and allows extra bytes as unparsed
      expectOk(
        byteStringP.curried,
        "0:ss",
        bstring(""),
        "ss",
        2
      )

      expectOk(
        byteStringP.curried,
        "1:ss",
        bstring("s"),
        "s",
        3
      )

      // testing limits: max limit Int allowed.
      // biggest problem found was atually the .toString being called from munit and blowing up there.
      val Right((parsedS, remainingB, nextParserPos)) =
         byteStringP(
           utf8Bytes"2147483647:" ++ ByteVector.fill(Int.MaxValue)(20)
         ): @unchecked
      assertEquals(parsedS, bstring(ByteVector.fill(Int.MaxValue)(20)))
      assertEquals(remainingB, utf8Bytes"")
      assertEquals(nextParserPos, 2147483658L)
   }

   test("integerP ❌ - invalid inputs") {
      expectBad(
        integerP.curried,
        "i010e",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"1"), List(End))
      )

      expectBad(
        integerP.curried,
        "iss1e",
        Unexpected(Context.BInteger, 1, Found.Token(utf8Bytes"s"), List(Digit, Minus))
      )

      expectBad(
        integerP.curried,
        "i5ie",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"i"), List(Digit, End))
      )

      expectBad(
        integerP.curried,
        "i0101010101010000000000000",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"1"), List(End))
      )

      expectBad(
        integerP.curried,
        "i-0e",
        InvalidInteger(NegativeZero)
      )

      expectBad(
        integerP.curried,
        "i-s5e",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"s"), List(Digit))
      )

      expectBad(
        integerP.curried,
        "i-i1e",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"i"), List(Digit))
      )

      expectBad(
        integerP.curried,
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"0"), List(End))
      )

      // making sure the only valid numbers are ascii decimal digits:
      // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
      // from: https://www.unicode.org/terminology/digits.html
      expectBad(
        integerP.curried,
        "i١٢e",
        Unexpected(Context.BInteger, 1, Found.Token(utf8Bytes"١".take(1)), List(Digit, Minus))
      )
   }

   test("integerP ✔ - valid inputs") {
      expectOk(
        integerP.curried,
        "i1e",
        binteger(1),
        "",
        3
      )

      expectOk(
        integerP.curried,
        "i10e",
        binteger(10),
        "",
        4
      )

      expectOk(
        integerP.curried,
        "i9999999999e",
        binteger(9999999999L),
        "",
        12
      )

      expectOk(
        integerP.curried,
        "i0e",
        binteger(0),
        "",
        3
      )

      expectOk(
        integerP.curried,
        "i-50e",
        binteger(-50L),
        "",
        5
      )

      expectOk(
        integerP.curried,
        "i-9999999999e:",
        binteger(-9999999999L),
        ":",
        13
      )
   }

   test("listP ❌ - invalid inputs") {
      expectBad(
        listP.curried,
        "",
        Unexpected(Context.BList, 0, Found.EOI, List(L))
      )

      expectBad(
        listP.curried,
        "e",
        Unexpected(Context.BList, 0, Found.Token(utf8Bytes"e"), List(L))
      )

      expectBad(
        listP.curried,
        "l",
        Unexpected(Context.BList, 1, Found.EOI, List(End, I, L, D, Digit))
      )

      expectBad(
        listP.curried,
        "l:e",
        Unexpected(Context.OneOf, 1, Found.Token(utf8Bytes":"), List(I, L, D, Digit))
      )

      expectBad(
        listP.curried,
        "l1e",
        Unexpected(Context.BString, 2, Found.Token(utf8Bytes"e"), List(Digit, Colon))
      )

      expectBad(
        listP.curried,
        "l1:e",
        Unexpected(Context.BList, 4, Found.EOI, List(End, I, L, D, Digit))
      )

      expectBad(
        listP.curried,
        "lie",
        Unexpected(Context.BInteger, 2, Found.Token(utf8Bytes"e"), List(Digit, Minus))
      )

      expectBad(
        listP.curried,
        "li-ee",
        Unexpected(Context.BInteger, 3, Found.Token(utf8Bytes"e"), List(Digit))
      )

      expectBad(
        listP.curried,
        "li10e5e",
        Unexpected(Context.BString, 6, Found.Token(utf8Bytes"e"), List(Digit, Colon))
      )

      expectBad(
        listP.curried,
        "lle",
        Unexpected(Context.BList, 3, Found.EOI, List(End, I, L, D, Digit))
      )

      expectBad(
        listP.curried,
        "ll5e",
        Unexpected(Context.BString, 3, Found.Token(utf8Bytes"e"), List(Digit, Colon))
      )
   }

   test("listP ✔ - valid inputs") {
      expectOk(
        listP.curried,
        "le",
        blist(),
        "",
        2
      )

      expectOk(
        listP.curried,
        "lee",
        blist(),
        "e",
        2
      )

      expectOk(
        listP.curried,
        "l0:e",
        blist(
          bstring("")
        ),
        "",
        4
      )

      expectOk(
        listP.curried,
        "l0:2:ssi1elee",
        blist(
          bstring(""),
          bstring("ss"),
          binteger(1L),
          blist()
        ),
        "",
        13
      )
   }

   test("dictionaryP ❌ - invalid inputs") {
      expectBad(
        dictionaryP.curried,
        "d",
        Unexpected(Context.BDictionary, 1, Found.EOI, List(End, Digit))
      )

      expectBad(
        dictionaryP.curried,
        "e",
        Unexpected(Context.BDictionary, 0, Found.Token(utf8Bytes"e"), List(D))
      )

      expectBad(
        dictionaryP.curried,
        "die",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"i"), List(Digit))
      )

      expectBad(
        dictionaryP.curried,
        "did",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"i"), List(Digit))
      )

      expectBad(
        dictionaryP.curried,
        "dld",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"l"), List(Digit))
      )

      expectBad(
        dictionaryP.curried,
        "dl5d",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"l"), List(Digit))
      )

      expectBad(
        dictionaryP.curried,
        "dlelel",
        Unexpected(Context.BString, 1, Found.Token(utf8Bytes"l"), List(Digit))
      )

      expectBad(
        dictionaryP.curried,
        "d5",
        Unexpected(Context.BString, 2, Found.EOI, List(Digit, Colon))
      )

      expectBad(
        dictionaryP.curried,
        "d5d",
        Unexpected(Context.BString, 2, Found.Token(utf8Bytes"d"), List(Digit, Colon))
      )

      expectBad(
        dictionaryP.curried,
        "d1:e",
        Unexpected(Context.OneOf, 4, Found.EOI, List(I, L, D, Digit))
      )

      expectBad(
        dictionaryP.curried,
        "d1:s",
        Unexpected(Context.OneOf, 4, Found.EOI, List(I, L, D, Digit))
      )

      expectBad(
        dictionaryP.curried,
        "d1:sle",
        Unexpected(Context.BDictionary, 6, Found.EOI, List(End, Digit))
      )

      expectBad(
        dictionaryP.curried,
        "d1:sli0",
        Unexpected(Context.BInteger, 7, Found.EOI, List(End))
      )

      // almost valid encoding, problem b2 key comes before b1, wich is unordered
      expectBad(
        dictionaryP.curried,
        "d1:a1:a2:b2i2e2:b1i1e1:c1:ce",
        InvalidDictionary(UnorderedOrEqualKeys(bstring("b2"), bstring("b1")))
      )

      // duplicated key problem
      expectBad(
        dictionaryP.curried,
        "d0:0:0:0:e",
        InvalidDictionary(UnorderedOrEqualKeys(bstring(""), bstring("")))
      )

      // duplicated key problem not immediately after
      expectBad(
        dictionaryP.curried,
        "d0:0:1:s0:0:1:se",
        InvalidDictionary(UnorderedOrEqualKeys(bstring("s"), bstring("")))
      )
   }

   test("dictionaryP ✔ - valid inputs") {
      expectOk(
        dictionaryP.curried,
        "d0:lee",
        bdictionary(
          bstring("") -> blist()
        ),
        "",
        6
      )

      expectOk(
        dictionaryP.curried,
        "d0:i0eefuuu",
        bdictionary(
          bstring("") -> binteger(0)
        ),
        "fuuu",
        7
      )

      expectOk(
        dictionaryP.curried,
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
        "...",
        47
      )

      // from spec examples:
      expectOk(
        dictionaryP.curried,
        "d3:cow3:moo4:spam4:eggse",
        bdictionary(
          bstring("cow")  -> bstring("moo"),
          bstring("spam") -> bstring("eggs")
        ),
        "",
        24
      )

      expectOk(
        dictionaryP.curried,
        "d4:spaml1:a1:bee",
        bdictionary(
          bstring("spam") -> blist(bstring("a"), bstring("b"))
        ),
        "",
        16
      )

      expectOk(
        dictionaryP.curried,
        "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee",
        bdictionary(
          bstring("publisher")          -> bstring("bob"),
          bstring("publisher-webpage")  -> bstring("www.example.com"),
          bstring("publisher.location") -> bstring("home")
        ),
        "",
        83
      )

      expectOk(
        dictionaryP.curried,
        "de",
        bdictionary(),
        "",
        2
      )
   }
}
