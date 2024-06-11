package butorrent4s

import scodec.bits.*
import scodec.bits.ByteVector

import munit.{Compare, Location}

import Bencode.*
import ParseError.*
import ParseError.Expected.*

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
       pos: Long
   )(using Location, Compare[A, B]) = {
      val in: ByteVector = ByteVector.view(input.getBytes("UTF-8"))

      parser(in)(0) match {
         case Left(value)                         => fail("expected to test a Right value, got a Left on the parser")
         case Right((position, unparsed, parsed)) =>
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

      // todo: leading zeros on bytestring valid?
      expectBad(
        byteStringP.curried,
        "01:",
        Parsing(
          Context.BString,
          2,
          ParseError.Detail.MsgFor("Unable to parse enough data. Not found the wanted '1 bytes'", utf8Bytes"")
        )
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
      assertEquals(
        err,
        Parsing(
          Context.BString,
          10,
          ParseError.Detail.MsgFor("Unable to parse 'length' from given bytes", utf8Bytes"2147483648")
        )
      )
   }

   test("byteStringP ✔ - valid inputs") {
      expectOk(
        byteStringP.curried,
        "1:s",
        bstringWithVec("s"),
        "",
        3
      )

      expectOk(
        byteStringP.curried,
        "0:",
        bstringWithVec(""),
        "",
        2
      )

      expectOk(
        byteStringP.curried,
        "2:ss",
        bstringWithVec("ss"),
        "",
        4
      )

      expectOk(
        byteStringP.curried,
        "10:ssssssssss",
        bstringWithVec("ssssssssss"),
        "",
        13
      )

      // prove parser not too eager and allows extra bytes as unparsed
      expectOk(
        byteStringP.curried,
        "0:ss",
        bstringWithVec(""),
        "ss",
        2
      )

      expectOk(
        byteStringP.curried,
        "1:ss",
        bstringWithVec("s"),
        "s",
        3
      )

      // testing limits: max limit Int allowed.
      // biggest problem found was atually the .toString being called from munit and blowing up there.
      val Right((nextParserPos, remainingB, parsedS)) =
         byteStringP(
           utf8Bytes"2147483647:" ++ ByteVector.fill(Int.MaxValue)(20)
         ): @unchecked
      assertEquals(parsedS, bstringWithVec(ByteVector.fill(Int.MaxValue)(20)))
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
        Parsing(
          Context.BInteger,
          2,
          ParseError.Detail.MsgFor("Unable to parse 'negative zero'", utf8Bytes"i-0")
        )
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
          bstringWithVec("")
        ),
        "",
        4
      )

      expectOk(
        listP.curried,
        "l0:2:ssi1elee",
        blist(
          bstringWithVec(""),
          bstringWithVec("ss"),
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
        Parsing(
          Context.BDictionary,
          17,
          ParseError.Detail.Msg(
            "Unable to use given key. Keys need to be ordered. 'b2' < 'b1' is false."
          )
        )
      )

      // duplicated key problem
      expectBad(
        dictionaryP.curried,
        "d0:0:0:0:e",
        Parsing(
          Context.BDictionary,
          6,
          ParseError.Detail.Msg(
            "Unable to use given key. Keys need to be ordered. '' < '' is false."
          )
        )
      )

      // duplicated key problem not immediately after
      expectBad(
        dictionaryP.curried,
        "d0:0:1:s0:0:1:se",
        Parsing(
          Context.BDictionary,
          11,
          ParseError.Detail.Msg(
            "Unable to use given key. Keys need to be ordered. 's' < '' is false."
          )
        )
      )
   }

   test("dictionaryP ✔ - valid inputs") {
      expectOk(
        dictionaryP.curried,
        "d0:lee",
        bdictionary(
          bstringWithStr("") -> blist()
        ),
        "",
        6
      )

      expectOk(
        dictionaryP.curried,
        "d0:i0eefuuu",
        bdictionary(
          bstringWithStr("") -> binteger(0)
        ),
        "fuuu",
        7
      )

      expectOk(
        dictionaryP.curried,
        "d1:a3:hey2:b1i0e2:b2le1:cl3:mome1:dd2:fu3:baree...",
        bdictionary(
          bstringWithStr("a")  -> bstringWithVec("hey"),
          bstringWithStr("b1") -> binteger(0),
          bstringWithStr("b2") -> blist(),
          bstringWithStr("c")  -> blist(bstringWithVec("mom")),
          bstringWithStr("d")  -> bdictionary(
            bstringWithStr("fu") -> bstringWithVec("bar")
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
          bstringWithStr("cow")  -> bstringWithVec("moo"),
          bstringWithStr("spam") -> bstringWithVec("eggs")
        ),
        "",
        24
      )

      expectOk(
        dictionaryP.curried,
        "d4:spaml1:a1:bee",
        bdictionary(
          bstringWithStr("spam") -> blist(bstringWithVec("a"), bstringWithVec("b"))
        ),
        "",
        16
      )

      expectOk(
        dictionaryP.curried,
        "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee",
        bdictionary(
          bstringWithStr("publisher")          -> bstringWithVec("bob"),
          bstringWithStr("publisher-webpage")  -> bstringWithVec("www.example.com"),
          bstringWithStr("publisher.location") -> bstringWithVec("home")
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
