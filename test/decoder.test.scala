package butorrent4s

import Bencode.*

def resultOk[A](v: A, r: String): ParseResult[A] = Some((v, r.toList))
def resultBad: ParseResult[Bencode] = None

class DecoderTests extends munit.FunSuite {

  test("parserByteString ❌ - invalid inputs") {
    assertEquals(parserByteString("".toList), resultBad)
    assertEquals(parserByteString(":".toList), resultBad)
    assertEquals(parserByteString("s".toList), resultBad)
    assertEquals(parserByteString("ss".toList), resultBad)
    assertEquals(parserByteString("s:".toList), resultBad)
    assertEquals(parserByteString("ss:".toList), resultBad)
    assertEquals(parserByteString("1".toList), resultBad)
    assertEquals(parserByteString("1s".toList), resultBad)
    assertEquals(parserByteString("1s:".toList), resultBad)
    assertEquals(parserByteString("s1".toList), resultBad)
    assertEquals(parserByteString("s1:".toList), resultBad)
    assertEquals(parserByteString("01:".toList), resultBad)

    // making sure the only valid numbers are ascii decimal digits:
    // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
    // from: https://www.unicode.org/terminology/digits.html
    assertEquals(
      parserByteString(
        "١:s".toList
      ),
      resultBad
    )
  }

  test("parserByteString ✔ - valid inputs") {
    assertEquals(
      parserByteString("1:s".toList),
      resultOk(BString("s"), "")
    )

    assertEquals(
      parserByteString("0:".toList),
      resultOk(BString(""), "")
    )

    assertEquals(
      parserByteString("2:ss".toList),
      resultOk(BString("ss"), "")
    )

    assertEquals(
      parserByteString("10:ssssssssss".toList),
      resultOk(BString("ssssssssss"), "")
    )

    // these 2 tests now pass since the parse string function
    // as become less eager and only checks that there's at least
    // the info requested to parse. Doesn't care if there's trash afterwards.
    assertEquals(
      parserByteString("0:ss".toList),
      resultOk(BString(""), "ss")
    )

    assertEquals(
      parserByteString("1:ss".toList),
      resultOk(BString("s"), "s")
    )

    // todo: test limits of Ints. As is currently represented I can't even
    //  create a string of the max size in the test.
    // parseByteString("2147483647:")
  }

  test("parserInteger ❌ - invalid inputs") {
    assertEquals(parserInteger("i010e".toList), resultBad)
    assertEquals(parserInteger("iss1e".toList), resultBad)
    assertEquals(parserInteger("i5ie".toList), resultBad)
    assertEquals(parserInteger("i0101010101010000000000000".toList), resultBad)

    assertEquals(parserInteger("i-0e".toList), resultBad)

    assertEquals(parserInteger("i-s5e".toList), resultBad)
    assertEquals(parserInteger("i-i1e".toList), resultBad)

    assertEquals(
      parserInteger(
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e".toList
      ),
      resultBad
    )

    // making sure the only valid numbers are ascii decimal digits:
    // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
    // from: https://www.unicode.org/terminology/digits.html
    assertEquals(
      parserInteger(
        "i١٢e".toList
      ),
      resultBad
    )
  }

  test("parserInteger ✔ - valid inputs") {
    assertEquals(
      parserInteger("i1e".toList),
      resultOk(BInteger(1L), "")
    )

    assertEquals(
      parserInteger("i10e".toList),
      resultOk(BInteger(10L), "")
    )

    assertEquals(
      parserInteger("i9999999999e:".toList),
      resultOk(BInteger(9999999999L), ":")
    )

    assertEquals(
      parserInteger("i0e".toList),
      resultOk(BInteger(0L), "")
    )

    assertEquals(
      parserInteger("i-50e".toList),
      resultOk(BInteger(-50L), "")
    )

    assertEquals(
      parserInteger("i-9999999999e:".toList),
      resultOk(BInteger(-9999999999L), ":")
    )
  }

  test("parserList ❌ - invalid inputs") {
    assertEquals(parserList("".toList), resultBad)
    assertEquals(parserList("e".toList), resultBad)
    assertEquals(parserList("l".toList), resultBad)

    assertEquals(parserList("l:e".toList), resultBad)
    assertEquals(parserList("l1e".toList), resultBad)
    assertEquals(parserList("l1:e".toList), resultBad)

    assertEquals(parserList("lie".toList), resultBad)
    assertEquals(parserList("li-ee".toList), resultBad)
    assertEquals(parserList("li10e5e".toList), resultBad)

    assertEquals(parserList("lle".toList), resultBad)
    assertEquals(parserList("ll5e".toList), resultBad)
  }

  test("parserList ✔ - valid inputs") {
    assertEquals(
      parserList("le".toList),
      resultOk(BList(List.empty), "")
    )

    assertEquals(
      parserList("lee".toList),
      resultOk(BList(List.empty), "e")
    )

    assertEquals(
      parserList("l0:e".toList),
      resultOk(
        BList(
          BString("") ::
            Nil
        ),
        ""
      )
    )

    assertEquals(
      parserList("l0:2:ssi1elee".toList),
      resultOk(
        BList(
          BString("") ::
            BString("ss") ::
            BInteger(1L) ::
            BList(Nil) ::
            Nil
        ),
        ""
      )
    )
  }

  test("parserDict ❌ - invalid inputs") {
    assertEquals(parserDictionary("d".toList), resultBad)
    assertEquals(parserDictionary("e".toList), resultBad)

    assertEquals(parserDictionary("die".toList), resultBad)
    assertEquals(parserDictionary("did".toList), resultBad)

    assertEquals(parserDictionary("dld".toList), resultBad)
    assertEquals(parserDictionary("dl5d".toList), resultBad)
    assertEquals(parserDictionary("dlelel".toList), resultBad)

    assertEquals(parserDictionary("d5".toList), resultBad)
    assertEquals(parserDictionary("d5d".toList), resultBad)
    assertEquals(parserDictionary("d1:e".toList), resultBad)
    assertEquals(parserDictionary("d1:s".toList), resultBad)
    assertEquals(parserDictionary("d1:sle".toList), resultBad)
    assertEquals(parserDictionary("d1:sli0".toList), resultBad)

    // almost valid encoding, problem b2 key comes before b1, wich is unordered
    assertEquals(
      parserDictionary(
        "d1:a1:a2:b2i2e2:b1i1e1:c1:ce".toList
      ),
      resultBad,
      ""
    )

    // duplicated key problem
    assertEquals(
      parserDictionary(
        "d0:0:0:0:e".toList
      ),
      resultBad,
      ""
    )

    // duplicated key problem not immediately after
    assertEquals(
      parserDictionary(
        "d0:0:1:s0:0:1:se".toList
      ),
      resultBad,
      ""
    )
  }

  test("parserDict ✔ - valid inputs") {
    assertEquals(
      parserDictionary("d0:lee".toList),
      resultOk(
        BDictionary(
          List(
            BString("") -> BList(Nil)
          )
        ),
        ""
      )
    )

    assertEquals(
      parserDictionary("d0:i0eefuuu".toList),
      resultOk(
        BDictionary(
          List(
            BString("") -> BInteger(0)
          )
        ),
        "fuuu"
      )
    )

    assertEquals(
      parserDictionary(
        "d1:a3:hey2:b1i0e2:b2le1:cl3:mome1:dd2:fu3:baree...".toList
      ),
      resultOk(
        BDictionary(
          BString("a") -> BString("hey") ::
            BString("b1") -> BInteger(0) ::
            BString("b2") -> BList(Nil) ::
            BString("c") -> BList(BString("mom") :: Nil) ::
            BString("d") -> BDictionary(
              BString("fu") -> BString("bar") :: Nil
            ) ::
            Nil
        ),
        "..."
      )
    )

    // from spec examples:
    assertEquals(
      parserDictionary("d3:cow3:moo4:spam4:eggse".toList),
      resultOk(
        BDictionary(
          BString("cow") -> BString("moo") ::
            BString("spam") -> BString("eggs") ::
            Nil
        ),
        ""
      )
    )

    assertEquals(
      parserDictionary("d4:spaml1:a1:bee".toList),
      resultOk(
        BDictionary(
          BString("spam") -> BList(BString("a") :: BString("b") :: Nil) ::
            Nil
        ),
        ""
      )
    )

    assertEquals(
      parserDictionary(
        "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee".toList
      ),
      resultOk(
        BDictionary(
          BString("publisher") -> BString("bob") ::
            BString("publisher-webpage") -> BString("www.example.com") ::
            BString("publisher.location") -> BString("home") ::
            Nil
        ),
        ""
      )
    )

    assertEquals(
      parserDictionary("de".toList),
      resultOk(BDictionary.empty, "")
    )
  }
}
