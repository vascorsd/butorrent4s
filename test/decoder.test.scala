package butorrent4s

import Bencode.*

extension (s: String) {
  def in: Array[Byte] = s.getBytes("UTF-8")
}

def resultOk[A](v: A, r: String): ParseResult[A] = Some((v, r.toList))
def resultOk2[A](v: A, r: Array[Byte]): ParseResult2[A] = Some((v, r))

def resultBad: ParseResult[Bencode] = None
def resultBad2: ParseResult2[Bencode] = None

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
    assertEquals(parserInteger2("i010e".in), resultBad2)
    assertEquals(parserInteger2("iss1e".in), resultBad2)
    assertEquals(parserInteger2("i5ie".in), resultBad2)
    assertEquals(
      parserInteger2("i0101010101010000000000000".in),
      resultBad2
    )

    assertEquals(parserInteger2("i-0e".in), resultBad2)

    assertEquals(parserInteger2("i-s5e".in), resultBad2)
    assertEquals(parserInteger2("i-i1e".in), resultBad2)

    assertEquals(
      parserInteger2(
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e".in
      ),
      resultBad2
    )

    // making sure the only valid numbers are ascii decimal digits:
    // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
    // from: https://www.unicode.org/terminology/digits.html
    assertEquals(
      parserInteger2(
        "i١٢e".in
      ),
      resultBad2
    )
  }

  test("parserInteger ✔ - valid inputs") {
    val Some((int1, r1)) = parserInteger2("i1e".in): @unchecked
    assertEquals(int1, BInteger(1L))
    assertEquals(String(r1), "")

    val Some((int2, r2)) = parserInteger2("i10e".in): @unchecked
    assertEquals(int2, BInteger(10L))
    assertEquals(String(r1), "")

    val Some((int3, r3)) = parserInteger2("i9999999999e:".in): @unchecked
    assertEquals(int3, BInteger(9999999999L))
    assertEquals(String(r3), ":")

    val Some((int4, r4)) = parserInteger2("i0e".in): @unchecked
    assertEquals(int4, BInteger(0L))
    assertEquals(String(r4), "")

    val Some((int5, r5)) = parserInteger2("i-50e".in): @unchecked
    assertEquals(int5, BInteger(-50L))
    assertEquals(String(r5), "")

    val Some((int6, r6)) = parserInteger2("i-9999999999e:".in): @unchecked
    assertEquals(int6, BInteger(-9999999999L))
    assertEquals(String(r6), ":")
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
