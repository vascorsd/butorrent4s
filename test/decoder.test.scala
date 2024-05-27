package butorrent4s

import BencodeData.*

def resultOk[A](v: A, r: String): ParseResult[A] = Some((v, r.toList))
def resultBad: ParseResult[BencodeData] = None

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
  }

  test("parserByteString ✔ - valid inputs") {
    assertEquals(
      parserByteString("1:s".toList),
      resultOk(BenString("s"), "")
    )

    assertEquals(
      parserByteString("0:".toList),
      resultOk(BenString(""), "")
    )

    assertEquals(
      parserByteString("2:ss".toList),
      resultOk(BenString("ss"), "")
    )

    assertEquals(
      parserByteString("10:ssssssssss".toList),
      resultOk(BenString("ssssssssss"), "")
    )

    // these 2 tests now pass since the parse string function
    // as become less eager and only checks that there's at least
    // the info requested to parse. Doesn't care if there's trash afterwards.
    assertEquals(
      parserByteString("0:ss".toList),
      resultOk(BenString(""), "ss")
    )

    assertEquals(
      parserByteString("1:ss".toList),
      resultOk(BenString("s"), "s")
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
  }

  test("parserInteger ✔ - valid inputs") {
    assertEquals(
      parserInteger("i1e".toList),
      resultOk(BenInteger(1L), "")
    )

    assertEquals(
      parserInteger("i10e".toList),
      resultOk(BenInteger(10L), "")
    )

    assertEquals(
      parserInteger("i9999999999e:".toList),
      resultOk(BenInteger(9999999999L), ":")
    )

    assertEquals(
      parserInteger("i0e".toList),
      resultOk(BenInteger(0L), "")
    )

    assertEquals(
      parserInteger("i-50e".toList),
      resultOk(BenInteger(-50L), "")
    )

    assertEquals(
      parserInteger("i-9999999999e:".toList),
      resultOk(BenInteger(-9999999999L), ":")
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
      resultOk(BenList(List.empty), "")
    )

    assertEquals(
      parserList("lee".toList),
      resultOk(BenList(List.empty), "e")
    )

    assertEquals(
      parserList("l0:e".toList),
      resultOk(
        BenList(
          BenString("") ::
            Nil
        ),
        ""
      )
    )

    assertEquals(
      parserList("l0:2:ssi1elee".toList),
      resultOk(
        BenList(
          BenString("") ::
            BenString("ss") ::
            BenInteger(1L) ::
            BenList(Nil) ::
            Nil
        ),
        ""
      )
    )
  }
}
