package butorrent4s

class DecoderTests extends munit.FunSuite {
  test("parseByteString ❌") {
    assertEquals(parseByteString("".toList), None)
    assertEquals(parseByteString(":".toList), None)
    assertEquals(parseByteString("s".toList), None)
    assertEquals(parseByteString("ss".toList), None)
    assertEquals(parseByteString("s:".toList), None)
    assertEquals(parseByteString("ss:".toList), None)
    assertEquals(parseByteString("1".toList), None)
    assertEquals(parseByteString("1s".toList), None)
    assertEquals(parseByteString("1s:".toList), None)
    assertEquals(parseByteString("s1".toList), None)
    assertEquals(parseByteString("s1:".toList), None)
    assertEquals(parseByteString("01:".toList), None)
  }

  test("parseByteString ✔") {
    assertEquals(
      parseByteString("1:s".toList),
      Some(("s", List.empty))
    )

    assertEquals(
      parseByteString("0:".toList),
      Some(("", List.empty))
    )

    assertEquals(
      parseByteString("2:ss".toList),
      Some(("ss", List.empty))
    )

    assertEquals(
      parseByteString("10:ssssssssss".toList),
      Some(("ssssssssss", List.empty))
    )

    // these 2 tests now pass since the parse string function
    // as become less eager and only checks that there's at least
    // the info requested to parse. Doesn't care if there's trash afterwards.
    assertEquals(
      parseByteString("0:ss".toList),
      Some(("", List('s', 's')))
    )

    assertEquals(
      parseByteString("1:ss".toList),
      Some(("s", List('s')))
    )

    // todo: test limits of Ints. As is currently represented I can't even
    //  create a string of the max size in the test.
    // parseByteString("2147483647:")
  }

  test("parseInteger ❌") {
    assertEquals(parseInteger("i010e".toList), None)
    assertEquals(parseInteger("iss1e".toList), None)
    assertEquals(parseInteger("i5ie".toList), None)
    assertEquals(parseInteger("i0101010101010000000000000".toList), None)

    assertEquals(parseInteger("i-0e".toList), None)

    assertEquals(parseInteger("i-s5e".toList), None)
    assertEquals(parseInteger("i-i1e".toList), None)

    assertEquals(
      parseInteger(
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e".toList
      ),
      None
    )
  }

  test("parseInteger ✔") {
    assertEquals(
      parseInteger("i1e".toList),
      Some((1L, List.empty))
    )

    assertEquals(
      parseInteger("i10e".toList),
      Some((10L, List.empty))
    )

    assertEquals(
      parseInteger("i9999999999e:".toList),
      Some((9999999999L, List(':')))
    )

    assertEquals(
      parseInteger("i0e".toList),
      Some((0L, List.empty))
    )

    assertEquals(
      parseInteger("i-50e".toList),
      Some((-50L, List.empty))
    )

    assertEquals(
      parseInteger("i-9999999999e:".toList),
      Some((-9999999999L, List(':')))
    )
  }
}
