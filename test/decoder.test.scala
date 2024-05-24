package butorrent4s

class DecoderTests extends munit.FunSuite {
  test("parseByteString ❌") {
    assert(parseByteString("".toList) == None)
    assert(parseByteString(":".toList) == None)
    assert(parseByteString("s".toList) == None)
    assert(parseByteString("ss".toList) == None)
    assert(parseByteString("s:".toList) == None)
    assert(parseByteString("ss:".toList) == None)
    assert(parseByteString("1".toList) == None)
    assert(parseByteString("1s".toList) == None)
    assert(parseByteString("1s:".toList) == None)
    assert(parseByteString("s1".toList) == None)
    assert(parseByteString("s1:".toList) == None)
    assert(parseByteString("1:ss".toList) == None)
    assert(parseByteString("0:ss".toList) == None)
    assert(parseByteString("01:".toList) == None)
  }

  test("parseByteString ✔") {
    assert(parseByteString("1:s".toList) == Some("s"))
    assert(parseByteString("0:".toList) == Some(""))
    assert(parseByteString("2:ss".toList) == Some("ss"))
    assert(parseByteString("10:ssssssssss".toList) == Some("ssssssssss"))
  }

  test("parseInteger ❌") {
    assert(parseInteger("i010e".toList) == None)
    assert(parseInteger("iss1e".toList) == None)
    assert(parseInteger("i5ie".toList) == None)
    assert(parseInteger("i0101010101010000000000000".toList) == None)

    assert(parseInteger("i-0e".toList) == None)

    assert(parseInteger("i-s5e".toList) == None)
    assert(parseInteger("i-i1e".toList) == None)
  }

  test("parseInteger ✔") {
    assert(parseInteger("i1e".toList) == Some(1))
    assert(parseInteger("i10e".toList) == Some(10))
    assert(parseInteger("i9999999999e:".toList) == Some(9999999999L))

    assert(parseInteger("i0e".toList) == Some(0))

    assert(parseInteger("i-50e".toList) == Some(-50))
    assert(parseInteger("i-9999999999e:".toList) == Some(-9999999999L))
  }
}
