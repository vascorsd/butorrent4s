class DecoderTests extends munit.FunSuite {
  test("parseByteString failure inputs") {
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

  test("parseByteString success inputs") {
    assert(parseByteString("1:s".toList) == Some("s"))
    assert(parseByteString("0:".toList) == Some(""))
    assert(parseByteString("2:ss".toList) == Some("ss"))
    assert(parseByteString("10:ssssssssss".toList) == Some("ssssssssss"))
  }
}
