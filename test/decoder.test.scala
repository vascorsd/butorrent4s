package butorrent4s

import Bencode.*

extension (s: String) {
  def in: Array[Byte] = s.getBytes("UTF-8")
}

def resultOk[A](v: A, r: Array[Byte]): ParseResult[A] = Some((v, r))
def resultBad: ParseResult[Bencode] = None

class DecoderTests extends munit.FunSuite {

  test("parserByteString ❌ - invalid inputs") {
    assertEquals(byteStringP("".in), resultBad)
    assertEquals(byteStringP(":".in), resultBad)
    assertEquals(byteStringP("s".in), resultBad)
    assertEquals(byteStringP("ss".in), resultBad)
    assertEquals(byteStringP("s:".in), resultBad)
    assertEquals(byteStringP("ss:".in), resultBad)
    assertEquals(byteStringP("1".in), resultBad)
    assertEquals(byteStringP("1s".in), resultBad)
    assertEquals(byteStringP("1s:".in), resultBad)
    assertEquals(byteStringP("s1".in), resultBad)
    assertEquals(byteStringP("s1:".in), resultBad)
    assertEquals(byteStringP("01:".in), resultBad)

    // making sure the only valid numbers are ascii decimal digits:
    // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
    // from: https://www.unicode.org/terminology/digits.html
    assertEquals(
      byteStringP(
        "١:s".in
      ),
      resultBad
    )
  }

  test("byteStringP ✔ - valid inputs") {
    assertEquals(
      byteStringP("1:s".in),
      resultOk(bstring("s"), "".in)
    )

    assertEquals(
      byteStringP("0:".in),
      resultOk(bstring(""), "".in)
    )

    assertEquals(
      byteStringP("2:ss".in),
      resultOk(bstring("ss"), "".in)
    )

    assertEquals(
      byteStringP("10:ssssssssss".in),
      resultOk(bstring("ssssssssss"), "".in)
    )

    // these 2 tests now pass since the parse string function
    // as become less eager and only checks that there's at least
    // the info requested to parse. Doesn't care if there's trash afterwards.
    assertEquals(
      byteStringP("0:ss".in),
      resultOk(bstring(""), "ss".in)
    )

    assertEquals(
      byteStringP("1:ss".in),
      resultOk(bstring("s"), "s".in)
    )

    // todo: test limits of Ints. As is currently represented I can't even
    //  create a string of the max size in the test.
    // parseByteString("2147483647:")
  }

  test("parserInteger ❌ - invalid inputs") {
    assertEquals(integerP("i010e".in), resultBad)
    assertEquals(integerP("iss1e".in), resultBad)
    assertEquals(integerP("i5ie".in), resultBad)
    assertEquals(
      integerP("i0101010101010000000000000".in),
      resultBad
    )

    assertEquals(integerP("i-0e".in), resultBad)

    assertEquals(integerP("i-s5e".in), resultBad)
    assertEquals(integerP("i-i1e".in), resultBad)

    assertEquals(
      integerP(
        "i000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e".in
      ),
      resultBad
    )

    // making sure the only valid numbers are ascii decimal digits:
    // Arabic-script digits - ٠١٢٣٤٥٦٧٨٩
    // from: https://www.unicode.org/terminology/digits.html
    assertEquals(
      integerP(
        "i١٢e".in
      ),
      resultBad
    )
  }

  test("parserInteger ✔ - valid inputs") {
    val Some((int1, r1)) = integerP("i1e".in): @unchecked
    assertEquals(int1, binteger(1L))
    assertEquals(String(r1), "")

    val Some((int2, r2)) = integerP("i10e".in): @unchecked
    assertEquals(int2, binteger(10L))
    assertEquals(String(r1), "")

    val Some((int3, r3)) = integerP("i9999999999e:".in): @unchecked
    assertEquals(int3, binteger(9999999999L))
    assertEquals(String(r3), ":")

    val Some((int4, r4)) = integerP("i0e".in): @unchecked
    assertEquals(int4, binteger(0L))
    assertEquals(String(r4), "")

    val Some((int5, r5)) = integerP("i-50e".in): @unchecked
    assertEquals(int5, binteger(-50L))
    assertEquals(String(r5), "")

    val Some((int6, r6)) = integerP("i-9999999999e:".in): @unchecked
    assertEquals(int6, binteger(-9999999999L))
    assertEquals(String(r6), ":")
  }

  test("parserList ❌ - invalid inputs") {
    assertEquals(listP("".in), resultBad)
    assertEquals(listP("e".in), resultBad)
    assertEquals(listP("l".in), resultBad)

    assertEquals(listP("l:e".in), resultBad)
    assertEquals(listP("l1e".in), resultBad)
    assertEquals(listP("l1:e".in), resultBad)

    assertEquals(listP("lie".in), resultBad)
    assertEquals(listP("li-ee".in), resultBad)
    assertEquals(listP("li10e5e".in), resultBad)

    assertEquals(listP("lle".in), resultBad)
    assertEquals(listP("ll5e".in), resultBad)
  }

  test("parserList ✔ - valid inputs") {
    assertEquals(
      listP("le".in),
      resultOk(blist(), "".in)
    )

    assertEquals(
      listP("lee".in),
      resultOk(blist(), "e".in)
    )

    assertEquals(
      listP("l0:e".in),
      resultOk(
        blist(
          bstring("")
        ),
        "".in
      )
    )

    assertEquals(
      listP("l0:2:ssi1elee".in),
      resultOk(
        blist(
          bstring(""),
          bstring("ss"),
          binteger(1L),
          blist()
        ),
        "".in
      )
    )
  }

  test("parserDict ❌ - invalid inputs") {
    assertEquals(dictionaryP("d".in), resultBad)
    assertEquals(dictionaryP("e".in), resultBad)

    assertEquals(dictionaryP("die".in), resultBad)
    assertEquals(dictionaryP("did".in), resultBad)

    assertEquals(dictionaryP("dld".in), resultBad)
    assertEquals(dictionaryP("dl5d".in), resultBad)
    assertEquals(dictionaryP("dlelel".in), resultBad)

    assertEquals(dictionaryP("d5".in), resultBad)
    assertEquals(dictionaryP("d5d".in), resultBad)
    assertEquals(dictionaryP("d1:e".in), resultBad)
    assertEquals(dictionaryP("d1:s".in), resultBad)
    assertEquals(dictionaryP("d1:sle".in), resultBad)
    assertEquals(dictionaryP("d1:sli0".in), resultBad)

    // almost valid encoding, problem b2 key comes before b1, wich is unordered
    assertEquals(
      dictionaryP(
        "d1:a1:a2:b2i2e2:b1i1e1:c1:ce".in
      ),
      resultBad,
      "".in
    )

    // duplicated key problem
    assertEquals(
      dictionaryP(
        "d0:0:0:0:e".in
      ),
      resultBad,
      "".in
    )

    // duplicated key problem not immediately after
    assertEquals(
      dictionaryP(
        "d0:0:1:s0:0:1:se".in
      ),
      resultBad,
      "".in
    )
  }

  test("parserDict ✔ - valid inputs") {
    assertEquals(
      dictionaryP("d0:lee".in),
      resultOk(
        bdictionary(
          bstring("") -> blist()
        ),
        "".in
      )
    )

    assertEquals(
      dictionaryP("d0:i0eefuuu".in),
      resultOk(
        bdictionary(
          bstring("") -> binteger(0)
        ),
        "fuuu".in
      )
    )

    assertEquals(
      dictionaryP(
        "d1:a3:hey2:b1i0e2:b2le1:cl3:mome1:dd2:fu3:baree...".in
      ),
      resultOk(
        bdictionary(
          bstring("a") -> bstring("hey"),
          bstring("b1") -> binteger(0),
          bstring("b2") -> blist(),
          bstring("c") -> blist(bstring("mom")),
          bstring("d") -> bdictionary(
            bstring("fu") -> bstring("bar")
          )
        ),
        "...".in
      )
    )

    // from spec examples:
    assertEquals(
      dictionaryP("d3:cow3:moo4:spam4:eggse".in),
      resultOk(
        bdictionary(
          bstring("cow") -> bstring("moo"),
          bstring("spam") -> bstring("eggs")
        ),
        "".in
      )
    )

    assertEquals(
      dictionaryP("d4:spaml1:a1:bee".in),
      resultOk(
        bdictionary(
          bstring("spam") -> blist(bstring("a"), bstring("b"))
        ),
        "".in
      )
    )

    assertEquals(
      dictionaryP(
        "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee".in
      ),
      resultOk(
        bdictionary(
          bstring("publisher") -> bstring("bob"),
          bstring("publisher-webpage") -> bstring("www.example.com"),
          bstring("publisher.location") -> bstring("home")
        ),
        "".in
      )
    )

    assertEquals(
      dictionaryP("de".in),
      resultOk(bdictionary(), "".in)
    )
  }
}
