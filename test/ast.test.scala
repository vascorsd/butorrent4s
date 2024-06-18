// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scodec.bits.*

import Bencode.*

class AstTests extends munit.FunSuite {
   test("bstring using underlying ByteVector .toString") {
      val b = bstringAsBytes(ByteVector(colon))
      assertEquals(b.toString, "bstring|1:0x3a|")

      val big = bstringAsBytes(ByteVector.fill(600)(50))
      assertEquals(
        big.toString,
        "bstring|600:0x323232323232323232323232323232323232323232323232323232323232323232" +
           "3232323232323232323232323232323232323232323232323232323232323232323232323232323232" +
           "3232323232323232323232323232323232323232323232323232323232323232323232323232323232323232" +
           "323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323" +
           "23232323232323232323232323232323232323232323232323232323232323232323232...|"
      )
   }

   test("bstring using underlying String .toString") {
      val b = bstringAsStr(String(":"))
      assertEquals(b.toString, "bstring\":\"")

      val big = bstringAsStr(String("hey").repeat(300))
      assertEquals(
        big.toString,
        "bstring\"900:heyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyhey" +
           "heyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyheyhey" +
           "heyheyheyheyheyheyheyhe...\""
      )
   }

   test("bstring using ByteVector") {
      val b2 = bstringAsBytes(":")
      val b3 = bstringAsBytes(ByteVector(colon))

      // all these constructions should be the same
      assertEquals(b2, b3)
      assertEquals(b2, b3)

      // make sure the implemented methods tell the right information
      assert(b2.isBytes)
      assert(b3.isBytes)

      assert(!b2.isString)
      assert(!b3.isString)

      // when .isBytes true, getUnsafe should not fail
      assertEquals(b2.getBytesUnsafe, ByteVector(colon))
      assertEquals(b3.getBytesUnsafe, ByteVector(colon))

      // expect failure when getUnsafe for String
      intercept[NoSuchElementException] { b2.getStringUnsafe }
   }

   test("bstring using String") {
      val b1 = bstringAsStr("hey")
      val b2 = bstringAsStr("hey")

      // all these constructions should be the same
      assertEquals(b1, b2)

      // make sure the implemented methods tell the right information
      assert(b2.isString)
      assert(!b2.isBytes)

      // when .isBytes true, getUnsafe should not fail
      assertEquals(b2.getStringUnsafe, "hey")

      // expect failure when getUnsafe for String
      intercept[NoSuchElementException] { b2.getBytesUnsafe }
   }

   test("bstring with valid Unicode") {
      val b1 = bstringAsBytes("hey there")
      val b2 = bstringAsBytes("\u019F\u1E19\u1F019\u1F514")

      assert(b1.isBytes)
      assert(b2.isBytes)

      // check the conversion to String from the utf 8 bytes works
      assertEquals(b1.tryIntoString, Some("hey there"))
      assertEquals(b2.tryIntoString, Some("\u019F\u1E19\u1F019\u1F514"))

      // should be able to convert to a bstring using String as the underlying type
      val b1s = b1.tryIntoBStringAsStr.get
      val b2s = b2.tryIntoBStringAsStr.get

      assert(b1s.isString)
      assert(b2s.isString)

      // converting an already bstring with a String representation again will work ok.
      assertEquals(b1s.tryIntoBStringAsStr, Some(bstringAsStr("hey there")))
   }

   test("bstring with invalid Unicode bytes in its underlying ByteVector") {
      // it's totally possible to create and valid as bstring.
      val b1 = bstringAsBytes(hex"C0080")

      assert(b1.isBytes)

      // should fail the conversion since it's not possible to convert into a String
      assertEquals(b1.tryIntoString, None)
      assertEquals(b1.tryIntoBStringAsStr, None)
   }

   test("binteger construction and its .toString representation") {
      val b1 = binteger(10)
      val b2 = binteger(2000009999999999999L)
      val b3 = binteger(BigInt("9999999999999999999999999999999999999999999999999"))

      assertEquals(b1.toString(), "bint:10")
      assertEquals(b2.toString(), "bint:2000009999999999999")
      assertEquals(b3.toString(), "bint:9999999999999999999999999999999999999999999999999")
   }

   test("blist constructors create expected structure") {
      val bl1 = blist()
      assertEquals(bl1.toString, "blist[  ]")

      val bl2 = blist(binteger(10))
      assertEquals(bl2.toString, "blist[ bint:10 ]")

      val bl3 = blist(binteger(10), bstringAsStr("blah"))
      assertEquals(bl3.toString, """blist[ bint:10, bstring"blah" ]""")
   }

   test("bdictionary unsafe constructs don't alter the inputs") {
      val bl = bstringAsStr("zoomer") -> binteger(80) ::
         bstringAsBytes("bananas") -> binteger(6) ::
         bstringAsBytes("bananas") -> blist(binteger(3)) ::
         Nil

      val b1 = bdictionaryUnsafe(bl)
      val b2 = bdictionaryUnsafe(
        bstringAsStr("b") -> binteger(10),
        bstringAsStr("a") -> binteger(20)
      )

      assertEquals(
        b1.toString,
        """bdict{ bstring"zoomer" -> bint:80, bstring|7:0x62616e616e6173| -> bint:6, bstring|7:0x62616e616e6173| -> blist[ bint:3 ] }"""
      )

      assertEquals(
        b2.toString,
        """bdict{ bstring"b" -> bint:10, bstring"a" -> bint:20 }"""
      )
   }

   test("bdictionary safe constructors cleans up duplicates - last key repeated key wins, old removed") {
      val b1 = bdictionary(
        "hy" -> binteger(1),
        "hy" -> binteger(2)
      )

      val b2 = bdictionary(
        ""      -> blist() ::
           "hy" -> binteger(2) ::
           "hy" -> binteger(1) ::
           ""   -> binteger(5) ::
           Nil
      )

      assertEquals(b1.toString, """bdict{ bstring"hy" -> bint:2 }""")
      assertEquals(b2.toString, """bdict{ bstring"" -> bint:5, bstring"hy" -> bint:1 }""")
   }

   test("bdictionary safe constructors ensure lexicographic order by the keys") {
      val b1 = bdictionary(
        "hey"     -> blist(),
        "zzz"     -> binteger(1),
        "rr"      -> binteger(2),
        "bananas" -> bdictionary(),
        "Bananas" -> binteger(1),
        "car"     -> blist(),
        "Car"     -> binteger(1),
        "élite"   -> binteger(10),
        "étoile"  -> blist()
      )

      assertEquals(
        b1.toString,
        """bdict{ """
           + """bstring"Bananas" -> bint:1, """
           + """bstring"Car" -> bint:1, """
           + """bstring"bananas" -> bdict{  }, """
           + """bstring"car" -> blist[  ], """
           + """bstring"hey" -> blist[  ], """
           + """bstring"rr" -> bint:2, """
           + """bstring"zzz" -> bint:1, """
           + """bstring"élite" -> bint:10, """
           + """bstring"étoile" -> blist[  ] """
           + """}"""
      )
   }

   test("bdictionary can be constructured as empty") {
      val bd = bdictionary()

      assertEquals(bd.toString, "bdict{  }")
   }

   test("bdictionary can be constructed with a single element") {
      val bd1 = bdictionary("one", blist())

      assertEquals(bd1.toString, """bdict{ bstring"one" -> blist[  ] }""")

      val bd2 = bdictionary("two" -> blist())

      assertEquals(bd2.toString, """bdict{ bstring"two" -> blist[  ] }""")
   }
}
