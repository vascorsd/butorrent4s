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
}
