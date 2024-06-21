// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scodec.bits.*

class EncoderTests extends munit.FunSuite {
   test("encoder: String") {
      assertEquals(encode(""), utf8Bytes"0:")
      assertEquals(encode("hello"), utf8Bytes"5:hello")
   }

   test("encoder: ByteVector") {}

   test("encoder: Int") {
      assertEquals(encode(0), utf8Bytes"i0e")
      assertEquals(encode(10), utf8Bytes"i10e")
      assertEquals(encode(1000000000), utf8Bytes"i1000000000e")
      assertEquals(encode(-1000000000), utf8Bytes"i-1000000000e")
      assertEquals(encode(-10), utf8Bytes"i-10e")
      assertEquals(encode(-0), utf8Bytes"i0e")

      assertEquals(encode(Int.MinValue), utf8Bytes"i-2147483648e")
      assertEquals(encode(Int.MaxValue), utf8Bytes"i2147483647e")
   }

   test("encoder: Long") {
      assertEquals(encode(0L), utf8Bytes"i0e")
      assertEquals(encode(10L), utf8Bytes"i10e")
      assertEquals(encode(1000000000000000000L), utf8Bytes"i1000000000000000000e")
      assertEquals(encode(-1000000000000000000L), utf8Bytes"i-1000000000000000000e")
      assertEquals(encode(-10L), utf8Bytes"i-10e")
      assertEquals(encode(-0L), utf8Bytes"i0e")

      assertEquals(encode(Long.MinValue), utf8Bytes"i-9223372036854775808e")
      assertEquals(encode(Long.MaxValue), utf8Bytes"i9223372036854775807e")
   }

   test("encoder: BigInt") {}

   test("encoder: List of String") {}

   test("encoder: List of Int") {}

   test("encoder: List of String & Int") {
      assertEquals(encode(List.empty[String]), utf8Bytes"le")
      assertEquals(encode(List("hey")), utf8Bytes"l3:heye")

      // interesting, union type infered and summon resolution fails to compile !
      //   [error] ./test/encoder.test.scala:51:43
      //   [error] No given instance of type butorrent4s.Encoder[List[String | Int]] was found for a context parameter of method encode in package butorrent4s.
      //   [error] I found:
      //   [error]
      //   [error]     butorrent4s.Encoder.given_Encoder_List[String | Int](
      //   [error]       /* missing */summon[butorrent4s.Encoder[String | Int]])
      //   [error]
      //   [error] But no implicit values were found that match type butorrent4s.Encoder[String | Int].
      //   [error]       assertEquals(encode(List("hey", 10)), utf8Bytes"l3:heyi10ee")
      //   [error]
      // assertEquals(encode(List("hey", 10)), utf8Bytes"l3:heyi10ee")

      // also doesn't compile:
      //   [error] No given instance of type butorrent4s.Encoder[List[Any]] was found for a context parameter of method encode in package butorrent4s.
      //   [error] I found:
      //   [error]
      //   [error]     butorrent4s.Encoder.given_Encoder_List[Any](
      //   [error]       /* missing */summon[butorrent4s.Encoder[Any]])
      //   [error]
      //   [error] But no implicit values were found that match type butorrent4s.Encoder[Any].
      //   [error]       assertEquals(encode(List[Any]("hey", 10)), utf8Bytes"l3:heye")
      // assertEquals(encode(List[Any]("hey", 10)), utf8Bytes"l3:heyi10ee")

      // So only actually possible to use a list of elements of the same type:
      assertEquals(encode(List("hey", "mom")), utf8Bytes"l3:hey3:mome")
      assertEquals(encode(List(5, -10)), utf8Bytes"li5ei-10ee")
   }

   test("encoder: Tuples") {
      // tuples are just the plain values, one after the other.
      // wanted to make it encode as a list but I'm too dumb to work around how the Tuple encoders work.

      assertEquals(encode(Tuple.apply()), utf8Bytes"")
      assertEquals(encode(Tuple.apply(1)), utf8Bytes"i1e")
      assertEquals(encode("hey", 10, 5 :: 10 :: Nil), utf8Bytes"3:heyi10eli5ei10ee")
   }

   test("encoder: Case classes") {
      import Encoder.*

      case class Hi(name: String, age: Int) derives Encoder
      val h = Hi("john", 42)

      val encodedData = h.encode

      assertEquals(encodedData, utf8Bytes"")
   }
}
