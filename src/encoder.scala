// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scodec.bits.*

// ----- main operation ------
def encode[A: Encoder](value: A): ByteVector = value.encode

// ----- Encoder typeclass and its instances -----

trait Encoder[-A] {
   extension (value: A) def encode: ByteVector
}

object Encoder {
   given Encoder[ByteVector] = { (value: ByteVector) =>
      ByteVector.encodeUtf8(s"${value.size}:").toOption.get
         ++ value
   }

   given Encoder[String] = { (value: String) =>
      ByteVector.view(value.getBytes("UTF-8")).encode
   }

   given Encoder[BigInt] = { (value: BigInt) =>
      ByteVector.encodeUtf8(s"i${value.toString(10)}e").toOption.get
   }

   given Encoder[Long] = { (value: Long) =>
      ByteVector.encodeUtf8(s"i${value}e").toOption.get
   }

   given Encoder[Int] = { (value: Int) =>
      ByteVector.encodeUtf8(s"i${value}e").toOption.get
   }

   given [A: Encoder]: Encoder[List[A]] = { (value: List[A]) =>
      value.foldLeft(utf8Bytes"l") { (acc, n) =>
         acc ++ n.encode
      } ++ utf8Bytes"e"
   }

   // help from:
   //  - https://blog.rockthejvm.com/practical-type-level-programming/
   //  - https://github.com/iRevive/union-derivation
   //  - https://stackoverflow.com/questions/75873631/tuples-in-scala-3-compiler-operations-for-typeclass-derivation
   //
   // TLDR: I'm too dumb for this. the errors are crazy and unreadable. Nothing works.
   // just he most basic thing works as is defined. If I want to encode the tuples a a List, I'm fucked.

//   import scala.compiletime.*
//
//   inline def fuck[T <: Tuple](v: T) = {
//      val encoders = summonAll[Tuple.Map[T, Encoder]]
//
//      encodeTuple(v)
//   }
//
//   inline def encodeTuple[T <: Tuple](
//       encs: Tuple.Map[T, Encoder]
//   ): Encoder[T] = {
//      inline encs match {
//         case _: EmptyTuple => encoderEmptyTuple.asInstanceOf[Encoder[T]]
//         case tup: (h *: t) => encoderNonEmptyTuple(tup.head, encodeTuple(tup.tail))
//      }
//   }
//
//   def encoderEmptyTuple: Encoder[EmptyTuple]                                 = { (value: EmptyTuple) => ByteVector.empty }
//   def encoderNonEmptyTuple[H: Encoder, T <: Tuple: Encoder]: Encoder[H *: T] = { (value: H *: T) =>
//      value.head.encode ++ value.tail.encode
//   }

   // noinspection NoTargetNameAnnotationForOperatorLikeDefinition
   given [H: Encoder, T <: Tuple: Encoder]: Encoder[H *: T] = { (value: H *: T) =>
      summon[Encoder[H]].encode(value.head) ++ summon[Encoder[T]].encode(value.tail)
   }

   given Encoder[EmptyTuple] = { (value: EmptyTuple) => ByteVector.empty }

}
