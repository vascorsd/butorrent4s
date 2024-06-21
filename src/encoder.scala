// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scala.compiletime.*
import scala.deriving.*

import scodec.bits.*

// ----- main operation ------
def encode[A: Encoder](value: A): ByteVector = Encoder[A].encode(value)

// ----- Encoder typeclass and its instances -----

trait Encoder[-A] {
   def encode(value: A): ByteVector
   // extension (value: A) inline def encode: ByteVector

   // we going to need a way to get the ast version for the encoder,
   // so we can use the proper way to construct bdictionary at least.
   // there's a known way to get a proper AST Bencode from ByteVector... decode it ^^
   // def ast(value: A): Bencode = decode(encode(value)).toOption.get._3
}

object Encoder {
   inline def apply[A: Encoder]: Encoder[A]           = summon[Encoder[A]]
   extension [A: Encoder](value: A) inline def encode = Encoder[A].encode(value)

   // --- Encoders for the AST values directly.
   //     Shouldn't really be used much by user code... but useful internally.
   //
   //     note: AST encoders assume that the ast values have been correctly created by their
   //     safe constructors. If manually created a BDictionary with repeated keys using
   //     an unsafe constructor that will be reflected in the output of these encoders.
   given Encoder[Bencode.BString] = { value =>
      value.fold(Encoder[ByteVector].encode, Encoder[String].encode)
   }

   given Encoder[Bencode.BInteger] = { value =>
      Encoder[BigInt].encode(value.v)
   }

   given Encoder[Bencode.BList] = { value =>
      Encoder[List[Bencode]].encode(value.v)
   }

   given Encoder[Bencode.BDictionary] = { value =>
      value.v.foldLeft(utf8Bytes"d") { case (acc, (k, v)) =>
         acc ++ k.encode ++ v.encode
      } ++ utf8Bytes"e"
   }

   // --- Basic encoders for all the relevant primitive types.
   given Encoder[Bencode] = { value =>
      value match {
         case bs @ Bencode.BString(_)     => bs.encode
         case bi @ Bencode.BInteger(_)    => bi.encode
         case bl @ Bencode.BList(_)       => bl.encode
         case bd @ Bencode.BDictionary(_) => bd.encode
      }
   }

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

   // --- Tuple Encoders:
   //   - default: just encode each value one after another.
   //   - alternative: not in the default instances, can be called explicitly - encode tuple elements as a blist

   given Encoder[EmptyTuple] = { (value: EmptyTuple) => ByteVector.empty }

   // noinspection NoTargetNameAnnotationForOperatorLikeDefinition -- intellij problem with return type
   given [H: Encoder, T <: Tuple: Encoder]: Encoder[H *: T] = { (value: H *: T) =>
      summon[Encoder[H]].encode(value.head) ++ summon[Encoder[T]].encode(value.tail)
   }

   // isn't ? the same as Any ? why can't we replace one for the other everywhere ??
   private inline def elemsInstances[T <: Tuple]: List[Encoder[?]] = {
      inline erasedValue[T] match {
         case _: EmptyTuple => Nil
         case _: (h *: t)   => summonInline[Encoder[t]] :: elemsInstances[t]
      }
   }

   // removed the extension on the Encoder because I have no idea how to call the encoder inside here with it.
   inline def encoderForTupleAsList[T <: Tuple]: Encoder[T] = { (value: T) =>
      value.productIterator
         .zip(elemsInstances[T])
         .foldLeft(utf8Bytes"l") { case (acc, (value, encoder: Encoder[?])) =>
            acc ++ encoder.asInstanceOf[Encoder[Any]].encode(value)
         }
         ++ utf8Bytes"l"
   }

   // --- Product aka case class Encoders, to be used with 'derives':

   // trying to rewrite from:
   //  inline given derived[A](using m: Mirror.Of[A]): Encoder[A] = { value => ... }
   //    which compiled but had a huge warning:
   //
   //  [warn] ./src/encoder.scala:130:65
   //  [warn] An inline given alias with a function value as right-hand side can significantly increase
   //  [warn] generated code size. You should either drop the `inline` or rewrite the given with an
   //  [warn] explicit `apply` method.
   //  [warn]
   //  [warn] Explanation
   //  [warn] ===========
   //  [warn] A function value on the right-hand side of an inline given alias expands to
   //  [warn] an anonymous class. Each application of the inline given will then create a
   //  [warn] fresh copy of that class, which can increase code size in surprising ways.
   //  [warn] For that reason, functions are discouraged as right hand sides of inline given aliases.
   //  [warn] You should either drop `inline` or rewrite to an explicit `apply` method. E.g.
   //  [warn]
   //  [warn]     inline given Conversion[A, B] = x => x.toB
   //  [warn]
   //  [warn] should be re-formulated as
   //  [warn]
   //  [warn]     given Conversion[A, B] with
   //  [warn]       inline def apply(x: A) = x.toB
   //  [warn]
   //
   //  to:
   //
   //  inline given derived[A](using m: Mirror.Of[A]): Encoder[A] with { inline def encode(value:A) ... }
   //    which fails at compilation about scrutinees and wtv.
   //
   // fuck all of this.
   //
   // Compiling project (Scala 3.3.3, JVM (system))
   // [error] ./src/encoder.scala:139:49
   // [error] cannot reduce inline match with
   // [error]  scrutinee:  this.butorrent4s$Encoder$derived$$inline$m : (derived.this.butorrent4s$Encoder$derived$$inline$m :
   // [error]   => scala.deriving.Mirror.Of[A])
   // [error]  patterns :  case s @ _:scala.deriving.Mirror.SumOf[derived.this.A]
   // [error]              case _:scala.deriving.Mirror.ProductOf[derived.this.A]
   // [error]       inline def encode(value: A): ByteVector = {
   // [error]                                                 ^
   // Error compiling project (Scala 3.3.3, JVM (system))
   // Compilation failed
   //
   //
   // Anyway before the tests were failing anyway with the previous encoding with:
   //
   // ==> X butorrent4s.EncoderTests.encoder: Case classes  0.003s java.lang.ClassCastException: class java.lang.Integer cannot be cast to class scala.Tuple$package$EmptyTuple$ (java.lang.Integer is in module java.base of loader 'bootstrap'; scala.Tuple$package$EmptyTuple$ is in unnamed module of loader 'app')
   //    at butorrent4s.EncoderTests.derived$Encoder$lzyINIT1$$anonfun$1$$anonfun$4(encoder.test.scala:91)
   //    at scala.collection.LinearSeqOps.foldLeft(LinearSeq.scala:183)
   //    at scala.collection.LinearSeqOps.foldLeft$(LinearSeq.scala:179)
   //    at scala.collection.immutable.List.foldLeft(List.scala:79)
   //    at butorrent4s.EncoderTests.butorrent4s$EncoderTests$Hi$3$$$_$derived$Encoder$lzyINIT1$$anonfun$1(encoder.test.scala:91)
   //    at butorrent4s.EncoderTests.$init$$$anonfun$10(encoder.test.scala:94)
   //
   // SO, nothing fucking works yay
   inline given derived[A](using m: Mirror.Of[A]): Encoder[A] with {
      inline def encode(value: A): ByteVector = {
         val instances = elemsInstances[m.MirroredElemTypes]

         inline m match {
            case s: Mirror.SumOf[A]     => ByteVector.empty
            case _: Mirror.ProductOf[A] =>
               val product: Product = value.asInstanceOf[Product]

               product.productElementNames
                  .zip(product.productIterator)
                  .zip(instances)
                  .to(List)
                  .sortBy { case ((key, _), _) => key }
                  .foldLeft(utf8Bytes"d") { case (acc, ((name, field), enc)) =>
                     acc ++
                        name.encode ++
                        enc.asInstanceOf[Encoder[Any]].encode(field)
                  } ++ utf8Bytes"e"
         }
      }
   }
}
