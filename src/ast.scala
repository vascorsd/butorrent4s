// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scala.annotation.tailrec
import scala.collection.{mutable, SeqView}

import scodec.bits.*

// notes:
//  - String - there's nothing saying that the string has a limit, or that it
//             is only ascii or even utf8 or any other unicode encoding.
//             It could just represent random bytes.
//
//  - Integer - There's also nothing saying that there's a limit or size for the
//              value, we could represent this as a BigInt, or a union of possible
//              types.
//
//  - List - The list doesn't need to be all of the same type and can be any of the other
//           known Bencode types all mixed together. In scala would be what we usually call
//           as HList or could be maybe encoded with the new Tuple syntax of scala 3.
//
//  - Dictionary - it needs to be explicit ordered by the keys on the encoded values.
//                 It seems straighforward to represent that at this level by a list,
//                 which we can validate at the procotol level to be correct, then at
//                 an higher level we can convert that to a Map which the user can then
//                 access with wtv pattern they want.
//                 The keys need to be lexographhic ordered and no duplicates can occur.

enum Bencode derives CanEqual {
   case BString private[Bencode] (v: ByteVector | String)
   case BInteger private[Bencode] (v: BigInt)
   case BList private[Bencode] (v: List[Bencode])
   case BDictionary private[Bencode] (v: List[(BString, Bencode)])

   override def toString: String = this match {
      case BString(v: ByteVector) =>
         if v.size < 200 then s"bstring|${v.size}:0x${v.toHex}|"
         else s"bstring|${v.size}:0x${v.take(200).toHex}...|"

      case BString(v: String) =>
         if v.length < 200 then s"""bstring"${v}""""
         else s"""bstring"${v.length}:${v.take(200)}...""""

      case BInteger(v) =>
         s"bint:${v}"

      case BList(v) =>
         s"blist[ ${v.map(_.toString()).mkString(", ")} ]"

      case BDictionary(v) =>
         s"bdict{ ${v.map((k, v) => s"$k -> $v").mkString(", ")} }"
   }
}

object Bencode {

   def bstringAsBytes(s: String): BString      = BString(ByteVector.view(s.getBytes("UTF-8")))
   def bstringAsBytes(bv: ByteVector): BString = BString(bv)
   def bstringAsStr(s: String): BString        = BString(String(s.getBytes("UTF-8"), "UTF-8"))

   def binteger(n: Int): BInteger    = BInteger(n)
   def binteger(n: Long): BInteger   = BInteger(n)
   def binteger(n: BigInt): BInteger = BInteger(n)

   def blist(): BList                     = BList(List.empty)
   def blist(elems: Bencode*): BList      = BList(elems.toList)
   def blist(elems: List[Bencode]): BList = BList(elems)

   // construction of safe and meaningful valid representations of the BDictionary.
   // the following are ensured:
   //  - no keys duplicated;
   //  - Strings are passed as keys - so they have a valid Unicode representation and we transform them into UTF8;
   //  - keys are lexicographically ordered - means that the order the elements are passed in is irrelavant
   //    and we make sure a correct order is done by explicitly ordering the elements ourselves;
   def bdictionary(): BDictionary                               = BDictionary(Nil)
   def bdictionary(k: String, v: Bencode): BDictionary          = BDictionary(BString(k) -> v :: Nil)
   def bdictionary(elems: (String, Bencode)*): BDictionary      = _bdictionary(elems.view)
   def bdictionary(elems: List[(String, Bencode)]): BDictionary = _bdictionary(elems.view)

   // try to reduce allocations avoiding multiple collections transformations and mappings
   // by using the views to have lazy behaviour.
   // mutable vs immutable TreeMap -> we just using for getting an ordering and remove duplicates,
   //   mutable should be faster to just do it and then throw away I guess... (?)
   private def _bdictionary(elems: SeqView[(String, Bencode)]): BDictionary = {
      given Ordering[BString] = Ordering.by(_.getStringUnsafe)

      val orderedElems: mutable.TreeMap[BString, Bencode] =
         elems.view.map((k, v) => (bstringAsStr(k), v)).to(mutable.TreeMap)

      BDictionary(orderedElems.toList)
   }

   // allows direct construction of the underlying BDictionary without checking none of the required
   // constraints to ensure it has valid meaning:
   //  - no key duplication is checked;
   //  - not checking BString for key has valid UTF8 for its representation;
   //  - not checking order and use the directly passed order of elements;
   def bdictionaryUnsafe(elems: (BString, Bencode)*): BDictionary      = BDictionary(elems.toList)
   def bdictionaryUnsafe(elems: List[(BString, Bencode)]): BDictionary = BDictionary(elems)

   extension (s: BString) {
      inline def fold[A](whenBytesFn: ByteVector => A, whenStringFn: String => A): A = s match {
         case BString(v: ByteVector) => whenBytesFn(v)
         case BString(v: String)     => whenStringFn(v)
      }

      def isBytes: Boolean                     = fold(_ => true, _ => false)
      def isString: Boolean                    = fold(_ => false, _ => true)
      def tryIntoString: Option[String]        = fold(
        bytes => bytes.decodeUtf8.toOption,
        str => Some(str)
      )
      def tryIntoBStringAsStr: Option[BString] = tryIntoString.map(BString(_))
      def getStringUnsafe: String              = fold(
        _ => throw new NoSuchElementException("BString is not 'String'"),
        identity
      )
      def getBytesUnsafe: ByteVector           = fold(
        identity,
        _ => throw new NoSuchElementException("BString is not 'ByteVector'")
      )
   }

   extension (l: BList) {
      def isEmpty: Boolean  = l.v.isEmpty
      def nonEmpty: Boolean = l.v.nonEmpty
   }

   extension (d: BDictionary) {
      def isEmpty: Boolean  = d.v.isEmpty
      def nonEmpty: Boolean = d.v.nonEmpty
   }
}
