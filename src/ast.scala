// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import java.util.Comparator

// notes:
//  - String - there's nothing saying that the string has a limit, or that it
//             is only ascii or even utf8 or any other unicode encoding.
//             It could just represent random bytes.
//
//  - Integer - There's also nothing saying that there's a limit or size for the
//              value, we could represent this as a BigDecimal, or a union of possible
//              types, like Integer | Long | BigDecimal.
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
  case BString(v: Array[Byte])
  case BInteger(v: Long)
  case BList(v: List[Bencode])
  case BDictionary(v: List[(BString, Bencode)])

  override def equals(o: Any): Boolean = {
    (this, o) match {
      case (BString(a), BString(b))         => a.sameElements(b)
      case (BInteger(a), BInteger(b))       => a == b
      case (BList(a), BList(b))             => a == b
      case (BDictionary(a), BDictionary(b)) => a == b
      case _                                => false
    }
  }

  override def toString: String = this match {
    case BString(v) =>
      s"bstring\"${String(v, "UTF-8")}\""

    case BInteger(v) =>
      s"bint:${v}"

    case BList(v) =>
      s"blist[ ${v.map(_.toString()).mkString(", ")} ]"

    case BDictionary(v) =>
      s"bdict{ ${v.map((k, v) => s"$k -> $v").mkString(", ")} }"
  }
}

object Bencode {

  def bstring(s: String): BString      = BString(s.getBytes("UTF-8"))
  def bstring(b: Array[Byte]): BString = BString(b)

  def binteger(l: Long): BInteger = BInteger(l)

  def blist(): BList                     = BList(List.empty)
  def blist(elem: Bencode): BList        = BList(elem :: Nil)
  def blist(elems: Bencode*): BList      = BList(elems.toList)
  def blist(elems: List[Bencode]): BList = BList(elems)

  def bdictionary(): BDictionary                                = BDictionary(Nil)
  def bdictionary(k: BString, v: Bencode): BDictionary          = BDictionary(k -> v :: Nil)
  def bdictionary(elems: (BString, Bencode)*): BDictionary      = BDictionary(elems.toList)
  def bdictionary(elems: List[(BString, Bencode)]): BDictionary = BDictionary(elems)

  extension (lc: BList.type) {
    def empty: BList = blist()
  }

  extension (l: BList) {
    def isEmpty: Boolean  = l.v.isEmpty
    def nonEmpty: Boolean = l.v.nonEmpty
  }

  extension (lc: BDictionary.type) {
    def empty: BDictionary = bdictionary()
  }

  extension (d: BDictionary) {
    def isEmpty: Boolean  = d.v.isEmpty
    def nonEmpty: Boolean = d.v.nonEmpty
  }

  given Ordering[BString] = Ordering.by[BString, String] { bs => String(bs.v, "UTF-8") }
}
