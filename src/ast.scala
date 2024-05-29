// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

// notes:
//  - String - there's nothing saying that the string has a limit, or that it
//             is only ascii or even utf8 or any other unicode encoding.
//             It could just represent random bytes.
//             So, lower the requirement to make suer we are passing along bytes and not
//             Scala's String. That can be a conversion somewhere else.
//
//  - Integer - There's also nothing saying that there's a limit or size for the
//              value, we could represent this as a BigDecimal, or a union of possible
//              types, like Integer | Long | BigDecimal.
//
//  - Dictionary - it needs to be explicit ordered by the keys on the encoded values.
//                 It seems straighforward to represent that at this level by a list,
//                 which we can validate at the procotol level to be correct, then at
//                 an higher level we can convert that to a Map which the user can then
//                 access with wtv pattern they want.
//                 The keys need to be lexographhic ordered which if I eventually change the
//                 BString to be raw bytes, I'm not sure how it's gonna look...

enum Bencode:
  case BString(v: String)
  case BInteger(v: Long)
  case BList(v: List[Bencode])
  case BDictionary(v: List[(BString, Bencode)])

object Bencode:
  def bstring(s: String): Bencode = BString(s)
  def bstring(b: Array[Byte]): Bencode = BString(String(b))
  def binteger(l: Long): Bencode = BInteger(l)

  def blist(): Bencode = BList(List.empty)
  def blist(elem: Bencode): Bencode = BList(elem :: Nil)
  def blist(elems: Bencode*): Bencode = BList(elems.toList)
  def blist(elems: List[Bencode]): Bencode = BList(elems)

  def bdictionary() = BDictionary(Nil)
  def bdictionary(k: BString, v: Bencode) = BDictionary(k -> v :: Nil)
  def bdictionary(elems: (BString, Bencode)*) = BDictionary(elems.toList)
