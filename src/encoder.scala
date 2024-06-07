// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scodec.bits.*

def encode(string: String): ByteVector = {
   byteStringE(string)
}

// ------- Internals --------

def byteStringE(str: String): ByteVector = {
   byteStringE(ByteVector.view(str.getBytes("UTF-8")))
}

def byteStringE(input: ByteVector): ByteVector = {
   val strLen: Long = input.size

   ByteVector.encodeUtf8(s"${strLen}:").toOption.get
      ++ input
}

def integerE(l: Long) = {
   ByteVector.encodeUtf8(s"i${l}e").toOption.get
}

// lists of simple direct types, for HLists will need something else...
// type class needed fror elems of List ?
def listE(l: List[String]) = {
   val bl = l.map(byteStringE)

   ByteVector.encodeUtf8(s"l").toOption.get ++
      ByteVector.concat(bl) ++
      ByteVector.encodeUtf8(s"e").toOption.get
}

// can't do this because List[String] and List[Long} are the same shit on the jvm
//def listE(l: List[Long]) = {
//   val bl = l.map(integerE)
//
//   ByteVector.encodeUtf8(s"l").toOption.get ++
//      ByteVector.concat(bl) ++
//      ByteVector.encodeUtf8(s"e").toOption.get
//}
