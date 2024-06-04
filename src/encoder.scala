// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import scodec.bits.*

def encode(string: String): ByteVector = {
   val b: Bencode.BString = Bencode.bstring(string)

   ByteVector.encodeUtf8(s"${b.v.size}:").toOption.get
      ++ b.v
}
