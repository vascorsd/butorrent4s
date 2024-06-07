// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

private val i: Byte     = 0x69 // 'i'
private val l: Byte     = 0x6c // 'l'
private val d: Byte     = 0x64 // 'd'
private val e: Byte     = 0x65 // 'e'
private val zero: Byte  = 0x30 // '0'
private val nine: Byte  = 0x39 // '9'
private val minus: Byte = 0x2d // '-'
private val colon: Byte = 0x3a // ':'

private def isASCIIDigit(c: Byte) = zero <= c && c <= nine
