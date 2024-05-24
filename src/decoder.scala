// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

// TODO:
//   - decode main entry point should decide which specific parsing to do.
//   - need to limit amount of info that can be pushed into us, to protect against infinite input
//   - probably change option to Eithers with specific error information
//   - output should return remaining input that has not being parsed. this will allow composition.
//     And also allows to replace the size check of the parse string.
//   - Use other encoding besides Lists...
//   - replace all of it by cats parser or any other custom parser combinators logic... (?)

package butorrent4s

import scala.annotation.tailrec

def decode(rawInput: String) = {
  val r = rawInput.charAt(0) match
    case 'i' => parseInteger(rawInput.toList)
    case 'l' => ???
    case 'd' => ???
    case _   => parseByteString(rawInput.toList)

  println(r)
}

@tailrec
def parseByteString(
    input: List[Char],
    acc: List[Char] = List.empty
): Option[String] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Byte_Strings
  //
  // Byte strings are encoded as follows: <string length encoded in base ten ASCII>:<string data>
  // Note that there is no constant beginning delimiter, and no ending delimiter.
  //
  //    Example: 4: spam represents the string "spam"
  //    Example: 0: represents the empty string ""

  input match {
    // Reached the ':', and found before it digits.
    case ':' :: xs if acc.nonEmpty =>
      // should not fail I guess, since we checked it's digits (?)
      // should we parse it to Int or Long (?)
      val strlen =
        acc.reverse.toArray.mkString.toIntOption

      // asking for more than available, is an error (?)
      // size asked should match the size of the remaining availabe input.
      strlen
        .filter(l => l == xs.size)
        .map(l => xs.take(l.toInt).toArray.mkString)

    // Next char is a digit, accumulate it, check next.
    case x :: xs if x.isDigit =>
      parseByteString(input = xs, acc = x :: acc)

    // No delimiter ':' and no digits, bad input
    case _ => None
  }
}

def parseInteger(
    input: List[Char]
): Option[Long] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Integers
  //
  // Integers are encoded as follows: i<integer encoded in base ten ASCII>e
  // The initial i and trailing e are beginning and ending delimiters.
  //
  //    Example: i3e represents the integer "3"
  //    Example: i-3e represents the integer "-3"
  //
  // i-0e is invalid. All encodings with a leading zero, such as i03e, are invalid, other than i0e,
  // which of course corresponds to the integer "0".
  //
  //    NOTE: The maximum number of bit of this integer is unspecified, but to handle it as a signed 64bit
  //    integer is mandatory to handle "large files" aka .torrent for more that 4Gbyte.

  // notes:
  // 1. number must be parsed to Long (64 bits) at least
  // 2. leading 0 is bad
  // 3. determine if negative number

  @tailrec
  def parseInnerLoop(
      isNegative: Boolean,
      input: List[Char],
      acc: List[Char]
  ): Option[Long] = {
    // todo(gotcha, fix, security): we have a problem that we are not shortcircuting as soon
    // as possible when we already know that many zeros are being pushed to the input
    // if starting by leading zero and we are only checking the validity if/when we encounter
    // the 'e' termination tag. This allows an attacker to push infinite zeros without
    // terminating and screw with us.
    //
    // Possible solutions: (1) Have a limited stack / memory size and only allow a certain amount of
    //                     data to be pushed. Probably needs something like this for the other parsers anyway.
    //                     Still allows to loop up until that size, but should not blow up.
    //                     (2) Exiting earlier, will complicate logic inside the parser.

    input match {
      case 'e' :: xs if acc.nonEmpty =>
        val digits = acc.reverse

        digits match {
          // we need to check if we have a valid encoding of the zero number.
          case '0' :: xs =>
            if isNegative || xs.nonEmpty then None
            else Some(0L)

          case xs =>
            // we need to recover the negative encoding since the acc only has digits,
            // not the negative sign.
            val numberToParse =
              if isNegative then '-' :: digits
              else digits

            numberToParse.toArray.mkString.toLongOption
        }

      case x :: xs if x.isDigit =>
        parseInnerLoop(isNegative, input = xs, acc = x :: acc)

      case _ =>
        None
    }
  }

  input match {
    case 'i' :: '-' :: x :: xs if x.isDigit =>
      parseInnerLoop(isNegative = true, input = xs, acc = x :: Nil)

    case 'i' :: x :: xs if x.isDigit =>
      parseInnerLoop(isNegative = false, input = xs, acc = x :: Nil)

    case _ =>
      None
  }
}

def parseList() = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Lists
  //
  // Lists are encoded as follows: l<bencoded values>e
  // The initial l and trailing e are beginning and ending delimiters.
  // Lists may contain any bencoded type, including integers, strings, dictionaries,
  // and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []Lists are encoded as follows: l<bencoded values>e
  //
  // The initial l and trailing e are beginning and ending delimiters.
  // Lists may contain any bencoded type, including integers, strings, dictionaries, and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []

  // notes:
  // 1. it seems nothing enforces that a list should have all elements be the same type.
  //    by the codecrafters info, possible to have a list of a string and a number. in scala this would be List[Any]
  // 2. ...

  ???
}

def parseDictionary() = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Dictionaries
  //
  // Dictionaries are encoded as follows: d<bencoded string><bencoded element>e
  // The initial d and trailing e are the beginning and ending delimiters.
  // Note that the keys must be bencoded strings. The values may be any bencoded type, including integers,
  // strings, lists, and other dictionaries.
  // Keys must be strings and appear in sorted order (sorted as raw strings, not alphanumerics).
  // The strings should be compared using a binary comparison, not a culture-specific "natural" comparison.
  //
  //    Example: d3:cow3:moo4:spam4:eggse represents the dictionary { "cow" => "moo", "spam" => "eggs" }
  //    Example: d4:spaml1:a1:bee represents the dictionary { "spam" => [ "a", "b" ] }
  //    Example: d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee represents { "publisher" => "bob", "publisher-webpage" => "www.example.com", "publisher.location" => "home" }
  //    Example: de represents an empty dictionary {}

  ???
}
