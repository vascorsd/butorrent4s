def decode(rawInput: String) = {
  parseByteString(rawInput.toList).foreach(println(_))
}

def testDecode() = {
  // should fail:
  decode("")
  decode(":")
  decode("s")
  decode("ss")
  decode("s:")
  decode("ss:")
  decode("1")
  decode("1s")
  decode("1s:")
  decode("s1")
  decode("s1:")
  decode("1:ss")
  decode("0:ss")
  decode("01:")

  // should suceed:
  decode("1:s")
  decode("0:")
  decode("2:ss")
  decode("10:ssssssssss")

  // unicode string supported? size??

}

def parseByteString(
    input: List[Char],
    accDigits: List[Char] = List.empty
): Option[String] = {
  // From spec at: https://wiki.theory.org/BitTorrentSpecification#Byte_Strings
  //
  // Byte strings are encoded as follows: <string length encoded in base ten ASCII>:<string data>
  // Note that there is no constant beginning delimiter, and no ending delimiter.
  //
  //    Example: 4: spam represents the string "spam"
  //    Example: 0: represents the empty string ""

//  println("input: " + input)
//  println("acc: " + accDigits)

  input match {
    // Reached the ':', and found before it digits.
    case ':' :: xs if accDigits.nonEmpty =>
      // should not fail I guess, since we checked it's digits (?)
      // should we parse it to Int or Long (?)
      val strlen =
        accDigits.reverse.toArray.mkString.toIntOption

//      println("digits: " + digits)
//      println("len: " + strlen)
//      println("str xs: " + xs)
//      println("str xs len: " + xs.size)

      // asking for more than available, is an error (?)
      // size asked should match the size of the remaining availabe input.
      strlen
        .filter(l => l == xs.size)
        .map(l => xs.take(l.toInt).toArray.mkString)

    // Next char is a digit, accumulate it, check next.
    case x :: xs if x.isDigit =>
      parseByteString(input = xs, accDigits = x :: accDigits)

    // No delimiter ':' and no digits, bad input
    case _ => None
  }
}

def parseInteger() = {
  // Integers are encoded as follows: i<integer encoded in base ten ASCII>e
  // The initial i and trailing e are beginning and ending delimiters.
  //
  //    Example: i3e represents the integer "3"
  //    Example: i-3e represents the integer "-3"
  //
  // i-0e is invalid. All encodings with a leading zero, such as i03e, are invalid, other than i0e, which of course corresponds to the integer "0".
  //
  //    NOTE: The maximum number of bit of this integer is unspecified, but to handle it as a signed 64bit integer is mandatory to handle "large files" aka .torrent for more that 4Gbyte.

  ???
}
def parseList() = {
  // Lists are encoded as follows: l<bencoded values>e
  // The initial l and trailing e are beginning and ending delimiters. Lists may contain any bencoded type, including integers, strings, dictionaries, and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []Lists are encoded as follows: l<bencoded values>e
  // The initial l and trailing e are beginning and ending delimiters. Lists may contain any bencoded type, including integers, strings, dictionaries, and even lists within other lists.
  //
  //    Example: l4:spam4:eggse represents the list of two strings: [ "spam", "eggs" ]
  //    Example: le represents an empty list: []

  ???
}
def parseDictionary() = {
  // Dictionaries are encoded as follows: d<bencoded string><bencoded element>e
  // The initial d and trailing e are the beginning and ending delimiters. Note that the keys must be bencoded strings. The values may be any bencoded type, including integers, strings, lists, and other dictionaries. Keys must be strings and appear in sorted order (sorted as raw strings, not alphanumerics). The strings should be compared using a binary comparison, not a culture-specific "natural" comparison.
  //
  //    Example: d3:cow3:moo4:spam4:eggse represents the dictionary { "cow" => "moo", "spam" => "eggs" }
  //    Example: d4:spaml1:a1:bee represents the dictionary { "spam" => [ "a", "b" ] }
  //    Example: d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee represents { "publisher" => "bob", "publisher-webpage" => "www.example.com", "publisher.location" => "home" }
  //    Example: de represents an empty dictionary {}

  ???
}
