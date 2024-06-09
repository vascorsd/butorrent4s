// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import cats.data.Validated
import cats.effect.*
import cats.effect.std.Console
import cats.syntax.monoid.*
import fs2.io.*
import scodec.bits.ByteVector

import com.monovore.decline.*

object Main extends IOApp {

   def run(args: List[String]): IO[ExitCode] = {
      val program = Program.description.parse(args)

      program match {
         case Left(help) =>
            val exitCode = if help.errors.nonEmpty then {
               ExitCode.Error
            } else {
               ExitCode.Success
            }

            //        Console[IO].errorln("pooper: ") *>
            Console[IO]
               .errorln(help)
               .as(exitCode)

         case Right(v) =>
            v match {
               case Program.Version =>
                  IO.println("Version 10").as(ExitCode.Success)

               case Program.Decode(input) =>
                  val source = input match {
                     case "-" => fs2.io.stdin[IO](10)
                     case _   => fs2.Stream.emit(input).through(fs2.text.utf8.encode)
                  }

                  source.compile
                     .to(ByteVector)
                     .map(decode)
                     .flatMap {
                        case Right(v @ Bencode.BString(parsed), remaining, lll) =>
                           IO.println(s"toString: ${v}") *>
                              IO.println(s"Decoded value raw: ${parsed}") *>
                              IO.println(s"Decoded value utf8: ${parsed.decodeUtf8Lenient}") *>
                              IO.println(s"Remaining unparsed input: ${remaining}")

                        case Right(v @ Bencode.BInteger(parsed), remaining, lll) =>
                           IO.println(s"toString: ${v}") *>
                              IO.println(s"Decoded value: ${parsed}") *>
                              IO.println(s"Remaining unparsed input: ${remaining}")

                        case Right(v @ Bencode.BList(parsed), remaining, lll) =>
                           IO.println(s"toString: ${v}") *>
                              IO.println(s"Decoded value: ${parsed}") *>
                              IO.println(s"Remaining unparsed input: ${remaining}")

                        case Right(v @ Bencode.BDictionary(parsed), remaining, lll) =>
                           IO.println(s"toString: ${v}") *>
                              IO.println(s"Decoded value: ${parsed}") *>
                              IO.println(s"Remaining unparsed input: ${remaining}")

                        case Left(error) =>
                           IO.println("Couldn't decode given input") *>
                              IO.println(error)
                     }
                     .as(ExitCode.Success)

               case Program.Encode(input) =>
                  val x = fs2.Stream
                     .emits(encode(input).toSeq)
                     .through(fs2.io.stdout[IO])
                     .compile
                     .drain

                  x.as(ExitCode.Success)
//                  IO.println(x) *>
//                     IO.println("Encoding...").as(ExitCode.Success)
            }
      }
   }
}

enum Program derives CanEqual {
   case Version
   case Decode(input: String)
   case Encode(input: String)
}

object Program {
   def description = Command[Program](
     name = "butorrent4s",
     header = "Experiment with bitorrent protocol details",
     helpFlag = false
   )(fullProgram)

   def helpFlag: Opts[Nothing] =
      Opts
         .flag(
           help = "Print help",
           long = "help",
           short = "h",
           visibility = Visibility.Partial
         )
         .asHelp

   def versionFlag: Opts[Program] =
      Opts
         .flag(
           help = "Print version",
           long = "version",
           short = "V",
           visibility = Visibility.Partial
         )
         .map(_ => Program.Version)

   def encodeCommand =
      Command(
        name = "encode",
        header = "Encode Bencoded data",
        helpFlag = false
      ) {
         helpFlag
            .orElse(Opts.argument[String]("INPUT"))
      }.map(Program.Encode(_))

   def decodeCommand =
      Command(
        name = "decode",
        header = "Decode Bencoded data",
        helpFlag = false
      ) {
         helpFlag
            .orElse(Opts.argument[String]("INPUT"))
      }.map(Program.Decode(_))

   def fullProgram: Opts[Program] =
      versionFlag
         .orElse(helpFlag)
         .orElse(
           Opts.subcommands(
             encodeCommand,
             decodeCommand
           )
         )

}
