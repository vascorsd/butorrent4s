// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

package butorrent4s

import cats.data.Validated
import cats.syntax.monoid.*
import cats.effect.*
import cats.effect.std.Console
import com.monovore.decline.*
import fs2.io.*

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    val program = Program.description.parse(args)

    program match
      case Left(help) =>
        val exitCode = if (help.errors.nonEmpty) {
          ExitCode.Error
        } else {
          ExitCode.Success
        }

//        Console[IO].errorln("pooper: ") *>
        Console[IO]
          .errorln(help)
          .as(exitCode)

      case Right(v) =>
        v match
          case Program.Version =>
            IO.println("Version 10").as(ExitCode.Success)

          case Program.Decode(input) =>
            val source = input match
              case "-" => fs2.io.stdinUtf8[IO](1024)
              case _   => fs2.Stream.emit(input)

            val r = source
              .map(decode)
              .evalMap {
                case Some((v @ Bencode.BString(parsed), remaining)) =>
                  IO.println(s"toString: ${v}") *>
                    IO.println(s"Decoded value: ${String(parsed)}") *>
                    IO.println(
                      s"Remaining unparsed input: ${String(remaining)}"
                    )

                case Some((v @ Bencode.BInteger(parsed), remaining)) =>
                  IO.println(s"toString: ${v}") *>
                    IO.println(s"Decoded value: ${parsed}") *>
                    IO.println(
                      s"Remaining unparsed input: ${String(remaining)}"
                    )

                case Some((v @ Bencode.BList(parsed), remaining)) =>
                  IO.println(s"toString: ${v}") *>
                    IO.println(s"Decoded value: ${parsed}") *>
                    IO.println(
                      s"Remaining unparsed input: ${String(remaining)}"
                    )

                case Some((v @ Bencode.BDictionary(parsed), remaining)) =>
                  IO.println(s"toString: ${v}") *>
                    IO.println(s"Decoded value: ${parsed}") *>
                    IO.println(
                      s"Remaining unparsed input: ${String(remaining)}"
                    )

                case None =>
                  IO.println("Couldn't decode given input")
              }
              .compile
              .drain

            r.as(ExitCode.Success)

          case Program.Encode(input) =>
            // encode(input)

            IO.println("Encoding...").as(ExitCode.Success)

}

enum Program {
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
