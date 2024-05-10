// SPDX-FileCopyrightText: 2024 Vasco Dias <m+code@vascorsd.com>
// SPDX-License-Identifier: AGPL-3.0-or-later

import cats.effect.*

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    if (args.nonEmpty) {
      IO.println("I did it!")
        .as(ExitCode.Success)
    } else {
      IO.println("Didn't do it")
        .as(ExitCode(-1))
    }
}
