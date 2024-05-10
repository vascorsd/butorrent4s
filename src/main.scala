import cats.effect.*


object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    if (args.nonEmpty) {
      IO.println("I did it!").as(ExitCode.Success)
    } else {
      IO.println("Didn't do it").as(ExitCode(-1))
    }
}
