package draw

object CommandParser {

  def buildCommand(line: String): Either[Error, Command] =
    splitCommandParams(line) match {
      case cmd if cmd.head == "Q" => toQuitCommand
      case cmd if cmd.head == "C" => toCreateCommand(cmd.tail)
      case cmd if cmd.head == "L" => toLineCommand(cmd)
      case cmd if cmd.head == "R" => toRectangleCommand(cmd)
      case cmd if cmd.head == "B" => toBucketFillCommand(cmd)
      case cmd if cmd.isEmpty     => Left(CmdError(s"Empty Command"))
      case unknown                => Left(CmdError(s"Command not recognised: $unknown"))
    }

  private def splitCommandParams(cmdParams: String) = cmdParams.trim.split(" ").toList

  private def toCreateCommand(tailParams: List[String]): Either[Error, CreateCmd] =
    if (tailParams.size != 2)
      Left(CmdError(s"Incorrect number of params following CreateCmd 'C': $tailParams"))
    else
      Right(
        CreateCmd(
          tailParams(0).toInt,
          tailParams(1).toInt
        )
      )

  private def toLineCommand(tailParams: List[String]): Either[Error, LineCmd] =
    if (tailParams.size != 4)
      Left(CmdError(s"Incorrect number of params following LineCmd 'L': $tailParams"))
    else
      Right(
        LineCmd(
          tailParams(0).toInt,
          tailParams(1).toInt,
          tailParams(2).toInt,
          tailParams(3).toInt
        )
      )

  private def toRectangleCommand(tailParams: List[String]): Either[Error, RectangleCmd] =
    if (tailParams.size != 4)
      Left(CmdError(s"Incorrect number of params for RectangleCmd 'R': $tailParams"))
    else
      Right(
        RectangleCmd(
          tailParams(0).toInt,
          tailParams(1).toInt,
          tailParams(2).toInt,
          tailParams(3).toInt
        )
      )

  private def toBucketFillCommand(tailParams: List[String]): Either[Error, BucketFillCmd] =
    if (tailParams.size != 3)
      Left(CmdError(s"Incorrect number of params for BucketFillCmd 'B': $tailParams"))
    else
      Right(
        BucketFillCmd(
          tailParams(0).toInt,
          tailParams(1).toInt,
          tailParams(2)
        )
      )

  private def toQuitCommand = Right(QuitCmd)

}
