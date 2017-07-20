package draw

import scala.util.{Failure, Success, Try}

object CommandParser {

  def buildCommand(line: String): Either[Error, Command] =
    splitCommandParams(line) match {
      case cmd if cmd.head == "Q" => toQuitCommand
      case cmd if cmd.head == "C" => toCreateCommand(cmd.tail)
      case cmd if cmd.head == "L" => toLineCommand(cmd.tail)
      case cmd if cmd.head == "R" => toRectangleCommand(cmd.tail)
      case cmd if cmd.head == "B" => toBucketFillCommand(cmd.tail)
      case _                      => Left(CmdError(s"Command not recognised: $line"))
    }

  private def splitCommandParams(cmdParams: String) = cmdParams.trim.split(" ").toList

  private val parsePF: PartialFunction[Try[Command], Either[Error, Command]] = {
    case Success(cmd)  => Right(cmd)
    case Failure(fail) => Left(CmdError(s"Failed to parse command: ${fail.getMessage}"))
  }

  private def toCreateCommand(tailParams: List[String]): Either[Error, Command] =
    if (tailParams.size != 2)
      Left(CmdError(s"Incorrect number of params following CreateCmd 'C': $tailParams"))
    else
      parsePF(
        Try(
          CreateCmd(
            tailParams(0).toInt,
            tailParams(1).toInt
          )
        )
      )

  private def toLineCommand(tailParams: List[String]): Either[Error, Command] =
    if (tailParams.size != 4)
      Left(CmdError(s"Incorrect number of params following LineCmd 'L': $tailParams"))
    else
      parsePF(
        Try(
          LineCmd(
            tailParams(0).toInt,
            tailParams(1).toInt,
            tailParams(2).toInt,
            tailParams(3).toInt
          )
        )
      )

  private def toRectangleCommand(tailParams: List[String]): Either[Error, Command] =
    if (tailParams.size != 4)
      Left(CmdError(s"Incorrect number of params for RectangleCmd 'R': $tailParams"))
    else
      parsePF(
        Try(
          RectangleCmd(
            tailParams(0).toInt,
            tailParams(1).toInt,
            tailParams(2).toInt,
            tailParams(3).toInt
          )
        )
      )

  private def toBucketFillCommand(tailParams: List[String]): Either[Error, Command] =
    if (tailParams.size != 3)
      Left(CmdError(s"Incorrect number of params for BucketFillCmd 'B': $tailParams"))
    else
      parsePF(
        Try(
          BucketFillCmd(
            tailParams(0).toInt,
            tailParams(1).toInt,
            tailParams(2).toCharArray.head
          )
        )
      )

  private def toQuitCommand = Right(QuitCmd)

}
