package draw

trait Command

trait Error

case class CreateCmd(width: Int, height: Int) extends Command

case class LineCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

case class RectangleCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

case class BucketFillCmd(x: Int, y: Int, colour: String) extends Command

case object QuitCmd extends Command

case class CmdError(reason: String) extends Error