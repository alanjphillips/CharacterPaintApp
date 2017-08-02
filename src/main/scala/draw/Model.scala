package draw

trait Command

trait Error {
  def reason: String
}

case class CreateCmd(width: Int, height: Int) extends Command

trait Orientation

case object Horizontal extends Orientation

case object Vertical extends Orientation

case object Diagonal extends Orientation

case class LineCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  def orientation: Orientation =
    if(y1 == y2) Horizontal
    else if (x1 == x2) Vertical
    else Diagonal

  def isHorizontal = orientation == Horizontal
  def isVertical = orientation == Vertical

  def normalize: LineCmd = this match {
    case line if line.isHorizontal => if (x1 > x2) copy(x1 = x2, x2 = x1) else this
    case line if line.isVertical   => if (y1 > y2) copy(y1 = y2, y2 = y1) else this
  }

}

case class RectangleCmd(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

case class FloodFillCmd(x: Int, y: Int, colour: Char) extends Command

case object QuitCmd extends Command

case class CmdError(reason: String) extends Error

case class CanvasError(reason: String) extends Error

case class CanvasDetails(canvas: Canvas, command: Command)