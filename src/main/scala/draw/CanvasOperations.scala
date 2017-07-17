package draw

import draw.CanvasOperations._

case class Canvas private (cells: Matrix)

object CanvasOperations {

  type Row = List[Char]
  type Matrix = List[Row]

  def printToConsole(canvas: Canvas): Unit =
    canvas.cells.foreach(row =>
      println(row.mkString)
    )

  def updateCanvas(cmd: Command, canvas: Option[Canvas]): Either[CanvasError, Canvas] = cmd match {
    case create: CreateCmd => createCanvas(create)
    case _ => Right(Canvas(Nil))
  }

  private def createCanvas(createCmd: CreateCmd): Either[CanvasError, Canvas] = {
    val borderRow: Row = List.fill(createCmd.width)('-')
    val innerRow: Row = ('|' :: List.fill(createCmd.width - 2)(' ')) :+ ('|')
    Right(
      Canvas(
        cells = (borderRow :: List.fill(createCmd.height)(innerRow)) :+ borderRow
      )
    )
  }

}
