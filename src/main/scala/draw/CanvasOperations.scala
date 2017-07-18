package draw

import draw.CanvasOperations._

case class Canvas(cells: Vector[Vector[Char]])

object CanvasOperations {

  type Row = Vector[Char]
  type Matrix = Vector[Row]

  def printToConsole(canvas: Canvas): Unit =
    canvas.cells.foreach(row =>
      println(row.mkString)
    )

  def updateCanvas(cmd: Command, canvas: Option[Canvas]): Either[CanvasError, Canvas] = cmd match {
    case createCmd: CreateCmd => createCanvas(createCmd)
    case lineCmd: LineCmd     => lineOnCanvas(lineCmd, canvas)
    case _ => Right(Canvas(Vector.empty[Row]))
  }

  private def createCanvas(createCmd: CreateCmd): Either[CanvasError, Canvas] = {
    val borderRow: Row = Vector.fill(createCmd.width)('-')
    val innerRow: Row = ('|' +: Vector.fill(createCmd.width - 2)(' ')) :+ ('|')
    Right(
      Canvas(
        cells = (borderRow +: Vector.fill(createCmd.height)(innerRow)) :+ borderRow
      )
    )
  }

  private def lineOnCanvas(lineCmd: LineCmd, canvas: Option[Canvas]): Either[CanvasError, Canvas] = {
    val eCanvas = canvas.toRight(CanvasError(s"No Canvas presented to draw line on."))
    eCanvas.flatMap(canvas => {
      if (lineCmd.isHorizontal)
        plotHorizontalLine(lineCmd, canvas)
      else if (lineCmd.isVertical)
        plotVerticalLine(lineCmd, canvas)
      else
        Left(CanvasError(s"${lineCmd.orientation} lines not supported."))
      }
    )
  }

  private def plotVerticalLine(lineCmd: LineCmd, canvas: Canvas, pixel: Char = 'X'): Either[CanvasError, Canvas] = {
    if (vLineFits(lineCmd, canvas)) {
      val len = (lineCmd.y2 - lineCmd.y1) + 1
      val updated = canvas.cells.slice(lineCmd.y1, lineCmd.y2 + 1).map(
        row => row.updated(lineCmd.x1, pixel)
      )
      Right(
        Canvas(
          cells = canvas.cells.patch(lineCmd.y1, updated, len)
        )
      )
    }
    else
      Left(CanvasError(s"${lineCmd} line will not fit."))
  }

  private def plotHorizontalLine(lineCmd: LineCmd, canvas: Canvas, pixel: Char = 'X'): Either[CanvasError, Canvas] = {
    if (hLineFits(lineCmd, canvas)) {
      val len = (lineCmd.x2 - lineCmd.x1) + 1
      val updatedRow = canvas.cells(lineCmd.y1).patch(lineCmd.x1, Vector.fill[Char](len)(pixel), len)
      Right(
        Canvas(
          cells = canvas.cells.updated(lineCmd.y1, updatedRow)
        )
      )
    }
    else
      Left(CanvasError(s"${lineCmd} line will not fit."))
  }

  private def hLineFits(lineCmd: LineCmd, canvas: Canvas): Boolean =
    lineCmd.isHorizontal &&
      (lineCmd.y1 > 0 && lineCmd.y1 < canvas.cells.size - 1) &&     // line starting point on y-axis fits
      (lineCmd.x1 > 0 && lineCmd.x2 < canvas.cells.head.size - 1)   // full horizontal line fits

  private def vLineFits(lineCmd: LineCmd, canvas: Canvas): Boolean =
    lineCmd.isVertical &&
      (lineCmd.x1 > 0 && lineCmd.x1 < canvas.cells.head.size - 1) &&  // line starting point on x-axis fits
      (lineCmd.y1 > 0 && lineCmd.y2 < canvas.cells.size - 1)          // full vertical line fits

}
