package draw

case class Canvas(cells: Vector[Vector[Char]]) {

  def hLineFits(lineCmd: LineCmd): Boolean =
    lineCmd.isHorizontal &&
      (lineCmd.y1 > 0 && lineCmd.y1 < cells.size - 1) && // line starting point on y-axis fits
      (lineCmd.x1 > 0 && lineCmd.x2 < cells.head.size - 1) // full horizontal line fits

  def vLineFits(lineCmd: LineCmd): Boolean =
    lineCmd.isVertical &&
      (lineCmd.x1 > 0 && lineCmd.x1 < cells.head.size - 1) &&  // line starting point on x-axis fits
      (lineCmd.y1 > 0 && lineCmd.y2 < cells.size - 1)          // full vertical line fits

  def rectangleFits(hLineCmd: LineCmd, vLineCmd: LineCmd): Boolean =
    hLineFits(hLineCmd) && vLineFits(vLineCmd)

}

object CanvasOperations {

  type Row = Vector[Char]
  type Matrix = Vector[Row]

  def printToConsole(canvas: Canvas): Unit =
    canvas.cells.foreach(row =>
      println(row.mkString)
    )

  def updateCanvas(cmd: Command, canvas: Option[Canvas]): Either[CanvasError, Canvas] = cmd match {
    case createCmd: CreateCmd       => createCanvas(createCmd)
    case lineCmd: LineCmd           => lineOnCanvas(lineCmd, canvas)
    case rectangleCmd: RectangleCmd => rectangleOnCanvas(rectangleCmd, canvas)
    case _ => Right(Canvas(Vector.empty[Row]))
  }

  private def createCanvas(createCmd: CreateCmd): Either[CanvasError, Canvas] = {
    val borderRow: Row = Vector.fill(createCmd.width + 2)('-')
    val innerRow: Row = ('|' +: Vector.fill(createCmd.width)(' ')) :+ ('|')
    Right(
      Canvas(
        cells = (borderRow +: Vector.fill(createCmd.height)(innerRow)) :+ borderRow
      )
    )
  }

  private def lineOnCanvas(lineCmd: LineCmd, canvas: Option[Canvas]): Either[CanvasError, Canvas] = {
    val eCanvas = canvas.toRight(CanvasError(s"No Canvas presented to draw line on."))
    eCanvas.flatMap(canvas =>
      if (lineCmd.isHorizontal)
        plotHorizontalLine(lineCmd, canvas)
      else if (lineCmd.isVertical)
        plotVerticalLine(lineCmd, canvas)
      else
        Left(CanvasError(s"${lineCmd.orientation} lines not supported."))
    )
  }

  private def rectangleOnCanvas(rectangleCmd: RectangleCmd, canvasOpt: Option[Canvas]): Either[CanvasError, Canvas] = {
    val eCanvas = canvasOpt.toRight(CanvasError(s"No Canvas presented to draw line on."))

    eCanvas.flatMap { canvas =>
      val hLineTop = LineCmd(rectangleCmd.x1, rectangleCmd.y1, rectangleCmd.x2, rectangleCmd.y1)
      val hLineBottom = LineCmd(rectangleCmd.x1, rectangleCmd.y2, rectangleCmd.x2, rectangleCmd.y2)
      val vLineLeft = LineCmd(rectangleCmd.x1, rectangleCmd.y1, rectangleCmd.x1, rectangleCmd.y2)
      val vLineRight = LineCmd(rectangleCmd.x2, rectangleCmd.y1, rectangleCmd.x2, rectangleCmd.y2)

      if (canvas.rectangleFits(hLineTop, vLineLeft)) {
        for {
          canvasLineTop  <- lineOnCanvas(hLineTop, Some(canvas))
          canvasLineBot  <- lineOnCanvas(hLineBottom, Some(canvasLineTop))
          canvasLineLeft <- lineOnCanvas(vLineLeft, Some(canvasLineBot))
          canvasFullRect <- lineOnCanvas(vLineRight, Some(canvasLineLeft))
        } yield canvasFullRect
      }
      else
        Left(CanvasError(s"$rectangleCmd rectangle will not fit."))
    }
  }

  private def plotVerticalLine(lineCmd: LineCmd, canvas: Canvas, pixel: Char = 'X'): Either[CanvasError, Canvas] =
    if (canvas.vLineFits(lineCmd)) {
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

  private def plotHorizontalLine(lineCmd: LineCmd, canvas: Canvas, pixel: Char = 'X'): Either[CanvasError, Canvas] =
    if (canvas.hLineFits(lineCmd)) {
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
