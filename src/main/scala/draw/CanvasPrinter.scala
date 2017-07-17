package draw

object CanvasPrinter {

  def printToConsole(canvas: Canvas): Unit = {
    canvas.cells.foreach(row =>
      println(row.mkString)
    )
  }

}
