package draw

import draw.Canvas.Matrix

case class Canvas private (cells: Matrix)

object Canvas {

  type Row = List[Char]
  type Matrix = List[Row]

  def apply(cmd: Command): Canvas = cmd match {
    case create: CreateCmd => createCanvas(create)
  }

//  def apply(cmd: Command, canvas: Canvas): Canvas = cmd match {
//    case create: CreateCmd => createCanvas(create)
//  }

  private def createCanvas(createCmd: CreateCmd): Canvas = {
    val borderRow = List.fill(createCmd.width)('-')
    val innerRow = ('|' :: List.fill(createCmd.width - 2)(' ')) :+ ('|')
    Canvas(
      cells = (borderRow :: List.fill(createCmd.height)(innerRow)) :+ borderRow
    )
  }

}
