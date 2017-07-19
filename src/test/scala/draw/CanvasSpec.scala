package draw

import org.scalatest.{Matchers, WordSpec}
import CanvasSpec._

class CanvasSpec  extends WordSpec with Matchers {

  "Canvas" should {

    "be instantiated" in {
      val canvas = Canvas(
        cells = Vector(
          Vector('-', '-', '-', '-', '-'),
          Vector('|', ' ', ' ', ' ', '|'),
          Vector('|', ' ', ' ', ' ', '|'),
          Vector('|', ' ', ' ', ' ', '|'),
          Vector('-', '-', '-', '-', '-')
        )
      )
      canvas.cells.size shouldBe 5
      canvas.cells.head.size shouldBe 5
      canvas.cells.last.size shouldBe 5
    }

    "have a horizontal line fit" in {
      val canvas = validCanvas
      val line = LineCmd(1, 1, 3, 1)
      canvas.hLineFits(line) shouldBe true
    }

    "fail a horizontal line fit" in {
      val canvas = validCanvas
      val line = LineCmd(1, 1, 13, 1)
      canvas.hLineFits(line) shouldBe false
    }

    "have a vertical line fit" in {
      val canvas = validCanvas
      val line = LineCmd(2, 1, 2, 3)
      canvas.vLineFits(line) shouldBe true
    }

    "fail a vertical line fit" in {
      val canvas = validCanvas
      val line = LineCmd(2, 1, 2, 6)
      canvas.vLineFits(line) shouldBe false
    }

    "have a rectangle fit" in {
      val canvas = validCanvas
      val hline = LineCmd(1, 1, 3, 1)
      val vline = LineCmd(1, 1, 1, 3)
      canvas.rectangleFits(hline, vline) shouldBe true
    }

    "fail a rectangle fit" in {
      val canvas = validCanvas
      val hline = LineCmd(1, 1, 3, 1)
      val vline = LineCmd(1, 1, 1, 9)
      canvas.rectangleFits(hline, vline) shouldBe false
    }

  }

}

object CanvasSpec {

  def validCanvas = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

}