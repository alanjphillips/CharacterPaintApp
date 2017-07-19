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