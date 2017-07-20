package draw

import org.scalatest.{Matchers, WordSpec}


class ModelSpec extends WordSpec with Matchers {

  "LineCmd" should {

    "return line orientation as horizontal" in {
      val hline = LineCmd(1, 1, 3, 1)
      hline.orientation shouldBe Horizontal
    }

    "return line orientation as vertical" in {
      val hline = LineCmd(1, 1, 1, 3)
      hline.orientation shouldBe Vertical
    }

    "return horizontal as true" in {
      val hline = LineCmd(1, 1, 3, 1)
      hline.isHorizontal shouldBe true
      hline.isVertical shouldBe false
    }

    "return vertical as true" in {
      val vline = LineCmd(1, 1, 1, 3)
      vline.isVertical shouldBe true
      vline.isHorizontal shouldBe false
    }

    "reverse vertical y coordinates if first point is higher than second point" in {
      val vline = LineCmd(1, 1, 1, 3)
      val vlineReversed = LineCmd(1, 3, 1, 1)
      val vlineInOrder = LineCmd(1, 3, 1, 1)

      vlineReversed.normalize shouldBe vline
      vlineInOrder.normalize shouldBe vline
    }

    "reverse horizontal x coordinates if first point is higher than second point" in {
      val hline = LineCmd(1, 1, 3, 1)
      val hlineReversed = LineCmd(3, 1, 1, 1)
      val hlineInOrder = LineCmd(1, 1, 3, 1)

      hlineReversed.normalize shouldBe hline
      hlineInOrder.normalize shouldBe hline
    }

  }

}
