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

  }

}
