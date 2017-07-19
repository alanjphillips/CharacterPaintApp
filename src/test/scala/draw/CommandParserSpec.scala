package draw

import org.scalatest.{Matchers, WordSpec}
import draw.CommandParser._

class CommandParserSpec extends WordSpec with Matchers {

  "CommandParser" should {

    "build a QuitCommand given a Quit formatted String" in {
      val input = "Q"
      buildCommand(input) shouldBe Right(QuitCmd)
    }

    "build a CreateCommand given a Create formatted String" in {
      val input = "C 20 4"
      val expected = CreateCmd(20, 4)
      buildCommand(input) shouldBe Right(expected)
    }

    "build a LineCommand given a Line formatted String" in {
      val input = "L 1 2 6 2"
      val expected = LineCmd(1, 2, 6, 2)
      buildCommand(input) shouldBe Right(expected)
    }

    "build a RectangleCommand given a Rectangle formatted String" in {
      val input = "R 16 1 20 3"
      val expected = RectangleCmd(16, 1, 20, 3)
      buildCommand(input) shouldBe Right(expected)
    }

    "build a BucketFillCommand given a BucketFill formatted String" in {
      val input = "B 10 3 o"
      val expected = BucketFillCmd(10, 3, 'o')
      buildCommand(input) shouldBe Right(expected)
    }

    "return a CmdError for an unknown input Command String" in {
      val input = "Z"
      buildCommand(input) shouldBe Left(CmdError(s"Command not recognised: $input"))
    }

  }

}
