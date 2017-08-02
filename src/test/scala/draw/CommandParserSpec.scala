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

    "return CmdError for incorrect number of params for a Create formatted String" in {
      val input = "C 20 4 5"
      buildCommand(input) shouldBe Left(CmdError(s"Incorrect number of params following CreateCmd 'C': List(20, 4, 5)"))
    }

    "build a LineCommand given a Line formatted String" in {
      val input = "L 1 2 6 2"
      val expected = LineCmd(1, 2, 6, 2)
      buildCommand(input) shouldBe Right(expected)
    }

    "return CmdError for incorrect number of params for a Line formatted String" in {
      val input = "L 1"
      buildCommand(input) shouldBe Left(CmdError(s"Incorrect number of params following LineCmd 'L': List(1)"))
    }

    "build a RectangleCommand given a Rectangle formatted String" in {
      val input = "R 16 1 20 3"
      val expected = RectangleCmd(16, 1, 20, 3)
      buildCommand(input) shouldBe Right(expected)
    }

    "return CmdError for incorrect number of params for a Rectangle formatted String" in {
      val input = "R 16 1 20"
      buildCommand(input) shouldBe Left(CmdError(s"Incorrect number of params for RectangleCmd 'R': List(16, 1, 20)"))
    }

    "build a FloodFillCommand given a FloodFill formatted String" in {
      val input = "F 10 3 o"
      val expected = FloodFillCmd(10, 3, 'o')
      buildCommand(input) shouldBe Right(expected)
    }

    "return CmdError for incorrect number of params for a FloodFill formatted String" in {
      val input = "F 10 3"
      buildCommand(input) shouldBe Left(CmdError(s"Incorrect number of params for FloodFillCmd 'F': List(10, 3)"))
    }

    "return a CmdError for an unknown input Command String" in {
      val input = "Z"
      buildCommand(input) shouldBe Left(CmdError(s"Command not recognised: $input"))
    }

  }

}
