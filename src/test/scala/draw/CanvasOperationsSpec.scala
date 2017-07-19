package draw

import org.scalatest.{Matchers, WordSpec}
import draw.CanvasOperations._
import CanvasOperationsSpec._

class CanvasOperationsSpec extends WordSpec with Matchers {

  "CanvasOperations" should {

    "create a Canvas" in {
      val createCmd = CreateCmd(3, 3)
      updateCanvas(createCmd, None) shouldBe Right(validCanvas)
    }

    "draw a horizontal line on blank Canvas" in {
      val horizontalLineCmd = LineCmd(1, 1, 3, 1)
      updateCanvas(horizontalLineCmd, Some(validCanvas)) shouldBe Right(validCanvasHorizontalLine)
    }

    "fail to fit horizontal line on blank Canvas" in {
      val horizontalLineCmd = LineCmd(1, 1, 13, 1)
      updateCanvas(horizontalLineCmd, Some(validCanvas)) shouldBe Left(CanvasError(s"${horizontalLineCmd} line will not fit."))
    }

    "draw a vertical line on blank Canvas" in {
      val verticalLineCmd = LineCmd(2, 1, 2, 3)
      updateCanvas(verticalLineCmd, Some(validCanvas)) shouldBe Right(validCanvasVerticalLine)
    }

    "fail to fit vertical line on blank Canvas" in {
      val verticalLineCmd = LineCmd(2, 1, 2, 13)
      updateCanvas(verticalLineCmd, Some(validCanvas)) shouldBe Left(CanvasError(s"${verticalLineCmd} line will not fit."))
    }

    "draw a rectangle on blank Canvas" in {
      val rectangleCmd = RectangleCmd(1, 1, 3, 3)
      updateCanvas(rectangleCmd, Some(validCanvas)) shouldBe Right(validCanvasRectangle)
    }

    "fail to fit rectangle on blank Canvas" in {
      val rectangleCmd = RectangleCmd(1, 1, 9, 9)
      updateCanvas(rectangleCmd, Some(validCanvas)) shouldBe Left(CanvasError(s"${rectangleCmd} rectangle will not fit."))
    }

    "draw a horizontal and vertical line on blank Canvas" in {
      val horizontalLineCmd = LineCmd(1, 1, 3, 1)
      val verticalLineCmd = LineCmd(2, 1, 2, 3)

      val resultCanvas = for {
        canvasHorizontal   <- updateCanvas(horizontalLineCmd, Some(validCanvas))
        canvasHorizAndVert <- updateCanvas(verticalLineCmd, Some(canvasHorizontal))
      } yield canvasHorizAndVert

      resultCanvas shouldBe Right(validCanvasHorizontalAndVertical)
    }

    "will not apply unknown Command" in {
      updateCanvas(UnknownCmd, None) shouldBe Right(Canvas(Vector.empty[Row]))
    }

  }

}

object CanvasOperationsSpec {
  case object UnknownCmd extends Command

  def validCanvas = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

  def validCanvasHorizontalLine = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', 'X', 'X', 'X', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('|', ' ', ' ', ' ', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

  def validCanvasVerticalLine = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', ' ', 'X', ' ', '|'),
      Vector('|', ' ', 'X', ' ', '|'),
      Vector('|', ' ', 'X', ' ', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

  def validCanvasRectangle = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', 'X', 'X', 'X', '|'),
      Vector('|', 'X', ' ', 'X', '|'),
      Vector('|', 'X', 'X', 'X', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

  def validCanvasHorizontalAndVertical = Canvas(
    cells = Vector(
      Vector('-', '-', '-', '-', '-'),
      Vector('|', 'X', 'X', 'X', '|'),
      Vector('|', ' ', 'X', ' ', '|'),
      Vector('|', ' ', 'X', ' ', '|'),
      Vector('-', '-', '-', '-', '-')
    )
  )

}