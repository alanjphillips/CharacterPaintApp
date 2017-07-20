package draw

import scala.io.StdIn
import CommandParser._
import CanvasOperations._

import scala.annotation.tailrec

object CommandListener {

  @tailrec
  def readLoop(canvas: Option[Canvas] = None): Unit = {
    val canvasDetails = for {
      cmd <- buildCommand(promptAndRead)
      cvs <- updateCanvas(cmd, canvas)
    } yield CanvasDetails(cvs, cmd)

    canvasDetails match {
      case Left(error: Error) => {
        println(s"Error: ${error.reason}")
        readLoop(canvas)
      }

      case Right(CanvasDetails(_, QuitCmd)) => ()

      case Right(CanvasDetails(canvas, _))  => {
        printToConsole(canvas)
        readLoop(Some(canvas))
      }
    }
  }

  def promptAndRead: String = {
    println("enter command: ")
    StdIn.readLine()
  }

  def printToConsole(canvas: Canvas): Unit =
    canvas.cells.foreach(row =>
      println(row.mkString)
    )

}