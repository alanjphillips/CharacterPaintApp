package draw

import scala.io.StdIn
import CommandParser._

import scala.annotation.tailrec

object CommandListener {

  def readLoop(): Unit = {
    buildCommand(promptAndRead) match {
      case Right(c) => CanvasPrinter.printToConsole(Canvas(c))
      case Left(e) => println(s"There was a problem with the command: $e")
    }
    readLoop()
  }


  def readLoop2(): Unit = {
    buildCommand(promptAndRead) match {
      case Right(c) => CanvasPrinter.printToConsole(Canvas(c))
      case Left(e) => println(s"There was a problem with the command: $e")
    }
    readLoop()
  }



  def promptAndRead: String = {
    println("enter command: ")
    StdIn.readLine()
  }

  def isNotQuit(cmd: Command) = !(cmd equals QuitCmd)

}
