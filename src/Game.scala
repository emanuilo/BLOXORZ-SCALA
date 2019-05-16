import FindingPath._
import FindingPath.State.State
import Main._
import Main.{Block, Map, Position, finishChar, maps, startChar}

import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}
import FindingPath.State._
import FindingPath.{newState, state}

object Game {
  var gameState: State = I
  var specPlate1: Option[(Int, Int)] = None
  var specPlate2: Option[(Int, Int)] = None

  def playMove(direction: () => Char, block: Block, map: Map): String = {
    val dir = direction()
    val row_col = getCoordsTuple(dir)
    if (row_col.isEmpty) {
      println("Pogresna komanda!")
      return playMove(direction, block, map)
    }
    val row = row_col.get._1
    val col = row_col.get._2
    val finishPos = getFinishPosition(map)

    block match {
      case Block(pos1, None) => // block is standing upright
        if ( map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-' ||  // game over if block gets out of the map
          map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos1.x + 2 * row)(pos1.y) == '-' )
          "Fail \n"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos1.x + 2 * row, pos1.y + 2 * col)
          map.map(pos1.x + row)(pos1.y + col) = 'X'                               // replace char with 'X' at new block position
          map.map(pos1.x + 2 * row)(pos1.y + 2 * col) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish position if it's been overwritten
          printMap(map)

          val newBlock = if(row == -1 || col == -1)  // sorting which position is first in Block object
            Block(Position(pos1.x + 2 * row, pos1.y + 2 * col), Some(Position(pos1.x + row, pos1.y + col)))
          else
            Block(Position(pos1.x + row, pos1.y + col), Some(Position(pos1.x + 2 * row, pos1.y + 2 * col)))

          gameState = newState(gameState, dir)
          playMove(direction, newBlock, map)
        }
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x => // block is lying horizontally
        // ovde sam promenio pos1.y + col
        if (map.map(pos1.x + row)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-' || (
            dir == left && map.map(pos1.x)(pos1.y + col) == specPlateChar) || (
            dir == right && map.map(pos2.x)(pos2.y + col) == specPlateChar))      // game over if block gets out of the map or steps on a spec plate upright
          "Fail \n"
        else if (map.map(pos1.x)(pos1.y + col) == 'T' && col == -1 || map.map(pos2.x)(pos2.y + col) == 'T' && col == 1) "Win \n"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          rewriteOldSpec(map)                                                     // rewrite spec plates if they have been replaced by the block
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos2.x + row, pos2.y + col)
          if(col == -1)
            map.map(pos1.x + row)(pos1.y + col) = 'X'                             // replace char with 'X' at new block position
          else
            map.map(pos2.x + row)(pos2.y + col) = 'X'                             // replace char with 'X' at new block position
          if (row != 0) map.map(pos1.x + row)(pos1.y) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish if it's been overwritten
          printMap(map)

          val newBlock = if (col == -1) Block(Position(pos1.x, pos1.y + col), None) // sorting which position is first in Block object
          else if (col == 1) Block(Position(pos2.x, pos2.y + col), None)
          else Block(Position(pos1.x + row, pos1.y), Some(Position(pos2.x + row, pos2.y)))

          gameState = newState(gameState, dir)
          playMove(direction, newBlock, map)
        }
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x => // block is lying vertically
        //ovde sam promenio pos1.x + row
        if (map.map(pos1.x + row)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-' || (
            dir == up && map.map(pos1.x + row)(pos1.y) == specPlateChar) || (
            dir == down && map.map(pos2.x + row)(pos2.y) == specPlateChar))       // game over if block gets out of the map or steps on a spec plate upright
          "Fail \n"
        else if (map.map(pos1.x + row)(pos1.y) == 'T' && row == -1 || map.map(pos2.x + row)(pos2.y) == 'T' && row == 1) "Win \n"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          map.map(pos2.x) = map.map(pos2.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          rewriteOldSpec(map)                                                     // rewrite spec plates if they have been replaced by the block
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos2.x + row, pos2.y + col)
          if (row == -1)
            map.map(pos1.x + row)(pos1.y + col) = 'X'                             // replace char with 'X' at new block position
          else
            map.map(pos2.x + row)(pos2.y + col) = 'X'                             // replace char with 'X' at new block position
          if (col != 0) map.map(pos1.x)(pos1.y + col) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish if it's been overwritten
          printMap(map)

          val newBlock = if (row == 1) Block(Position(pos2.x + row, pos2.y), None) // sorting which position is first in Block object
          else if (row == -1) Block(Position(pos1.x + row, pos1.y), None)
          else Block(Position(pos1.x, pos1.y + col), Some(Position(pos2.x, pos2.y + col)))

          gameState = newState(gameState, dir)
          playMove(direction, newBlock, map)
        }
    }
  }

  def saveOldSpec(map: Map, x1: Int, y1: Int, x2: Int, y2: Int) = {
    if (map.map(x1)(y1) == specPlateChar) specPlate1 = Some((x1, y1)) else specPlate1 = None
    if (map.map(x2)(y2) == specPlateChar) specPlate2 = Some((x2, y2)) else specPlate2 = None
  }

  def rewriteOldSpec(map: Map) = {
    specPlate1 match {
      case Some((x, y)) => map.map(x)(y) = specPlateChar
      case None =>
    }
    specPlate2 match {
      case Some((x, y)) => map.map(x)(y) = specPlateChar
      case None =>
    }
  }

  def inputFileName(): String = {
    println("Uneti ime fajla: \n")
    StdIn.readLine()
  }

  def getFileLineIterator(file: String): Iterator[String] = {
    Source.fromFile(file).getLines()
  }

  def getCoordsTuple(ch: Char): Option[(Int, Int)] = {
    ch match {
      case 'r' => Some((0, 1))
      case 'l' => Some((0, -1))
      case 'd' => Some((1, 0))
      case 'u' => Some((-1, 0))
      case _ => None
    }
  }

  def printMap(map: Map) {
    for(line <- map.map) println(line.mkString(""))
    println("\n")
  }

  def getFinishPosition = getPosition(finishChar)

  def getStartPosition = getPosition(startChar)

  def getPosition(char: Char): Map => (Int, Int) = {
    map => (for ((line, lineIndex) <- map.map.zipWithIndex; (ch, chIndex) <- line.zipWithIndex if ch == char)
      yield (lineIndex, chIndex)).head
  }

  def inputPlayerMove: Char = {
    println("Uneti potez: \n")
    StdIn.readChar()
  }

  def initBlockPosition(map: Map): Block = {
    (for ((line, lineIndex) <- map.map.zipWithIndex; (ch, chIndex) <- line.zipWithIndex if ch == 'S')
      yield Block(Position(lineIndex, chIndex), None)).head
  }

  def getMap(number: Int, list: List[Map]): Option[Map] = {
    list match {
      case List() => None
      case h :: t if number > 0 => getMap(number - 1, t)
      case h :: t if number == 0 => Some(h)
    }
  }

  def inputMapNumber(): Int = {
    def printMaps() {
      for ((map, index) <- maps.zipWithIndex) {
        println(s"Map $index:")
        for(line <- map.map)
          println(line.mkString(""))
        println("\n")
      }
    }
    printMaps()
    println("Izaberi mapu: ")
    StdIn.readInt()
  }

  def readMapFromFile(path: String): Option[Map] ={
    val map: ArrayBuffer[Array[Char]] = ArrayBuffer()
    try{
      for (line <- Source.fromFile(path).getLines()) {
        map += line.toCharArray
      }
      Some(Map(map))
    }
    catch {
      case _: Exception => None
    }
  }

  def getMapFilePath: String = {
    println("Unesi putanju: ")
    StdIn.readLine()
  }
}
