import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}

object Main {
  var maps: List[Map] = List()

  val menuStr: String = "1. Ucitaj mapu \n" +
    "2. Zapocni igru \n" +
    "3. Kreiranje mapa \n" +
    "4. Kraj \n" +
    "Uneti izbor: "

  val mapCreating: String = "1. Uklanjanje zadate ploce sa ivice terena \n" +
    "2. Dodavanje ploce na zadatu poziciju na ivici terena \n" +
    "3. Zamena obicne ploce na zadatoj poziciji specijalnom \n" +
    "4. Zamena specijalne ploce na zadatoj poziciji obicnom \n" +
    "5. Postavljanje startne pozicije na zadato polje \n" +
    "6. Postavljanje ciljne pozicije na zadato polje \n" +
    "7. Pravljenje imenovane kompozitne operacije \n" +
    "8. "

  case class Map(map: ArrayBuffer[Array[Char]])
  case class Block(position1: Position, position2: Option[Position])
  case class Position(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val input = menu

    input match {
      case 1 =>
        readMapFromFile(getMapFilePath) match {
          case None => println("Neuspesno ucitavanje mape!")
          case Some(x) => maps = x :: maps
        }
      case 2 =>
        val map = getMap(inputMapNumber(), maps)
        map match {
          case None => println("Pogresan unos!")
          case Some(_map) =>
            val copyMap = Map(_map.map.map(_.clone()))
            println(playMove(inputPlayerMove, initBlockPosition(copyMap), copyMap))
        }
      case 3 =>

      case 4 =>
      case _ => "Pogresan unos!"
    }

    if(input != 4) main(args)
  }

  def playMove(direction: Char, block: Block, map: Map): String = {
    val row_col = getCoordsTuple(direction)
    if (row_col.isEmpty) {
      println("Pogresna komanda!")
      return playMove(inputPlayerMove, block, map)
    }
    val row = row_col.get._1
    val col = row_col.get._2
    val finishPos = getFinishPosition(map)

    block match {
      case Block(pos1, None) => // block is standing upright
        if ( map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-' ||  // game over if block gets out of the map
          map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos1.x + 2 * row)(pos1.y) == '-' )
          "Fail"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          map.map(pos1.x + row)(pos1.y + col) = 'X'                               // replace char with 'X' at new block position
          map.map(pos1.x + 2 * row)(pos1.y + 2 * col) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish position if it's been overwritten
          printMap(map)

          val newBlock = if(row == -1 || col == -1)  // sorting which position is first in Block object
            Block(Position(pos1.x + 2 * row, pos1.y + 2 * col), Some(Position(pos1.x + row, pos1.y + col)))
          else
            Block(Position(pos1.x + row, pos1.y + col), Some(Position(pos1.x + 2 * row, pos1.y + 2 * col)))

          playMove(inputPlayerMove, newBlock, map)
        }
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x => // block is lying horizontally
        if (map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') // game over if block gets out of the map
          "Fail"
        else if (map.map(pos1.x)(pos1.y + col) == 'T' && col == -1 || map.map(pos2.x)(pos2.y + col) == 'T' && col == 1) "Win"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
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

          playMove(inputPlayerMove, newBlock, map)
        }
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x => // block is lying vertically
        if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') // game over if block gets out of the map
          "Fail"
        else if (map.map(pos1.x + row)(pos1.y) == 'T' && row == -1 || map.map(pos2.x + row)(pos2.y) == 'T' && row == 1) "Win"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
          map.map(pos2.x) = map.map(pos2.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
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

          playMove(inputPlayerMove, newBlock, map)
        }
    }
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

  def getFinishPosition(map: Map): (Int, Int) = {
    (for ((line, lineIndex) <- map.map.zipWithIndex; (ch, chIndex) <- line.zipWithIndex if ch == 'T')
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

  def menu: Int = {
    println(menuStr)
    scala.io.StdIn.readInt()
  }

}
