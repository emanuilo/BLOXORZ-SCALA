import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}

object Main {
  var maps: List[Map] = List()

  val menuStr: String = "1. Ucitaj mapu \n" +
    "2. Zapocni igru \n" +
    "3. Kreiranje mapa \n" +
    "4. Kraj \n" +
    "Uneti izbor: "

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
          case Some(_map) => println(playMove(inputPlayerMove, initBlockPosition(_map), _map))
        }
      case 3 =>
      case 4 =>
      case _ => "Pogresan unos!"
    }

    if(input != 4) main(args)
  }

//  Maps.printMaps()
//  println("Izaberi mapu: ")
//  val mapNumber = StdIn.readInt()
//  val map = Maps.getMap(mapNumber)
//  map match {
//    case None => println("Pogresan unos!")
//    case Some(x) =>
//  }

  def initBlockPosition(map: Map): Block = {
    (for ((line, lineIndex) <- map.map.zipWithIndex; (ch, chIndex) <- line.zipWithIndex if ch == 'S')
      yield Block(Position(lineIndex, chIndex), None)).head
  }

  def getCoordsTuple(ch: Char): (Int, Int) = {
    ch match {
      case 'r' => (0, 1)
      case 'l' => (0, -1)
      case 'd' => (1, 0)
      case 'u' => (-1, 0)
    }
  }

  def printMap(map: Map) {
    for(line <- map.map) println(line.mkString(""))
    println("\n")
  }

  def playMove(direction: Char, block: Block, map: Map): String = {
    val row_col = getCoordsTuple(direction)
    val row = row_col._1
    val col = row_col._2

    block match {
      case Block(pos1, None) =>
        if ( map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-' ||
          map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos1.x + 2 * row)(pos1.y) == '-' )
          "Fail"
        else {
          map.map(pos1.x).map(ch => if (ch == 'X') '.' else ch)             // replace old block position with '.'
          map.map(pos1.x + row).patch(pos1.y + col, Array('X'), 1)          // replace char with 'X' at new block position
          map.map(pos1.x + 2 * row).patch(pos1.y + 2 * col, Array('X'), 1)  // replace char with 'X' at new block position
          printMap(map)

          playMove(inputPlayerMove, Block(Position(pos1.x + row, pos1.y + col), Some(Position(pos1.x + 2 * row, pos1.y + 2 * col))), map)
        }
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x =>
        if (map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-')
          "Fail"
        else if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-') ""
        else ""
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x =>
        if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-')
          "Fail"
        else if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-') ""
        else ""
    }
  }

  def inputPlayerMove: Char = {
    println("Uneti potez: \n")
    StdIn.readChar()
  }

  def getMap(number: Int, list: List[Map]): Option[Map] = {
    list match {
      case List() => None
      case h :: t if number > 1 => getMap(number - 1, t)
      case h :: t if number == 1 => Some(h)
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
      case Exception => None
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
