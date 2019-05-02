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

  def moveBlock(direction: Char, block: Block, map: Map): Unit = {
    direction match {
      case 'd' => {
        // check if block falls outside
        // update block fields
        // update map
        // print new map state or if game finished
        // if game not finished read another player move
        // call moveBlock
      }
      case 'u' =>
      case 'l' =>
      case 'r' =>
    }
  }

  def playMove(direction: Char, block: Block, map: Map): String = {
    direction match {
      case 'd' => {
//        block match {
//          case Block(pos1, None) =>
//            if (map.map(pos1.x + 1)(pos1.y) == '-' || map.map(pos1.x + 2)(pos1.y) == '-') "Zavrsena igra!"
//            else ""
//          case Block(pos1, Some(pos2)) if pos1.x == pos2.x =>
//            if (map.map(pos1.x + 1)(pos1.y) == '-' || map.map(pos2.x + 1)(pos2.y) == '-') "Zavrsena igra!"
//            else ""
//          case Block(pos1, Some(pos2)) if pos1.x != pos2.x =>
//            if (map.map(pos2.x + 1)(pos2.y) == '-') "Zavrsena igra!"
//            else ""
//        }

        checkNewPosition(0, 1, block, map)
      }
      case 'u' => {

      }
      case 'l' => {

      }
      case 'r' => {
        // check if block falls outside
        // update block fields
        // update map
        // print new map state or if game finished
        // if game not finished read another player move
        // call moveBlock

//        block match {
//          case Block(pos1, None) =>
//            if (map.map(pos1.x)(pos1.y + 1) == '-' || map.map(pos1.x)(pos1.y + 2) == '-') "Zavrsena igra!"
//            else ""
//          case Block(pos1, Some(pos2)) if pos1.x == pos2.x =>
//            if (map.map(pos2.x)(pos2.y + 1) == '-') "Zavrsena igra!"
//            else ""
//          case Block(pos1, Some(pos2)) if pos1.x != pos2.x =>
//            if (map.map(pos1.x)(pos1.y + 1) == '-' || map.map(pos2.x)(pos2.y + 1) == '-') "Zavrsena igra!"
//            else ""
//        }
      }
    }
  }

  def checkNewPosition(row: Int, col: Int, block: Block, map: Map): Boolean = {
    block match {
      case Block(pos1, None) =>
        if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-') false
        else ""
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x =>
        if (map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') false
        else ""
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x =>
        if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') false
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
