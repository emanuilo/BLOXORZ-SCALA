import Main._

import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}
import MapOperations._
import FindingPath._
import scala.collection.mutable

object Main {
  val menuStr: String = "1. Ucitaj mapu \n" +
    "2. Zapocni igru \n" +
    "3. Kreiranje mapa \n" +
    "4. Ispis resenja \n" +
    "5. Kraj \n" +
    "Uneti izbor: "

  val mapCreatingMenu: String = "1. Uklanjanje zadate ploce sa ivice terena \n" +
    "2. Dodavanje ploce na zadatu poziciju na ivici terena \n" +
    "3. Zamena obicne ploce na zadatoj poziciji specijalnom \n" +
    "4. Zamena specijalne ploce na zadatoj poziciji obicnom \n" +
    "5. Postavljanje startne pozicije na zadato polje \n" +
    "6. Postavljanje ciljne pozicije na zadato polje \n" +
    "7. Inverzija \n" +
    "8. Zamena \n" +
    "9. Filtriranje \n" +
    "10. Formiranje imenovane sekvence operacija \n" +
    "11. Pravljenje imenovane kompozitne operacije \n" +
    "12. Sacuvaj mapu \n" +
    "13. Nazad"

  val startChar: Char = 'S'
  val finishChar: Char = 'T'
  val plateChar: Char = 'o'
  val specPlateChar: Char = '.'
  val noPlateChar: Char = '-'
  val OFFSET = 4

  var maps: List[Map] = List()
  var menuStack: List[Menu] = List(new MainMenu)
  val operationList = List(
    removeEdgePlate(inputRowAndCol) _,
    addEdgePlate(inputRowAndCol) _,
    replaceBasicToSpec(inputRowAndCol) _,
    replaceSpecToBasic(inputRowAndCol) _,
    setPosition(inputRowAndCol, getStartPosition, startChar) _,
    setPosition(inputRowAndCol, getFinishPosition, finishChar) _,
    inverseStartFinish _,
    replaceAllSpecToBasic _,
    filter(inputRowAndCol, inputDistance) _
  )

  val sequencesMap: mutable.LinkedHashMap[String, List[Option[Map] => Option[Map]]] = mutable.LinkedHashMap()
  val compositsMap: mutable.LinkedHashMap[String, Option[Map] => Option[Map]] = mutable.LinkedHashMap()

  case class Map(map: ArrayBuffer[Array[Char]])
  case class Block(var position1: Position, var position2: Option[Position])
  case class Position(var x: Int, var y: Int)

  trait Menu {
    def menu()
  }

  class MainMenu extends Menu {
    override def menu(): Unit = {
      println(menuStr)
      StdIn.readChar() match {
        case '1' =>
          readMapFromFile(getMapFilePath) match {
            case None => println("Neuspesno ucitavanje mape!")
            case Some(x) => maps = x :: maps
          }
        case '2' =>
          val map = getMap(inputMapNumber(), maps)
          map match {
            case None => println("Pogresan unos!")
            case Some(_map) =>
              println(playMove(inputPlayerMove, initBlockPosition(_map), Map(_map.map.map(_.clone()))))
          }
        case '3' =>
          menuStack = new MapOpsMenu(getMap(inputMapNumber(), maps)) :: menuStack
        case '4' =>
          val map = getMap(inputMapNumber(), maps)
          map match {
            case Some(_map) =>
              stateHistory += ((initBlockPosition(_map), down))
              findPath(initBlockPosition(_map), Map(_map.map.map(_.clone())))
              println(stack.reverse)
            case None => println("Pogresan unos!")
          }
        case '5' =>
          menuStack = List()
        case _ =>
          println("Pogresan unos!")
      }
    }
  }

  class MapOpsMenu(var map: Option[Map]) extends Menu {
    override def menu(): Unit = {
      println(mapCreatingMenu)
      printCustomOperations()
      println("Uneti izbor: \n")
      StdIn.readLine() match {
        case "1" => removeEdgePlate(inputRowAndCol)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "2" => addEdgePlate(inputRowAndCol)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "3" => replaceBasicToSpec(inputRowAndCol)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "4" => replaceSpecToBasic(inputRowAndCol)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "5" => setPosition(inputRowAndCol, getStartPosition, startChar)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "6" => setPosition(inputRowAndCol, getFinishPosition, finishChar)(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "7" => inverseStartFinish(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "8" => replaceAllSpecToBasic(map) match {
          case Some(_map) => map = Some(_map)
          case None => println("Greska!")
        }
        case "9" => filter(inputRowAndCol, inputDistance)(map) match {
          case Some(_map) => map = Option(_map)
          case None => println("Greska!")
        }
        case "10" => makeListOfInts(inputOperationsNumbers()) match {
          case Some(listOfNumbers) => sequencesMap += (inputSequenceName() -> makeListOfOperations(listOfNumbers))
          case None => println("Greska!")
        }
        case "11" => makeListOfInts(inputOperationsNumbers()) match {
          case Some(listOfNumbers) => compositsMap += (inputSequenceName() -> makeCompositeOperation(listOfNumbers.reverse))
          case None => println("Greska!")
        }
        case "12" =>
          maps = map.get :: maps
          menuStack = menuStack.tail
        case "13" =>
          menuStack = menuStack.tail
        case other =>
          try{
            val number = Integer.parseInt(other) - operationList.size - 1 - OFFSET    // 4 = {10, 11, 12, 13}
            if(number < sequencesMap.size){
              sequencesMap.drop(number).headOption match {
                case Some((name, operations)) =>
                  for(f <- operations)
                    f(map) match {
                      case Some(_map) => map = Option(_map); printMap(map.get)
                      case None => println("Greska!")
                  }
                case None => println("Greska!")
              }
            }
            else{
              compositsMap.drop(number - sequencesMap.size).headOption match {
                case Some((name, operation)) =>
                    operation(map) match {
                      case Some(_map) => map = Option(_map); printMap(map.get)
                      case None => println("Greska!")
                    }
                case None => println("Greska!")
              }
            }
          } catch { case e: NumberFormatException => println("Greska!")}
      }
      printMap(map.get)
    }
  }

  def main(args: Array[String]): Unit = {
    menuStack.head.menu()
    if(menuStack.nonEmpty) main(args)
  }

  def printCustomOperations(): Unit = {
    for(((key, value), index) <- sequencesMap.zipWithIndex){
      val number = operationList.size + index + 1 + OFFSET   // plus 4 because there is 4 other items in list besides basic operations (save the map, back, etc.)
      println(s"$number. $key")
    }
    for(((key, value), index) <- compositsMap.zipWithIndex){
      val number = operationList.size + sequencesMap.size + index + 1 + OFFSET   // plus 4 because there is 4 other items in list besides basic operations (save the map, back, etc.)
      println(s"$number. $key")
    }
  }

  def inputOperationsNumbers(): String = {
    println("Uneti redne brojeve operacija: \n")
    StdIn.readLine()
  }

  def makeListOfInts(str: String): Option[List[Int]] = {
    val numbers = str.split(" ").toList
    try{
      Option(numbers.map(_str => Integer.parseInt(_str) - 1))
    }
    catch {
      case e: NumberFormatException => None
    }
  }

  def inputSequenceName(): String = {
    println("Uneti ime sekvence: \n")
    StdIn.readLine()
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

  def menu: Int = {
    println(menuStr)
    StdIn.readInt()
  }

}
