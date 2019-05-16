import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}
import MapOperations._
import FindingPath._
import Game._
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

  val gameMenu: String = "1. Odigravanje poteza \n" +
    "2. Ucitavanje poteza iz fajla \n" +
    "3. Nazad \n" +
    "Uneti izbor: "

  val startChar = 'S'
  val finishChar = 'T'
  val plateChar = 'o'
  val specPlateChar = '.'
  val noPlateChar = '-'
  val OFFSET = 4
  var fileNameCnt = 0

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
        case '1' => readMapFromFile(getMapFilePath) match {
            case Some(x) => maps = x :: maps
            case None => println("Neuspesno ucitavanje mape!")
          }
        case '2' =>
          menuStack = new GameMenu() :: menuStack
        case '3' =>
          menuStack = new MapOpsMenu(getMap(inputMapNumber(), maps)) :: menuStack
        case '4' => getMap(inputMapNumber(), maps) match {
            case Some(_map) => printPathToFile(getPath(initBlockPosition(_map), _map))
            case None => println("Pogresan unos!")
          }
        case '5' =>
          menuStack = List()
        case _ =>
          println("Pogresan unos!")
      }
    }
  }

  class GameMenu() extends Menu {
    override def menu(): Unit = {
      println(gameMenu)
      StdIn.readChar() match {
        case '1' => getMap(inputMapNumber(), maps) match {
          case Some(_map) =>
            println(playMove(inputPlayerMove _, initBlockPosition(_map), Map(_map.map.map(_.clone()))))
          case None => println("Pogresan unos!")
        }
        case '2' => getMap(inputMapNumber(), maps) match {
          case Some(_map) =>
            val iter = getFileLineIterator(inputFileName())
            try{
              println(playMove(() => iter.next().charAt(0), initBlockPosition(_map), Map(_map.map.map(_.clone()))))
            }
            catch {
              case e: NoSuchElementException => println("Fail \n")
            }
          case None => println("Pogresan unos!")
        }
        case '3' => menuStack = menuStack.tail
        case _ => println("Pogresan unos!")
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
        case "12" => // sacuvaj mapu
          maps = map.get :: maps
          menuStack = menuStack.tail
        case "13" => // nazad
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

}
