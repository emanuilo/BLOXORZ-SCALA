
import Main._
import Game._
import scala.io.StdIn

object MapOperations {
  def inputMapOperation(): Option[Int] = {
    val choice = StdIn.readInt()
    if (choice > 11 || choice < 1) None
    else Some(choice)
  }

  def inputRowAndCol(): Option[(Int, Int)] = {
    println("Uneti poziciju u formatu 'red kolona': \n")
    val line = StdIn.readLine()
    val row_col = line.split(" ")
    try{
      if(row_col.size == 2)
        Some((Integer.parseInt(row_col(0)), Integer.parseInt(row_col(1))))
      else None
    }
    catch {
      case e: NumberFormatException => None
    }
  }

  def inputDistance(): Option[Int] = {
    println("Uneti distancu: \n")
    try{
      Option(StdIn.readInt())
    }
    catch {
      case e: NumberFormatException => None
    }
  }

  def checkIndices(row: Int, col: Int, map: Map): Boolean = {
      row < map.map.size - 1 &&
      col < map.map(0).length - 1 &&
      row > 0 &&
      col > 0
  }

  def removeEdgePlate(target: () => Option[(Int, Int)])(map: Option[Map]): Option[Map] = {
    (target(), map) match {
      case (Some((row, col)), Some(_map)) =>
        val newMap = Map(_map.map.map(_.clone()))
        if ( checkIndices(row, col, newMap) && ((
            newMap.map(row)(col - 1) == noPlateChar ||
            newMap.map(row)(col + 1) == noPlateChar ||
            newMap.map(row - 1)(col) == noPlateChar ||
            newMap.map(row + 1)(col) == noPlateChar ) && !(
            newMap.map(row)(col - 1) == noPlateChar &&
            newMap.map(row)(col + 1) == noPlateChar &&
            newMap.map(row - 1)(col) == noPlateChar &&
            newMap.map(row + 1)(col) == noPlateChar )) ) {
          newMap.map(row)(col) = '-'
          Some(newMap)
        }
        else None
      case _ => None
    }
  }

  def addEdgePlate(target: () => Option[(Int, Int)])(map: Option[Map]): Option[Map] = {
    (target(), map) match {
      case (Some((row, col)), Some(_map)) =>
        val newMap = Map(_map.map.map(_.clone()))
        if ( checkIndices(row, col, newMap) && ((             // at least one surrounding plate
            newMap.map(row)(col - 1) != noPlateChar ||
            newMap.map(row)(col + 1) != noPlateChar ||
            newMap.map(row - 1)(col) != noPlateChar ||
            newMap.map(row + 1)(col) != noPlateChar ) && !(   // but not all of them
            newMap.map(row)(col - 1) != noPlateChar &&
            newMap.map(row)(col + 1) != noPlateChar &&
            newMap.map(row - 1)(col) != noPlateChar &&
            newMap.map(row + 1)(col) != noPlateChar )) ) {
          newMap.map(row)(col) = 'o'
          Some(newMap)
        }
        else None
      case _ => None
    }
  }

  def replaceBasicToSpec(target: () => Option[(Int, Int)])(map: Option[Map]): Option[Map] = {
    (target(), map) match {
      case (Some((row, col)), Some(_map)) =>
        val newMap = Map(_map.map.map(_.clone()))
        if ( checkIndices(row, col, newMap) &&
          newMap.map(row)(col) == plateChar ) {
          newMap.map(row)(col) = specPlateChar
          Some(newMap)
        }
        else None
      case _ => None
    }
  }

  def replaceSpecToBasic(target: () => Option[(Int, Int)])(map: Option[Map]): Option[Map] = {
    (target(), map) match {
      case (Some((row, col)), Some(_map)) =>
        val newMap = Map(_map.map.map(_.clone()))
        if ( checkIndices(row, col, newMap) &&
             newMap.map(row)(col) == specPlateChar ) {
          newMap.map(row)(col) = plateChar
          Some(newMap)
        }
        else None
      case _ => None
    }
  }

  def setPosition(target: () => Option[(Int, Int)], getOldPosition: Map => (Int, Int), char: Char)(map: Option[Map]): Option[Map] = {
    (target(), map) match {
      case (Some((row, col)), Some(_map)) =>
        val newMap = Map(_map.map.map(_.clone()))
        val oldPosition = getOldPosition(newMap)
        if( checkIndices(row, col, newMap) && !(
            newMap.map(row)(col - 1) == noPlateChar &&
            newMap.map(row)(col + 1) == noPlateChar &&
            newMap.map(row - 1)(col) == noPlateChar &&
            newMap.map(row + 1)(col) == noPlateChar) ) {
          newMap.map(oldPosition._1)(oldPosition._2) = plateChar
          newMap.map(row)(col) = char
          Some(newMap)
        }
        else None
      case _ => None
    }
  }

  def inverseStartFinish(map: Option[Map]): Option[Map] = {
    if(map.isEmpty) return None

    setPosition(
      () => Some(getStartPosition(map.get)),     // new position -> start position
      Map => getStartPosition(map.get),          // old position -> start position, because we don't want to rewrite the old finish position, now there is the start position
      finishChar
    )(
      setPosition(                               // setting start position at the finish position
        () => Some(getFinishPosition(map.get)),  // new position -> finish position
        Map => getStartPosition(map.get),        // old start position
        startChar                                // which char to put on the position
      )(
        map
      )
    )
  }

  def replaceAllSpecToBasic(map: Option[Map]): Option[Map] = {
    if(map.isEmpty) return None
    var newMap: Option[Map] = Some(Map(map.get.map.map(_.clone())))

    for(row <- 1 until map.get.map.size - 1;
        col <- 1 until map.get.map.head.length - 1
        if map.get.map(row)(col) == specPlateChar)
      newMap = replaceSpecToBasic(() => Some(row, col))(newMap)

    newMap
  }

  def filter(target: () => Option[(Int, Int)], getDistance: () => Option[Int])(_map: Option[Map]): Option[Map] = {
    (target(), getDistance(), _map) match {
      case (Some((row, col)), Some(distance), Some(map)) =>
        val exist = (for{i <- row - distance to row + distance         // looking for at least one spec plate in a square around the plate size of distance
            if i >= 0 && i < map.map.size
            j <- col - distance to col + distance
            if j >= 0 && j < map.map.head.length
            if map.map(i)(j) == specPlateChar} yield true).headOption  // it doesn't matter if there is more than one spec plate
        exist match {
          case Some(_) => replaceSpecToBasic(target)(_map)
          case None => None
        }
      case _ => None
    }
  }

  def makeListOfOperations(list: List[Int]): List[Option[Map] => Option[Map]] = {
    list match {
      case h :: t => operationList(h) :: makeListOfOperations(t)
      case List() => List()
    }
  }

  def compositsRange(number: Int): Int = number - OFFSET - sequencesMap.size - operationList.size

  def makeCompositeOperation(listOfInts: List[Int]): Option[Map] => Option[Map] = {
    def compose(map: Option[Map], list: List[Int]): Option[Map] = {
      list match {
        case h :: t if t.isEmpty && h < operationList.size =>
          operationList(h)(map)
        case h :: t if t.isEmpty && h >= operationList.size + OFFSET + sequencesMap.size =>
          compositsMap.drop(compositsRange(h)).head._2(map)
        case h :: t if h < operationList.size =>
          operationList(h)(compose(map, t))
        case h :: t if h >= operationList.size + OFFSET + sequencesMap.size =>
          compositsMap.drop(compositsRange(h)).head._2(compose(map, t))
        case _ => None
      }
    }
    map => compose(map, listOfInts)
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
}


