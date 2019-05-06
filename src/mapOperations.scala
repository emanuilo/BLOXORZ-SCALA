
import Main._
import scala.io.StdIn

object MapOperations {
  def inputMapOperation(): Option[Int] = {
    val choice = StdIn.readInt()
    if (choice > 11 || choice < 1) None
    else Some(choice)
  }

  def inputRowAndCol(): Option[(Int, Int)] = {
    println("Uneti poziciju u formatu 'red kolona' \n")
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

  def checkIndices(row: Int, col: Int, map: Map): Boolean = {
      row < map.map.size - 1 &&
      col < map.map(0).length - 1 &&
      row > 0 &&
      col > 0
  }

  def removeEdgePlate(map: Option[Map], target: Option[(Int, Int)]): Option[Map] = {
    if(map.isEmpty) return None
    if(target.isEmpty) return None
    val row = target.get._1
    val col = target.get._2
    val newMap = Map(map.get.map.map(_.clone()))

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
    else
      None
  }

  def addEdgePlate(map: Option[Map], target: Option[(Int, Int)]): Option[Map] = {
    if(map.isEmpty) return None
    if(target.isEmpty) return None
    val row = target.get._1
    val col = target.get._2
    val newMap = Map(map.get.map.map(_.clone()))

    if ( checkIndices(row, col, newMap) && ((                            // at least one surrounding plate
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
    else
      None
  }

  def replaceBasicToSpec(map: Option[Map], target: Option[(Int, Int)]): Option[Map] = {
    if(map.isEmpty) return None
    if(target.isEmpty) return None
    val row = target.get._1
    val col = target.get._2
    val newMap = Map(map.get.map.map(_.clone()))

    if ( checkIndices(row, col, newMap) &&
         newMap.map(row)(col) == plateChar ) {
      newMap.map(row)(col) = specPlateChar
      Some(newMap)
    }
    else
      None
  }

  def replaceSpecToBasic(map: Option[Map], target: Option[(Int, Int)]): Option[Map] = {
    if(map.isEmpty) return None
    val newMap = Map(map.get.map.map(_.clone()))

    target match {
      case Some((row, col)) =>
        if ( checkIndices(row, col, newMap) &&
             newMap.map(row)(col) == specPlateChar ) {
          newMap.map(row)(col) = plateChar
          Some(newMap)
        }
        else None
      case None => None
    }
  }

  def setPosition(map: Option[Map], target: Option[(Int, Int)], oldPosition: (Int, Int), char: Char): Option[Map] = {
    if(map.isEmpty) return None
    val newMap = Map(map.get.map.map(_.clone()))

    target match {
      case Some((row, col)) =>
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
      case None => None
    }
  }

  def inverseStartFinish(map: Option[Map]): Option[Map] = {
    if(map.isEmpty) return None

    setPosition(
      setPosition(
        map,
        Some(getFinishPosition(map.get)),
        getStartPosition(map.get),
        startChar
      ),
      Some(getStartPosition(map.get)),
      getStartPosition(map.get),           // start position because we don't want to rewrite old position, now there is finish position
      finishChar
    )
  }

  def replaceAllSpecToBasic(map: Option[Map]): Option[Map] = {
    if(map.isEmpty) return None
    var newMap: Option[Map] = Some(Map(map.get.map.map(_.clone())))

    for(row <- 1 until map.get.map.size - 1;
        col <- 1 until map.get.map.head.length - 1
        if map.get.map(row)(col) == specPlateChar)
      newMap = replaceSpecToBasic(newMap, Some(row, col))

    newMap
  }
}


