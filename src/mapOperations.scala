
import Main._
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

  def setPosition(getOldPosition: Map => (Int, Int), char: Char)(map: Option[Map], target: Option[(Int, Int)]): Option[Map] = {
    if(map.isEmpty) return None
    val newMap = Map(map.get.map.map(_.clone()))
    val oldPosition = getOldPosition(newMap)

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
      getStartPosition,           // start position because we don't want to rewrite the old position, now there is the finish position
      finishChar
    )(
      setPosition(
        getStartPosition,
        startChar
      )(
        map,
        Some(getFinishPosition(map.get))
      ),
      Some(getStartPosition(map.get))
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

  def filter(_map: Option[Map], target: Option[(Int, Int)], _distance: Option[Int]): Option[Map] = {
    (target, _distance, _map) match {
      case (Some((row, col)), Some(distance), Some(map)) =>
        val exist = (for{i <- row - distance to row + distance
            if i >= 0 && i < map.map.size
            j <- col - distance to col + distance
            if j >= 0 && j < map.map.head.length
            if map.map(i)(j) == specPlateChar} yield true).headOption
        exist match {
          case Some(_) => replaceSpecToBasic(_map, target)
          case None => None
        }
      case _ => None
    }
  }
}


