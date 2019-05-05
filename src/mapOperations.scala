
import Main.Map
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

  def removeEdgePlate(_map: Option[Map], row_col: Option[(Int, Int)]): Option[Map] = {
    if(_map.isEmpty) return None
    if(row_col.isEmpty) return None
    val map = _map.get
    val row = row_col.get._1
    val col = row_col.get._2
    val newMap = Map(map.map.map(_.clone()))

    if (row < map.map.size - 1 &&
        row > 0 &&
        col < map.map(0).length - 1 &&
        col > 0 && ((
        newMap.map(row)(col - 1) == '-' ||
        newMap.map(row)(col + 1) == '-' ||
        newMap.map(row - 1)(col) == '-' ||
        newMap.map(row + 1)(col) == '-' ) && !(
        newMap.map(row)(col - 1) == '-' &&
        newMap.map(row)(col + 1) == '-' &&
        newMap.map(row - 1)(col) == '-' &&
        newMap.map(row + 1)(col) == '-' ))) {
      newMap.map(row)(col) = '-'
      Some(newMap)
    }
    else
      None
  }

  def addEdgePlate(_map: Option[Map], row_col: Option[(Int, Int)]): Option[Map] = {
    if(_map.isEmpty) return None
    if(row_col.isEmpty) return None
    val map = _map.get
    val row = row_col.get._1
    val col = row_col.get._2
    val newMap = Map(map.map.map(_.clone()))

    if (row < map.map.size -1 &&
        row > 0 &&
        col < map.map(0).length - 1 &&
        col > 0 && ((                            // at least one surrounding plate
        newMap.map(row)(col - 1) != '-' ||
        newMap.map(row)(col + 1) != '-' ||
        newMap.map(row - 1)(col) != '-' ||
        newMap.map(row + 1)(col) != '-' ) && !(   // but not all of them
        newMap.map(row)(col - 1) != '-' &&
        newMap.map(row)(col + 1) != '-' &&
        newMap.map(row - 1)(col) != '-' &&
        newMap.map(row + 1)(col) != '-' ))) {
      newMap.map(row)(col) = 'o'
      Some(newMap)
    }
    else
      None
  }

  def replaceBasicWithSpec(_map: Option[Map], row_col: Option[(Int, Int)]): Option[Map] = {
    if(_map.isEmpty) return None
    if(row_col.isEmpty) return None
    val map = _map.get
    val row = row_col.get._1
    val col = row_col.get._2
    val newMap = Map(map.map.map(_.clone()))

    if (row < map.map.size -1 &&
        row > 0 &&
        col < map.map(0).length - 1 &&
        col > 0 &&
        newMap.map(row)(col) == 'o' ) {
      newMap.map(row)(col) = '.'
      Some(newMap)
    }
    else
      None
  }
}


