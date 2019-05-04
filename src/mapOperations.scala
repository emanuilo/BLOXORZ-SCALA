import Main.Map

def removeEdgePlate(row: Int, col: Int, map: Map): Option[Map] = {
  val newMap = Map(map.map.map(_.clone()))
  if (newMap.map(row)(col - 1) == '-' ||
      newMap.map(row)(col + 1) == '-' ||
      newMap.map(row - 1)(col) == '-' ||
      newMap.map(row + 1)(col) == '-')
    newMap(row)(col) = '-'
  else
    None

  Some(newMap)
}



