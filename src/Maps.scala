import scala.io.StdIn

object Maps extends MenuItem {
  var maps: List[Map] = List()

  override def job() {
    println("Unesi putanju: ")
    val path = StdIn.readLine()
    val newMap = Map.readFromFile(path)
    newMap match {
      case Some(x) => maps = x :: maps
      case None => println("Neuspesno ucitavanje mape!")
    }
  }

  def printMaps(): Unit = {
    for ((map, index) <- maps.zipWithIndex) {
      println(s"Map $index:")
      map.printMap()
    }
  }

  def getMap(num: Int): Option[Map] = {
    def _getMap(_num: Int, _maps: List[Map]): Option[Map] = {
      _maps match {
        case List() => None
        case h :: t if _num > 1 => _getMap(_num - 1, t)
        case h :: t if _num == 1 => Some(h)
      }
    }
    _getMap(num, maps)
  }

}
