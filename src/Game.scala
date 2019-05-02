import scala.io.StdIn

object Game extends MenuItem {
  override def job() {
    Maps.printMaps()
    println("Izaberi mapu: ")
    val mapNumber = StdIn.readInt()
    val map = Maps.getMap(mapNumber)
    map match {
      case None => println("Pogresan unos!")
      case Some(x) =>
    }
  }


}
