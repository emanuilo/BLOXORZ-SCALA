import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Map(val map: ArrayBuffer[Array[Char]]) {
   def printMap() {
      for(line <- map)
        println(line.mkString(""))
      println("\n")
  }
}

object Map {
  def readFromFile(path: String): Option[Map] ={
    val map: ArrayBuffer[Array[Char]] = ArrayBuffer()
    try{
      for (line <- Source.fromFile(path).getLines()) {
        map += line.toCharArray
      }
      Some(new Map(map))
    }
    catch {
      case Exception => None
    }
  }
}
