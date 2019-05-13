import Main._

object FindingPath {
  val right = 'r'
  val left = 'l'
  val up = 'u'
  val down = 'd'
  var stack: List[Char] = List()

  def findPath(block: Block, map: Map): String = {
    play(down, block, map) match {
      case (_block, "Win") =>
        stack = down :: stack
        return "Win"
      case (_block, "OK") =>
        if (stack.isEmpty || stack.head != up){
          stack = down :: stack
          if (findPath(_block, map) == "Win") return "Win"
        }
      case (_block, "Fail") =>
    }
    play(right, block, map) match {
      case (_block, "Win") =>
        stack = right :: stack
        return "Win"
      case (_block, "OK") =>
        if (stack.isEmpty || stack.head != left){
          stack = right :: stack
          if (findPath(_block, map) == "Win") return "Win"
        }
      case (_block, "Fail") =>
    }
    play(left, block, map) match {
      case (_block, "Win") =>
        stack = left :: stack
        return "Win"
      case (_block, "OK") =>
        if (stack.isEmpty || stack.head != right){
          stack = left :: stack
          if (findPath(_block, map) == "Win") return "Win"
        }
      case (_block, "Fail") =>
    }
    play(up, block, map) match {
      case (_block, "Win") =>
        stack = up :: stack
        "Win"
      case (_block, "OK") =>
        if (stack.isEmpty || stack.head != down){
          stack = up :: stack
          if (findPath(_block, map) == "Win") "Win"
          else "Fail"
        }
        else "Fail"
      case (_block, "Fail") => "Fail"
    }
  }

  def play(direction: Char, block: Block, map: Map): (Block, String) = {
    val row_col = getCoordsTuple(direction)
    if (row_col.isEmpty) {
      println("Pogresna komanda!")
      return play(inputPlayerMove, block, map)
    }
    val row = row_col.get._1
    val col = row_col.get._2
    val finishPos = getFinishPosition(map)

    block match {
      case Block(pos1, None) => // block is standing upright
        if ( map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-' ||  // game over if block gets out of the map
          map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos1.x + 2 * row)(pos1.y) == '-' )
          (block, "Fail")
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

          (newBlock, "OK")
        }
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x => // block is lying horizontally
        if (map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') // game over if block gets out of the map
          (block, "Fail")
        else if (map.map(pos1.x)(pos1.y + col) == 'T' && col == -1 || map.map(pos2.x)(pos2.y + col) == 'T' && col == 1) (block, "Win")
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

          (newBlock, "OK")
        }
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x => // block is lying vertically
        if (map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-') // game over if block gets out of the map
          (block, "Fail")
        else if (map.map(pos1.x + row)(pos1.y) == 'T' && row == -1 || map.map(pos2.x + row)(pos2.y) == 'T' && row == 1) (block, "Win")
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

          (newBlock, "OK")
        }
    }
  }
}
