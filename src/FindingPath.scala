import Main._
import Game._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FindingPath {

  object State extends Enumeration {
    type State = Value
    val I, II, III, IV, V, VI = Value
  }
  import State._

  val right = 'r'
  val left = 'l'
  val up = 'u'
  val down = 'd'

  var moves: List[Char] = List(up, left, down, right)
  var stack: List[Char] = List()
  val stackBuffer: ArrayBuffer[List[Char]] = ArrayBuffer()
  val stateHistory: mutable.HashSet[(Block, State)] = mutable.HashSet()
  var state: State = I

  val oppositeDir: Char => Char = {
    case `down` => up
    case `left` => right
    case `right` => left
    case `up` => down
  }

  def printPathToFile(path: List[Char]): Unit = {
    println(path)
    import java.io._
    val pw = new PrintWriter(new File(s"path$fileNameCnt.txt" ))
    pw.write(path.mkString("\n"))
    pw.close()
    fileNameCnt = fileNameCnt + 1
  }

  def getPath(block: Block, map: Map): List[Char] = {
    stackBuffer.clear()
    for (permutation <- moves.permutations.toList){                      // every permutation of moves collection
      moves = permutation
      state = I
      stack = List()
      stateHistory.clear()
      stateHistory += ((block.copy(), state))                            // adding starting state of the block
      if (findPath(block.copy(), Map(map.map.map(_.clone()))) == "Win")  // if the path is found, add it to the buffer
        stackBuffer += stack.reverse
    }

    if (stackBuffer.nonEmpty) stackBuffer.minBy(_.size)                  // find the shortest path
    else List()                                                          // couldn't find the path
  }

  def findPath(block: Block, map: Map): String = {
    for (move <- moves) {
      play(move, block, map) match {
        case "Win" =>
          stack = move :: stack
          return "Win"
        case "OK" =>
            if(!stateHistory.contains((block, state)) &&                 // if the block has already been in this state
            numOfPossibleMoves(block, map) > 0)                          // or if there is 0 available moves, reverse the move
            {
              stack = move :: stack
              stateHistory += ((block.copy(), state))
//              printMap(map)
//              println(stack)
              if (findPath(block, map) == "Win") return "Win"            // go find another move
            }
            else play(oppositeDir(move), block, map)                     // reverse already played move
        case "Fail" => {
//          printMap(map)
//          println(stack)
        }
      }
    }
    "Fail"
  }

  def numOfPossibleMoves(block: Block, map: Map): Int = {
    val backupState = state
    val size = (for {                                                    // find number of available moves on the current position
      move <- moves
      _block = block.copy()
      _map = Map(map.map.map(_.clone()))
      if play(move, _block, _map) != "Fail"                              // move is available if its not a fail
      if !stateHistory.contains((_block, newState(backupState, move)))   // or if it hasn't been before
    } yield move).size                                                   // size of the returned list is the number of avail moves
    state = backupState
    size
  }

  def newState(_state: State, move: Char): State = {
    _state match {
      case I =>
        move match {
          case `down` => II
          case `up` => VI
          case `left` => V
          case `right` => IV
        }
      case II =>
        move match {
          case `down` => III
          case `up` => I
          case `left` => II
          case `right` => II
        }
      case III =>
        move match {
          case `down` => VI
          case `up` => II
          case `left` => IV
          case `right` => V
        }
      case IV =>
        move match {
          case `down` => IV
          case `up` => IV
          case `left` => I
          case `right` => III
        }
      case V =>
        move match {
          case `down` => V
          case `up` => V
          case `left` => III
          case `right` => I
        }
      case VI =>
        move match {
          case `down` => I
          case `up` => III
          case `left` => VI
          case `right` => VI
        }
    }
  }

  def play(direction: Char, block: Block, map: Map): String = {
    val row_col = getCoordsTuple(direction)
    val row = row_col.get._1
    val col = row_col.get._2
    val finishPos = getFinishPosition(map)

    block match {
      case Block(pos1, None) => // block is standing upright
        if ( map.map(pos1.x)(pos1.y + col) == '-' || map.map(pos1.x)(pos1.y + 2 * col) == '-' ||  // game over if block gets out of the map
          map.map(pos1.x + row)(pos1.y) == '-' || map.map(pos1.x + 2 * row)(pos1.y) == '-' )
          "Fail"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with 'o'
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos1.x + 2 * row, pos1.y + 2 * col)
          map.map(pos1.x + row)(pos1.y + col) = 'X'                               // replace char with 'X' at new block position
          map.map(pos1.x + 2 * row)(pos1.y + 2 * col) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish position if it's been overwritten

          if(row == -1 || col == -1) {                                            // sorting which position is first in Block object
            block.position1 = Position(pos1.x + 2 * row, pos1.y + 2 * col)
            block.position2 = Some(Position(pos1.x + row, pos1.y + col))
          }
          else{
            block.position1 = Position(pos1.x + row, pos1.y + col)
            block.position2 = Some(Position(pos1.x + 2 * row, pos1.y + 2 * col))
          }

          state = newState(state, direction)
          "OK"
        }
      case Block(pos1, Some(pos2)) if pos1.x == pos2.x => // block is lying horizontally
        if (map.map(pos1.x + row)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-' || (
            direction == left && map.map(pos1.x)(pos1.y + col) == specPlateChar) || (
            direction == right && map.map(pos2.x)(pos2.y + col) == specPlateChar)) // game over if block gets out of the map
          "Fail"
        else if (map.map(pos1.x)(pos1.y + col) == 'T' && col == -1 || map.map(pos2.x)(pos2.y + col) == 'T' && col == 1) "Win"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
          rewriteOldSpec(map)                                                     // rewrite spec plates if they have been replaced by the block
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos2.x + row, pos2.y + col)
          if(col == -1)
            map.map(pos1.x + row)(pos1.y + col) = 'X'                             // replace char with 'X' at new block position
          else
            map.map(pos2.x + row)(pos2.y + col) = 'X'                             // replace char with 'X' at new block position
          if (row != 0) map.map(pos1.x + row)(pos1.y) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish if it's been overwritten

          if (col == -1){                                                         // sorting which position is first in Block object
            block.position1 = Position(pos1.x, pos1.y + col)
            block.position2 = None
          }
          else if (col == 1){
            block.position1 = Position(pos2.x, pos2.y + col)
            block.position2 = None
          }
          else{
            block.position1 = Position(pos1.x + row, pos1.y)
            block.position2 = Some(Position(pos2.x + row, pos2.y))
          }

          state = newState(state, direction)
          "OK"
        }
      case Block(pos1, Some(pos2)) if pos1.x != pos2.x => // block is lying vertically
        if (map.map(pos1.x + row)(pos1.y + col) == '-' || map.map(pos2.x + row)(pos2.y + col) == '-' || (
            direction == up && map.map(pos1.x + row)(pos1.y) == specPlateChar) || (
            direction == down && map.map(pos2.x + row)(pos2.y) == specPlateChar)) // game over if block gets out of the map
          "Fail"
        else if (map.map(pos1.x + row)(pos1.y) == 'T' && row == -1 || map.map(pos2.x + row)(pos2.y) == 'T' && row == 1) "Win"
        else {
          map.map(pos1.x) = map.map(pos1.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
          map.map(pos2.x) = map.map(pos2.x).map(ch => if (ch == 'X') 'o' else ch) // replace old block position with '.'
          rewriteOldSpec(map)                                                     // rewrite spec plates if they have been replaced by the block
          saveOldSpec(map, pos1.x + row, pos1.y + col, pos2.x + row, pos2.y + col)
          if (row == -1)
            map.map(pos1.x + row)(pos1.y + col) = 'X'                             // replace char with 'X' at new block position
          else
            map.map(pos2.x + row)(pos2.y + col) = 'X'                             // replace char with 'X' at new block position
          if (col != 0) map.map(pos1.x)(pos1.y + col) = 'X'                       // replace char with 'X' at new block position
          map.map(finishPos._1)(finishPos._2) = 'T'                               // put back finish if it's been overwritten

          if (row == 1){                                                          // sorting which position is first in Block object
            block.position1 = Position(pos2.x + row, pos2.y)
            block.position2 = None
          }
          else if (row == -1){
            block.position1 = Position(pos1.x + row, pos1.y)
            block.position2 = None
          }
          else{
            block.position1 = Position(pos1.x, pos1.y + col)
            block.position2 = Some(Position(pos2.x, pos2.y + col))
          }

          state = newState(state, direction)
          "OK"
        }
    }
  }
}
