package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  
  test("isLegal") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).isLegal)
      assert(!(Block(Pos(999, 84), Pos(999, 84)).isLegal))
      assert(!(Block(Pos(3, 0), Pos(3, 1)).isLegal))
      assert((Block(Pos(3, 1), Pos(3, 1)).isLegal))
      assert((Block(Pos(3, 1), Pos(3, 1)).isLegal))
      assert(!(Block(Pos(-1, 1), Pos(-1, 1)).isLegal))

    }
  }
  
  test("legalNeighbors") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).legalNeighbors == List((Block(Pos(1, 2), Pos(1, 3)), Right), (Block(Pos(2, 1), Pos(3, 1)), Down)))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet == Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(newNeighborsOnly(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))) == Set(
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("Solver: from (level 1") {
    new Level1 {
      val moves = from(Set((startBlock, List())).toStream, Set(startBlock))
      assert(moves(0) === (Block(Pos(1, 2), Pos(1, 3)), List(Right)))
      assert(moves(1) === (Block(Pos(2, 1), Pos(3, 1)), List(Down)))
      assert(moves(2) === (Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)))
      assert(moves(3) === (Block(Pos(2, 2), Pos(2, 3)), List(Down, Right)))
      assert(moves(4) === (Block(Pos(2, 2), Pos(3, 2)), List(Right, Down)))
      assert(moves(5) === (Block(Pos(2, 4), Pos(3, 4)), List(Down, Right, Right)))
      assert(moves(6) === (Block(Pos(2, 1), Pos(2, 1)), List(Left, Down, Right)))
      // ...
      assert(moves.length === 110)
    }
  }
}
