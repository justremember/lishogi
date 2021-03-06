package shogi

import Pos._

class BishopTest extends ShogiTest {

  "a bishop" should {

    val bishop = Sente - Bishop

    "move in 4 directions" in {
      pieceMoves(bishop, E5) must bePoss(F4, F6, G3, G7, H2, H8, I1, I9, D4, D6, C3, C7, B2, B8, A1, A9)
    }

    "move in 2 directions, when at the edges" in {
      pieceMoves(bishop, I7) must bePoss(H8, G9, H6, G5, F4, E3, D2, C1)
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B



N B     P

PPPPPPPPP

    K
"""
      board destsFrom C5 must bePoss(
        board,
        """
k B   x
     x
x   x
 x x
N B     P
 x x
PPPPPPPPP

    K
"""
      )
    }

    "capture opponent pieces" in {
      val board = """
k B
     r
p

N B    P

PPPPPPPPP

    K
"""
      board destsFrom C5 must bePoss(
        board,
        """
k B
     x
x   x
 x x
N B    P
 x x
PPPPPPPPP

    K
"""
      )
    }
    "threaten" in {
      val board = """
k B
  r  r
p

N B    P

PPPPPPPPP

    K
"""
      "a reachable enemy" in {
        board actorAt C5 map (_ threatens A7) must beSome(true)
      }
      "an unreachable enemy" in {
        board actorAt C5 map (_ threatens C8) must beSome(false)
      }
      "a reachable friend" in {
        board actorAt C5 map (_ threatens A3) must beSome(true)
      }
      "nothing up left" in {
        board actorAt C5 map (_ threatens B6) must beSome(true)
      }
      "nothing down right" in {
        board actorAt C5 map (_ threatens D4) must beSome(true)
      }
    }
  }
}
