package chess
package format
package pgn

import play.api.libs.json._

import scala._

case class Pgn(
    tags: Tags,
    turns: List[Turn],
    initial: Initial = Initial.empty
) {

  def updateTurn(fullMove: Int, f: Turn => Turn) = {
    val index = fullMove - 1
    (turns lift index).fold(this) { turn =>
      copy(turns = turns.updated(index, f(turn)))
    }
  }
  def updatePly(ply: Int, f: Move => Move) = {
    val fullMove = (ply + 1) / 2
    val color    = Color(ply % 2 == 1)
    updateTurn(fullMove, _.update(color, f))
  }
  def updateLastPly(f: Move => Move) = updatePly(nbPlies, f)

  def nbPlies = turns.foldLeft(0)(_ + _.count)

  def moves =
    turns.flatMap { t =>
      List(t.white, t.black).flatten
    }

  def withEvent(title: String) =
    copy(
      tags = tags + Tag(_.Event, title)
    )

  def render: String = {
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr = turns mkString " "
    val endStr  = tags(_.Result) | ""
    s"$tags\n\n$initStr$turnStr $endStr"
  }.trim

  val destSymbols = Map(
    "9" -> "一",
    "8" -> "二",
    "7" -> "三",
    "6" -> "四",
    "5" -> "五",
    "4" -> "六",
    "3" -> "七",
    "2" -> "八",
    "1" -> "九",
    "a" ->  "９",
    "b" ->  "８",
    "c" ->  "７",
    "d" ->  "６",
    "e" ->  "５",
    "f" ->  "４",
    "g" ->  "３",
    "h" ->  "２",
    "i" ->  "１"
  )
  val origSymbols = Map(
    "9" -> "1",
    "8" -> "2",
    "7" -> "3",
    "6" -> "4",
    "5" -> "5",
    "4" -> "6",
    "3" -> "7",
    "2" -> "8",
    "1" -> "9",
    "a" ->  "9",
    "b" ->  "8",
    "c" ->  "7",
    "d" ->  "6",
    "e" ->  "5",
    "f" ->  "4",
    "g" ->  "3",
    "h" ->  "2",
    "i" ->  "1"
  )
  val pieceSymbols = Map(
    "P" ->  "歩",
    "L" ->  "香",
    "N" ->  "桂",
    "S" ->  "銀",
    "B" ->  "角",
    "R" ->  "飛",
    "G" ->  "金",
    "U" ->  "成香",
    "M" ->  "成桂",
    "A" ->  "成銀",
    "T" ->  "と",
    "H" ->  "馬",
    "D" ->  "龍",
    "K" ->  "玉"
  )
  val kifuSymbols = Map(
    "+" ->  "成",
    "same" -> "同　",
    "*" -> "打"
  )

  def renderAsKifu(uciPgn: scala.collection.IndexedSeq[(String, String)]) = {
    val movesHeader = """手合割：平手
手数----指手---------消費時間--
"""
    val moveStr = movesAsKifu(uciPgn.foldLeft(Vector[(String, String)]()) {_ :+ _}).zipWithIndex map { (move) => s"${move._2 + 1} ${move._1}" } mkString "\n"
    s"$movesHeader$moveStr"
  }

  def movesAsKifu(uciPgn: Vector[(String, String)]): Vector[String] = {
    uciPgn.foldLeft(Vector[String]()) { (prev, t) =>
      // t is a tuple of (uci, pgn)
      val movePattern = "([a-i])([1-9])([a-i])([1-9])(\\+?)".r
      val dropPattern = "([A-Z])\\*([a-i])([1-9])".r
      val pgnPattern = "([A-Z]).*".r
      val kifuMove = t match {
        case (movePattern(o1, o2, d1, d2, pro), pgnPattern(piece)) => {
          val lastMovePattern = s"(.*)${origSymbols(o1)}${origSymbols(o2)}".r
          (prev.lastOption match {
            // check if 同 is needed
            case Some(lastMovePattern(_)) => kifuSymbols("same")
            // else use dest coords
            case _ => destSymbols(d1) + destSymbols(d2)
          }) + pieceSymbols(piece) + (if (pro == "+") kifuSymbols("+") else "") + "(" + origSymbols(o1) + origSymbols(o2) + ")"
        }
        case (dropPattern(piece, d1, d2), _) => destSymbols(d1) + destSymbols(d2) + pieceSymbols(piece) + kifuSymbols("*")
        case _ => "UCI/PGN parse error"
      }
      prev :+ kifuMove
    }
  }

  override def toString = render
}

case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

case class Turn(
    number: Int,
    white: Option[Move],
    black: Option[Move]
) {

  def update(color: Color, f: Move => Move) =
    color.fold(
      copy(white = white map f),
      copy(black = black map f)
    )

  def updateLast(f: Move => Move) = {
    black.map(m => copy(black = f(m).some)) orElse
      white.map(m => copy(white = f(m).some))
  } | this

  def isEmpty = white.isEmpty && black.isEmpty

  def plyOf(color: Color) = number * 2 - color.fold(1, 0)

  def count = List(white, black) count (_.isDefined)

  override def toString = {
    val text = (white, black) match {
      case (Some(w), Some(b)) if w.isLong => s" $w $number... $b"
      case (Some(w), Some(b))             => s" $w $b"
      case (Some(w), None)                => s" $w"
      case (None, Some(b))                => s".. $b"
      case _                              => ""
    }
    s"$number.$text"
  }
}

object Turn {

  def fromMoves(moves: List[Move], ply: Int): List[Turn] = {
    moves.foldLeft((List[Turn](), ply)) {
      case ((turns, p), move) if p % 2 == 1 =>
        (Turn((p + 1) / 2, move.some, none) :: turns) -> (p + 1)
      case ((Nil, p), move) =>
        (Turn((p + 1) / 2, none, move.some) :: Nil) -> (p + 1)
      case ((t :: tt, p), move) =>
        (t.copy(black = move.some) :: tt) -> (p + 1)
    }
  }._1.reverse
}

case class Move(
    san: String,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    variations: List[List[Turn]] = Nil,
    // time left for the user who made the move, after he made it
    secondsLeft: Option[Int] = None
) {

  def isLong = comments.nonEmpty || variations.nonEmpty

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]")

  override def toString = {
    val glyphStr = glyphs.toList
      .map({
        case glyph if glyph.id <= 6 => glyph.symbol
        case glyph                  => s" $$${glyph.id}"
      })
      .mkString
    val commentsOrTime =
      if (comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments map Move.noDoubleLineBreak)
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    val variationString =
      if (variations.isEmpty) ""
      else variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$san$glyphStr$commentsOrTime$variationString"
  }
}

object Move {

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  private def formatPgnSeconds(t: Int) =
    periodFormatter.print(
      org.joda.time.Duration.standardSeconds(t).toPeriod
    )

  private[this] val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().printZeroAlways
    .minimumPrintedDigits(1)
    .appendHours
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendMinutes
    .appendSeparator(":")
    .appendSeconds
    .toFormatter

}
