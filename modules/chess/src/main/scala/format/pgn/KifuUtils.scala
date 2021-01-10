package chess
package format
package pgn

import scala._

object KifuUtils {
  val tagParse = Map[TagType, String](
    Tag.Event -> "棋戦",
    Tag.Site -> "場所",
    Tag.TimeControl -> "持ち時間",
    Tag.Handicap -> "手合割",
    Tag.White -> "先手",
    Tag.Black -> "後手",
    Tag.Opening -> "戦型"
  )
  val tagParseInverted = tagParse.map(_.swap)

  val tagIndex = (List(
    Tag.Event,
    Tag.Site,
    Tag.TimeControl,
    Tag.Handicap,
    Tag.White,
    Tag.Black,
    Tag.Opening
  ) map { _.name }).zipWithIndex.toMap

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
  val destSymbolsInverted = destSymbols.map(_.swap)
  val destSymbolsPattern = "(" + destSymbols.values.mkString("|") + ")"
  val origSymbols1 = Map(
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
  val origSymbols2 = Map(
    "9" -> "1",
    "8" -> "2",
    "7" -> "3",
    "6" -> "4",
    "5" -> "5",
    "4" -> "6",
    "3" -> "7",
    "2" -> "8",
    "1" -> "9"
  )
  val origSymbols1Inverted = origSymbols1.map(_.swap)
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
  val pieceSymbolsInverted = pieceSymbols.map(_.swap)
  val pieceSymbolsPattern = "(" + pieceSymbols.values.mkString("|") + ")"
  val kifuSymbols = Map(
    "+" ->  "成",
    "same" -> "同　",
    "*" -> "打"
  )

  def tagsAsKifu(tags: Tags): Vector[String] = {
    val tagsWithHc = tags + Tag(Tag.Handicap, "平手")
    val preprocessedTags = Tags(value = tagsWithHc.value sortBy { tag =>
      tagIndex.getOrElse(tag.name.name, 9999)
    })
    preprocessedTags.value.toVector map { tag =>
      tagParse.get(tag.name).fold("") { x: String =>
        val timeControlPattern = "(\\d+)\\+(\\d+)\\+(\\d+)\\((\\d+)\\)".r

        val preprocessedValue = tag.name match {
          case Tag.TimeControl => tag.value match {
            case timeControlPattern(init, inc, byo, periods) => {
              val realInit = if (init.toInt % 60 == 0) s"${init.toInt / 60}分" else s"${init}秒"
              s"$realInit+${byo}秒 # $init seconds initial time, $inc seconds increment, $byo seconds byoyomi with $periods periods"
            }
            case _ => "-"
          }
          case _ => tag.value
        }
        if (tag.value != "?") x + "：" +  preprocessedValue else ""
      }
    } filter { _ != "" }
  }

  def movesAsKifu(uciPgn: Vector[(String, String)]): Vector[String] = {
    uciPgn.foldLeft(Vector[String]()) { (prev, t) =>
      // t is a tuple of (uci, pgn)
      val movePattern = "([a-i])([1-9])([a-i])([1-9])(\\+?)".r
      val dropPattern = "([A-Z])\\*([a-i])([1-9])".r
      val pgnPattern = "([A-Z]).*".r
      val kifuMove = t match {
        case (movePattern(o1, o2, d1, d2, pro), pgnPattern(piece)) => {
          val lastMovePattern = s"(.*)${origSymbols1(o1)}${origSymbols2(o2)}".r
          (prev.lastOption match {
            // check if 同 is needed
            case Some(lastMovePattern(_)) => "同　"
            // else use dest coords
            case _ => destSymbols(d1) + destSymbols(d2)
          }) + pieceSymbols(piece) + (if (pro == "+") "成" else "") + "(" + origSymbols1(o1) + origSymbols2(o2) + ")"
        }
        case (dropPattern(piece, d1, d2), _) => destSymbols(d1) + destSymbols(d2) + pieceSymbols(piece) + "打"
        case _ => "UCI/PGN parse error"
      }
      prev :+ kifuMove
    }
  }

  def kifuToPgn(kifu: String): String = {
    val commentPattern = "#.*($|\\n)".r
    val cleanKifu = commentPattern.replaceAllIn(kifu, "$1").replace("\r\n", "\n").replace("\r", "\n")
    val kifuByLines = cleanKifu.split("\n")
    val tagPattern = "(.*)：(.*)".r
    pprint.log(kifu)
    if (kifu.length > 27) {
      pprint.log(cleanKifu(21))
      pprint.log(cleanKifu(22))
      pprint.log(cleanKifu(23))
      pprint.log(cleanKifu(24))
      pprint.log(cleanKifu(25))
      pprint.log(cleanKifu(26))
      pprint.log(cleanKifu(27))
      pprint.log(kifuByLines)
      pprint.log(kifuByLines(0))
      pprint.log(kifuByLines(1))
      pprint.log(kifuByLines(2))
      pprint.log(kifuByLines(3))
    }

    val tagStr = kifuByLines map {
      case tagPattern(tagName, tagValue) => {
        pprint.log(tagName)
        pprint.log(tagValue)
        (tagParseInverted get tagName)
          .fold(""){(tagEn: TagType) => s"""[$tagEn "$tagValue"]\n""" }
      }
      case _ => ""
    } mkString ""

    def movesStrRecurse(kifuLines: Array[String], i: Integer = 0, moveNum: Integer = 1, movesStr: String = ""): String = {
      if (kifuLines.length == i) return movesStr
      val movePattern = s"\\s*(\\d+)\\s*$destSymbolsPattern$destSymbolsPattern$pieceSymbolsPattern(成?)\\((\\d)(\\d)\\).*".r
      val samePattern = s"\\s*(\\d+)\\s*同　$pieceSymbolsPattern(成?)\\((\\d)(\\d)\\).*".r
      val dropPattern = s"\\s*(\\d+)\\s*$destSymbolsPattern$destSymbolsPattern${pieceSymbolsPattern}打.*".r
      kifuLines(i) match {
        case movePattern(mn, d1, d2, pc, pro, o1, o2) => {
          if (moveNum != mn.toInt) movesStr else {
            val proPlus = if (pro.length > 0) "+" else ""
            val newMovesStr = movesStr + s" ${pieceSymbolsInverted(pc)}${origSymbols1Inverted(o1)}${10 - o2.toInt}${destSymbolsInverted(d1)}${destSymbolsInverted(d2)}${proPlus}"
            movesStrRecurse(kifuLines, i + 1, moveNum + 1, newMovesStr)
          }
        }
        case samePattern(mn, pc, pro, o1, o2) => {
          if (moveNum != mn.toInt) movesStr else {
            val lastDest = movesStr takeRight 3
            val lastDest2 = if ((lastDest takeRight 1) == "+") lastDest take 2 else lastDest takeRight 2
            val d1 = lastDest2 take 1
            val d2 = lastDest2 takeRight 1
            val proPlus = if (pro.length > 0) "+" else ""
            val newMovesStr = movesStr + s" ${pieceSymbolsInverted(pc)}${origSymbols1Inverted(o1)}${10 - o2.toInt}${d1}${d2}${proPlus}"
            movesStrRecurse(kifuLines, i + 1, moveNum + 1, newMovesStr)
          }
        }
        case dropPattern(mn, d1, d2, pc) => {
          if (moveNum != mn.toInt) movesStr else {
            val newMovesStr = movesStr + s" ${pieceSymbolsInverted(pc)}*${destSymbolsInverted(d1)}${destSymbolsInverted(d2)}"
            movesStrRecurse(kifuLines, i + 1, moveNum + 1, newMovesStr)
          }
        }
        case _ => movesStrRecurse(kifuLines, i + 1, moveNum, movesStr)
      }

    }
    val movesStr = movesStrRecurse(kifuByLines)

    s"$tagStr\n$movesStr".pp
  }
}
//Result & Termination <-> saigo no te
