package lila.explorer

import akka.stream.scaladsl.Sink
import chess.format.pgn.Tag
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.util.Random.nextFloat
import scala.util.{ Try, Success, Failure }

import lila.common.LilaStream
import lila.db.dsl._
import lila.game.{ Game, GameRepo, Query, PgnDump, Player }
import lila.user.{ User, UserRepo }

private final class ExplorerIndexer(
    gameRepo: GameRepo,
    userRepo: UserRepo,
    getBotUserIds: lila.user.GetBotIds,
    ws: play.api.libs.ws.WSClient,
    internalEndpoint: InternalEndpoint
)(implicit mat: akka.stream.Materializer) {

  private val separator = "\n\n\n"
  private val datePattern = "yyyy-MM-dd"
  private val dateFormatter = DateTimeFormat forPattern datePattern
  private val pgnDateFormat = DateTimeFormat forPattern "yyyy.MM.dd";
  private val internalEndPointUrl = s"$internalEndpoint/import/lichess"

  private def parseDate(str: String): Option[DateTime] =
    Try(dateFormatter parseDateTime str).toOption

  def apply(sinceStr: String): Funit = getBotUserIds() flatMap { botUserIds =>
    parseDate(sinceStr).fold(fufail[Unit](s"Invalid date $sinceStr")) { since =>
      logger.info(s"Start indexing since $since")
      val query =
        Query.createdSince(since) ++
          Query.rated ++
          Query.finished ++
          Query.turnsGt(8) ++
          Query.noProvisional ++
          Query.bothRatingsGreaterThan(1501)

      gameRepo
        .sortedCursor(query, Query.sortChronological)
        .documentSource()
        .via(LilaStream.logRate[Game]("fetch")(logger))
        .mapAsyncUnordered(8) { makeFastPgn(_, botUserIds) }
        .mapConcat(_.toList)
        .via(LilaStream.logRate("index")(logger))
        .grouped(50)
        .map(_ mkString separator)
        .mapAsyncUnordered(2) { pgn =>
          ws.url(internalEndPointUrl).put(pgn).flatMap {
            case res if res.status == 200 => funit
            case res => fufail(s"Stop import because of status ${res.status}")
          }
        }
        .to(Sink.ignore)
        .run.void
    }
  }

  def apply(game: Game): Funit = getBotUserIds() flatMap { botUserIds =>
    makeFastPgn(game, botUserIds) map {
      _ foreach flowBuffer.apply
    }
  }

  private object flowBuffer {
    private val max = 30
    private val buf = scala.collection.mutable.ArrayBuffer.empty[String]
    def apply(pgn: String): Unit = {
      buf += pgn
      val startAt = nowMillis
      if (buf.size >= max) {
        ws.url(internalEndPointUrl).put(buf mkString separator) andThen {
          case Success(res) if res.status == 200 =>
            lila.mon.explorer.index.time(((nowMillis - startAt) / max).toInt)
            lila.mon.explorer.index.success(max)
          case Success(res) =>
            logger.warn(s"[${res.status}]")
            lila.mon.explorer.index.failure(max)
          case Failure(err) =>
            logger.warn(s"$err", err)
            lila.mon.explorer.index.failure(max)
        }
        buf.clear
      }
    }
  }

  private def valid(game: Game) =
    game.finished &&
      game.rated &&
      game.turns >= 10 &&
      game.variant != chess.variant.FromPosition &&
      !Game.isOldHorde(game)

  private def stableRating(player: Player) = player.rating ifFalse player.provisional

  // probability of the game being indexed, between 0 and 1
  private def probability(game: Game, rating: Int) = {
    import lila.rating.PerfType._
    game.perfType ?? {
      case Correspondence => 1
      case Rapid | Classical if rating >= 2000 => 1
      case Rapid | Classical if rating >= 1800 => 2 / 5f
      case Rapid | Classical => 1 / 8f
      case Blitz if rating >= 2000 => 1
      case Blitz if rating >= 1800 => 1 / 4f
      case Blitz => 1 / 15f
      case Bullet if rating >= 2300 => 1
      case Bullet if rating >= 2200 => 4 / 5f
      case Bullet if rating >= 2000 => 1 / 4f
      case Bullet if rating >= 1800 => 1 / 7f
      case Bullet => 1 / 20f
      case _ if rating >= 1600 => 1 // variant games
      case _ => 1 / 2f // noob variant games
    }
  }

  private def makeFastPgn(game: Game, botUserIds: Set[User.ID]): Fu[Option[String]] = ~(for {
    whiteRating <- stableRating(game.whitePlayer)
    blackRating <- stableRating(game.blackPlayer)
    minPlayerRating = if (game.variant.exotic) 1400 else 1500
    minAverageRating = if (game.variant.exotic) 1520 else 1600
    if whiteRating >= minPlayerRating
    if blackRating >= minPlayerRating
    averageRating = (whiteRating + blackRating) / 2
    if averageRating >= minAverageRating
    if probability(game, averageRating) > nextFloat
    if !game.userIds.exists(botUserIds.contains)
    if valid(game)
  } yield gameRepo initialFen game flatMap { initialFen =>
    userRepo.usernamesByIds(game.userIds) map { usernames =>
      def username(color: chess.Color) = game.player(color).userId flatMap { id =>
        usernames.find(_.toLowerCase == id)
      } orElse game.player(color).userId getOrElse "?"
      val fenTags = initialFen.?? { fen => List(s"[FEN $fen]") }
      val timeControl = Tag.timeControl(game.clock.map(_.config)).value
      val otherTags = List(
        s"[LichessID ${game.id}]",
        s"[Variant ${game.variant.name}]",
        s"[TimeControl $timeControl]",
        s"[White ${username(chess.White)}]",
        s"[Black ${username(chess.Black)}]",
        s"[WhiteElo $whiteRating]",
        s"[BlackElo $blackRating]",
        s"[Result ${PgnDump.result(game)}]",
        s"[Date ${pgnDateFormat.print(game.createdAt)}]"
      )
      val allTags = fenTags ::: otherTags
      s"${allTags.mkString("\n")}\n\n${game.pgnMoves.take(maxPlies).mkString(" ")}".some
    }
  })

  private val logger = lila.log("explorer")
}
