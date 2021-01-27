package lila.pool

import play.api.libs.json.Json
import scala.concurrent.duration._

object PoolList {

  import PoolConfig._

  val all: List[PoolConfig] = List(
    PoolConfig(0 ++ (0, 10, 0), Wave(12 seconds, 40 players))
    PoolConfig(3 ++ (0, 0, 0), Wave(12 seconds, 40 players))
    PoolConfig(5 ++ (0, 30, 0), Wave(12 seconds, 40 players))
    PoolConfig(10 ++ (0, 0, 0), Wave(12 seconds, 40 players))
    PoolConfig(15 ++ (0, 60, 0), Wave(12 seconds, 40 players))
    PoolConfig(30 ++ (0, 60, 0), Wave(12 seconds, 40 players))
  )

  val clockStringSet: Set[String] = all.view.map(_.clock.show) to Set

  val json = Json toJson all

  implicit private class PimpedInt(self: Int) {
    def ++(increment: Int, byoyomi: Int, periods: Int) = chess.Clock.Config(self * 60, increment, byoyomi, periods)
    def players            = NbPlayers(self)
  }
}
