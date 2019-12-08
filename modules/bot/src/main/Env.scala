package lila.bot

import com.softwaremill.macwire._

@Module
final class Env(
    chatApi: lila.chat.ChatApi,
    gameRepo: lila.game.GameRepo,
    lightUserApi: lila.user.LightUserApi,
    rematches: lila.game.Rematches,
    isOfferingRematch: lila.round.IsOfferingRematch
)(implicit system: akka.actor.ActorSystem) {

  private def scheduler = system.scheduler

  lazy val jsonView = wire[BotJsonView]

  lazy val gameStateStream = wire[GameStateStream]

  lazy val player = wire[BotPlayer]

  lazy val onlineBots: OnlineBots = wire[OnlineBots]

  val form = BotForm
}
