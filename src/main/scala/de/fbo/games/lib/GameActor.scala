package de.fbo.games.lib
import akka.actor.Actor
import de.fbo.games.model.Spiel
import de.fbo.games.model.ISpiel
import de.fbo.games.model.Spieler
import akka.actor.ActorRef
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.Timeout
import net.liftweb.util.Helpers._
import net.liftweb.common.Box
import de.fbo.games.lib.UserActor._

/**
 * Actor for Handling a running game.
 * It is initialized with a List of <code>ActorRef</code>s
 * for each player and a Function: <code>Seq[Spieler] => ISpiel<code>,
 * that creates a <code>ISpiel</code> from the obtained <code>Spieler</code>-objects
 */
class GameActor(startSpiel: Seq[Spieler] => ISpiel, spielerActors: Seq[ActorRef]) extends Actor {

  implicit val timeOut = Timeout(20 seconds)

  var game: ISpiel = _

  override def preStart() {
    val spieler = for (
      actor <- spielerActors;
      val future = (actor ? UserActor.GetSpieler).mapTo[Spieler];
      val box = tryo {
        Await.result(future, timeOut.duration)
      } ?~! "Spieler not found";
      spieler <- box
    ) yield spieler

    if (spieler.size != spielerActors.size) {
      //If a player is lost, notify others, then kill yourself
      notifyAll(UserActor.OpponentDead)
      context.stop(self)
    } else {
      game = startSpiel(spieler)
      notifyAll(Start(game))
    }
    super.preStart()
  }

  private def notifyAll(msg: UserActorMessage) {
    spielerActors.foreach(_ ! msg)
  }

  override def receive = {
    case _ =>
  }

}