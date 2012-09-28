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

class GameActor(startSpiel: Seq[Spieler] => ISpiel, spielerActors: Seq[ActorRef]) extends Actor {

  implicit val timeOut = Timeout(20 seconds)

  var game: ISpiel = _

  override def preStart() {
    println("Created!!!!! with " + spielerActors.size)

    val spieler = for (
      actor <- spielerActors;
      val f = (actor ? UserActor.GetSpieler).mapTo[Spieler];
      val box = tryo {
        Await.result(f, timeOut.duration)
      } ?~! "Spieler not found";
      spieler <- box
    ) yield spieler

    if (spieler.size != spielerActors.size) {
      notifyAll(UserActor.OpponentDead)
      context.stop(self)
    } else {
      game = startSpiel(spieler)
      notifyAll(Start(game))
      println(">>>>>>>>>>>>>>>>>> Spiel gestartet!!! >>>>>>>>>>>>>>>>>>>")
    }
    super.preStart()
  }

  private def notifyAll[T <: UserActorMessage](msg: T) {
    spielerActors.foreach(_ ! msg)
  }

  override def receive = {
    case _ =>
  }

}