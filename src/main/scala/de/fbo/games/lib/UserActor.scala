package de.fbo.games.lib
import akka.actor.Actor
import de.fbo.games.comet.ChallengeList
import de.fbo.games.model.{ User, Spieler, ISpiel, Challenge }
import net.liftweb.common.{ Empty, Box, Full }
import net.liftweb.http.CometActor
import akka.actor.ActorRef
import net.liftweb.http.SessionVar
import akka.actor.PoisonPill

object UserActor {

  def broker: ActorRef = AkkaEnvironment.broker.vend
  val system = AkkaEnvironment.system.vend

  object actor extends SessionVar[Box[ActorRef]](Empty) {
    override def onShutdown(session: CleanUpParam) {
      get.foreach(_ ! PoisonPill)
      super.onShutdown(session)
    }
  }

  sealed trait UserActorMessage
  case class ListenToChallenges(comet: CometActor) extends UserActorMessage
  case class ListenToGame(comet: CometActor) extends UserActorMessage
  case class CreateChallenge(c: Challenge) extends UserActorMessage
  case object OpponentDead extends UserActorMessage
  case class Start(spiel: ISpiel) extends UserActorMessage
  case object GetSpieler extends UserActorMessage

}

class UserActor(spieler: Spieler) extends Actor {
  import UserActor._

  var currComet: Box[CometActor] = Empty

  import context._

  lazy val broker = AkkaEnvironment.broker.vend

  override def preStart() {
    broker ! SpielBroker.Register(spieler, self)
    super.preStart()
  }

  override def postStop() {
    broker ! SpielBroker.UnRegister(spieler)
    super.postStop()
  }

  override def receive = {
    case ListenToChallenges(newComet) =>
      currComet = Full(newComet)
      become {
        challengeList
      }
  }

  def challengeList: PartialFunction[Any, Unit] = { msg =>
    msg match {
      case CreateChallenge(c) =>
        broker ! SpielBroker.CreateChallenge(self, c)
      case _: Traversable[_] =>
        currComet.foreach(_ ! msg)
      case GetSpieler =>
        sender ! spieler
      case OpponentDead | Start(_) =>
        currComet.foreach(_ ! msg)
      case ListenToGame(comet) =>
        become {
          gameActios
        }
    }
  }

  def gameActios: PartialFunction[Any, Unit] = {
    case _ => ""
  }
}
