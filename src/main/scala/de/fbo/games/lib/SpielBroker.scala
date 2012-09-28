package de.fbo.games.lib

import akka.actor.actorRef2Scala
import akka.actor.{ ActorRef, Actor }
import de.fbo.games.model._
import akka.actor.Props
import de.fbo.games.model.BestOfThreeScorer

object SpielBroker {
  /**
   * Messages
   */
  sealed trait BrokerMessage
  case class Register(spieler: Spieler, userActor: ActorRef) extends BrokerMessage
  case class UnRegister(spieler: Spieler) extends BrokerMessage
  case class ChallengeAccepted(userActor: ActorRef, challenge: Challenge) extends BrokerMessage
  case class CreateChallenge(userActor: ActorRef, challenge: Challenge) extends BrokerMessage
  case class GetChallenges(spieler: Spieler) extends BrokerMessage
  case class StartGame(gameActor: ActorRef) extends BrokerMessage
  case class RemoveChallenge(challenge: Challenge) extends BrokerMessage

  case object Abort
}

class ChallengeActor(challenge: Challenge, creator: ActorRef) extends Actor {
  import SpielBroker.{ ChallengeAccepted, RemoveChallenge, Abort }

  var acceptors: Seq[ActorRef] = Seq(creator)

  override def receive = {
    case ChallengeAccepted(actor, challenge) =>
      acceptors = acceptors :+ actor
      if (acceptors.size == challenge.spiel.noOfPlayers) {
        context.actorOf(Props(new GameActor(
          spieler =>
            RegistryInjector.registry.vend.createGame(
              challenge.spiel,
              new BestOfThreeScorer(spieler),
              spieler),
          acceptors)))
        sender ! RemoveChallenge(challenge)
        context.stop(self)
      }
    case Abort => //TODO  
  }
}

class SpielBroker extends Actor {

  import SpielBroker._

  var challenges: Map[Challenge, ActorRef] = Map()

  var users = Map.empty[Spieler, ActorRef]

  override def receive = {

    case UnRegister(spieler) => {
      users = users - spieler
      challenges.foreach {
        case (Challenge(spieler, _), actor) => actor ! Abort
        case _ =>
      }
      challenges = challenges.filterKeys(_.creator != spieler)
      notifyUsers
    }

    case RemoveChallenge(challenge) => {
      challenges = challenges.filterKeys(_ != challenge)
      notifyUsers
    }

    case Register(spieler, user) => {
      users = users + (spieler -> user)
      user ! challenges.keys
    }

    case msg @ ChallengeAccepted(user, challenge) => {
      challenges.get(challenge).map(_ ! msg)
    }

    case CreateChallenge(user, challenge) => {
      val challengeActor = context.actorOf(Props(new ChallengeActor(challenge, user)))
      challenges = challenges + (challenge -> challengeActor)
      notifyUsers
    }

    case GetChallenges(spieler) =>
      sender ! challenges.keys.filter(_.creator == spieler)
  }

  private def notifyUsers(): Unit = {
    users.values.foreach(_ ! challenges.keys)
  }

}