package de.fbo.games.lib

import akka.actor.actorRef2Scala
import akka.actor.{ ActorRef, Actor }
import de.fbo.games.model.Spieler
case class Challenge(creator: Spieler, spiel: SpielDescriptor)
case class SpielDescriptor(name: String)

object SpielBroker {
  /**
   * Messages
   */
  case class Register(userActor: ActorRef)
  case class ChallengeAccepted(userActor: ActorRef, challenge: Challenge)
  case class CreateChallenge(userActor: ActorRef, challenge: Challenge)
  case class GetChallenges(spieler: Spieler)
}

class SpielBroker extends Actor {

  import SpielBroker._

  var challenges: Seq[Challenge] = Seq.empty

  var users = Seq.empty[ActorRef]

  override def receive = {
    case Register(user) => {
      users = users :+ user
      user ! challenges
    }
    case ChallengeAccepted(user, challenge) => {
      challenges = challenges.filter(_ != challenge)
      users.foreach(_ ! challenges)
    }
    case CreateChallenge(user, challenge) => {
      challenges = challenges :+ challenge
      users.foreach(_ ! challenges)
    }
    case GetChallenges(spieler) =>
      sender ! challenges.filter(_.creator == spieler)
  }

}