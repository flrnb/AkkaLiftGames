package de.fbo.games.lib

import akka.actor.actorRef2Scala
import akka.actor.{ ActorRef, Actor }
import de.fbo.games.model.Spieler
import de.fbo.games.model.SpielDescriptor
import de.fbo.games.model.Spiel
case class Challenge(creator: Spieler, spiel: SpielDescriptor[_ <: Spiel])

object SpielBroker {
  /**
   * Messages
   */
  case class Register(userActor: ActorRef)
  case class ChallengeAccepted[S](userActor: ActorRef, challenge: Challenge)
  case class CreateChallenge[S](userActor: ActorRef, challenge: Challenge)
  case class GetChallenges[S](spieler: Spieler)
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