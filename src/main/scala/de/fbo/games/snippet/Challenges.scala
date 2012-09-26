package de.fbo.games.snippet
import scala.xml.{Text, NodeSeq}

import akka.actor.actorRef2Scala
import akka.dispatch.{Future, Await}
import akka.pattern.ask
import akka.util.Timeout
import de.fbo.games.comet.ChallengeList.CreateChallenge
import de.fbo.games.comet.ChallengeList
import de.fbo.games.lib.{SpielDescriptor, SpielBroker, Challenge, AkkaEnvironment}
import de.fbo.games.model.{User, Spieler}
import net.liftweb.common.{Full, Failure, Empty, Box}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.SHtml
import net.liftweb.util.ConvertableToDate.toMillis
import net.liftweb.util.Helpers.{tryo, strToCssBindPromoter, intToTimeSpanBuilder}
import net.liftweb.util.StringPromotable.jsCmdToStrPromo

class Challenges {

  val naText = Text("Not available")

  def clActor = ChallengeList.actor.is;

  def add(in: NodeSeq): NodeSeq = {
    (for (
      user <- User.currentUser;
      val spieler = user.asSpieler
    ) yield {
      (getChallenges(spieler) match {
        case Failure(msg, _, _) => "#add_new" #> msg
        case Empty => "#add_new [onclick]" #> SHtml.ajaxInvoke(() => {
          createSchereSteinPapierChallenge(spieler)
          JsCmds.Replace("add_new", naText)
        })
        case _ => "#add_new" #> naText
      })(in)
    }) openOr in
  }

  private def createSchereSteinPapierChallenge(spieler: Spieler): Unit =
    clActor.foreach(
      _ ! CreateChallenge(
        Challenge(
          spieler,
          SpielDescriptor("Stein Schere Papier"))))

  private def getChallenges(spieler: Spieler): Box[Seq[Challenge]] = {
    implicit val timeout = Timeout(5 seconds)
    val challenges: Future[Seq[Challenge]] = (AkkaEnvironment.broker.vend ? SpielBroker.GetChallenges(spieler)).mapTo[Seq[Challenge]]
    for (
      box <- tryo {
        Await.result(challenges, timeout.duration) match {
          case Seq() => Empty
          case s => Full(s)
        }
      } ?~! "Challenges not found";
      result <- box
    ) yield result
  }

}