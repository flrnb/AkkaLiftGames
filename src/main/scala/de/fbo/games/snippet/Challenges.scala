package de.fbo.games.snippet
import scala.xml.{ Text, NodeSeq }
import akka.actor.actorRef2Scala
import akka.dispatch.{ Future, Await }
import akka.util.Timeout
import de.fbo.games.comet.ChallengeList
import de.fbo.games.lib.{ SpielBroker, AkkaEnvironment }
import de.fbo.games.model.{ User, Spieler, SchereSteinPapier, Challenge }
import net.liftweb.common.{ Full, Failure, Empty, Box }
import net.liftweb.http.js.JsCmds
import net.liftweb.http.SHtml
import net.liftweb.util.ConvertableToDate.toMillis
import net.liftweb.util.Helpers.{ tryo, strToCssBindPromoter, intToTimeSpanBuilder }
import net.liftweb.util.StringPromotable.jsCmdToStrPromo
import de.fbo.games.lib.UserActor
import net.liftweb.http.S.?

class Challenges {

  val naText = Text("Not available")

  def uActor = UserActor.actor.is;

  def add(in: NodeSeq): NodeSeq = {
    (for (
      user <- User.currentUser;
      val spieler = user.asSpieler
    ) yield {
      val cssSel = getChallenges(spieler) match {
        case Failure(msg, _, _) =>
          "#add_new" #> msg
        case Empty =>
          "#add_new *" #> ?(SchereSteinPapier.descriptor.name) &
            "#add_new [onclick]" #> SHtml.ajaxInvoke(() => {
              createSchereSteinPapierChallenge(spieler)
              JsCmds.Replace("add_new", naText)
            })
        case _ => "#add_new" #> naText
      }
      cssSel(in)
    }) openOr in
  }

  private def createSchereSteinPapierChallenge(spieler: Spieler): Unit =
    uActor.foreach(
      _ ! UserActor.CreateChallenge(
        Challenge(
          spieler,
          SchereSteinPapier.descriptor)))

  private def getChallenges(spieler: Spieler): Box[Seq[Challenge]] = {
    import akka.pattern.ask
    implicit val timeout = Timeout(5 seconds)
    val challenges = (AkkaEnvironment.broker.vend ?
      SpielBroker.GetChallenges(spieler)).mapTo[Traversable[Challenge]]
    for (
      seq <- tryo {
        Await.result(challenges, timeout.duration).toSeq
      } ?~! "Challenges not found";
      if (!seq.isEmpty)
    ) yield seq
  }
}