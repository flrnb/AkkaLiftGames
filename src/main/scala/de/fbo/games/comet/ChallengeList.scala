package de.fbo.games.comet
import scala.annotation.serializable
import scala.xml.NodeSeq
import de.fbo.games.lib.Challenge
import de.fbo.games.lib.SpielBroker
import de.fbo.games.model.User
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.http.js.JsCmd.unitToJsCmd
import net.liftweb.http.js.jquery.JqWiringSupport
import net.liftweb.http.SessionVar
import net.liftweb.http.CometActor
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.WiringUI
import net.liftweb.util.StringPromotable.jsCmdToStrPromo
import net.liftweb.util.ValueCell
import net.liftweb.common.Full
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import de.fbo.games.lib.AkkaEnvironment
import net.liftweb.http.js.JsCmds

object ChallengeList {
  case class CreateChallenge(c: Challenge)

  def broker: ActorRef = AkkaEnvironment.broker.vend
  val system = AkkaEnvironment.system.vend

  object actor extends SessionVar[Box[ActorRef]](Empty)
}

class ChallengeListActor(comet: CometActor) extends Actor {
  import ChallengeList._
  override def receive = {
    case CreateChallenge(c) =>
      broker ! SpielBroker.CreateChallenge(self, c)
    case challenges: Seq[_] =>
      comet ! challenges
  }
}

class ChallengeList extends CometActor {
  comet =>

  import ChallengeList._

  override def localSetup() {
    val clActor = system.actorOf(Props(new ChallengeListActor(this)))
    actor.set(Full(clActor))
    broker ! SpielBroker.Register(clActor)
    super.localSetup
  }

  val challengeList = ValueCell[Seq[Challenge]](Seq())

  override def lowPriority = {
    case challenges: Seq[Challenge] =>
      challengeList.atomicUpdate(_ => challenges)
      reRender(false)
  }

  override def render = {
    "#challenges" #> (
      "tbody" #> findOrCreateId(id =>
        WiringUI.history(challengeList) {
          (oldValues, newValues, ns) =>
            {
              val theTR = ("tr ^^" #> "**")(ns)
              def challengeToId(c: Challenge) = "c_" + c.creator.hashCode + c.spiel.hashCode
              def html(c: Challenge): NodeSeq = {
                (
                  "tr [id]" #> challengeToId(c) &
                  "@name *" #> c.creator.name &
                  "td *" #> (if (User.currentUser.open_!.firstName.is == c.creator.name) {
                    "@join" #> "Join"
                  } else {
                    "@join [onclick]" #> SHtml.ajaxInvoke(() => acceptChallenge(c))
                  }))(theTR)
              }
              JqWiringSupport.calculateDeltas(oldValues, newValues, id)(challengeToId _, html _)
            }
        }))
  }

  private def acceptChallenge(c: Challenge) = {
    ChallengeList.actor.is.foreach(actor => {
      broker ! SpielBroker.ChallengeAccepted(actor, c)
      S.notice("Accepted Challenge from " + c.creator.name)
    })
  }
}