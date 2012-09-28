package de.fbo.games.comet
import scala.xml.NodeSeq

import akka.actor.actorRef2Scala
import akka.actor.Props
import de.fbo.games.lib.{ UserActor, SpielBroker, AkkaEnvironment }
import de.fbo.games.model.{ User, Challenge }
import net.liftweb.common.Full
import net.liftweb.http.js.JsCmd.unitToJsCmd
import net.liftweb.http.js.jquery.JqWiringSupport
import net.liftweb.http.js.JsCmds
import net.liftweb.http.{ WiringUI, SHtml, S, CometActor }
import net.liftweb.util.StringPromotable.jsCmdToStrPromo
import net.liftweb.util.ValueCell

object ChallengeList {
  case object GameStarted
}

class ChallengeList extends CometActor {
  comet =>

  override def localSetup() {
    val spieler = User.currentUser.open_!.asSpieler
    val uActor = AkkaEnvironment.system.vend.actorOf(Props(new UserActor(spieler)))
    uActor ! UserActor.ListenToChallenges(this)
    UserActor.actor.set(Full(uActor))
    super.localSetup
  }

  val challengeList = ValueCell[Seq[Challenge]](Seq())

  override def lowPriority = {
    case challenges: Traversable[Challenge] =>
      challengeList.atomicUpdate(_ => challenges.toSeq)
      reRender(false)
    case UserActor.OpponentDead =>
      partialUpdate(JsCmds.Alert("Game aborted") &
        JsCmds.RedirectTo("/"))
    case UserActor.Start(spiel) =>
      GameState.isRunning.atomicUpdate(_ => true)
      partialUpdate(JsCmds.Confirm(S.??(spiel.descriptor.name) + "?",
        JsCmds.RedirectTo("/curr")))

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
    UserActor.actor.is.foreach(actor => {
      AkkaEnvironment.broker.vend ! SpielBroker.ChallengeAccepted(actor, c)
      S.notice("Accepted Challenge from " + c.creator.name)
    })
  }
}