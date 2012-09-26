package bootstrap.liftweb

import akka.actor.ActorSystem
import de.fbo.games.lib.AkkaEnvironment
import de.fbo.games.lib.SpielBroker
import de.fbo.games.lib.SpielBroker
import de.fbo.games.lib.SpielBroker
import de.fbo.games.model._
import de.fbo.games.snippet.SchereSteinPapier
import net.liftweb.common._
import net.liftweb.http.provider._
import net.liftweb.http._
import net.liftweb.mapper.DB
import net.liftweb.mapper.DefaultConnectionIdentifier
import net.liftweb.mapper.Schemifier
import net.liftweb.mapper.StandardDBVendor
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Helpers._
import net.liftweb.util._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("de.fbo.games")
    Schemifier.schemify(true, Schemifier.infoF _, User)

    val gameGroup = If(() => User.loggedIn_?, () => RedirectResponse(User.loginPath.mkString("/")));

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Games") / "index" >> gameGroup,
      Menu("Current") / "curr" >> gameGroup >> Hidden >> Test(_ => SchereSteinPapier.isRunning.get)
        >>
        User.AddUserMenusAfter)

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    LiftRules.noticesAutoFadeOut.default.set((noticeType: NoticeType.Value) => Full((1 seconds, 2 seconds)))

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)

  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
