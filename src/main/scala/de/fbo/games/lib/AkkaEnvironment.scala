package de.fbo.games {
  package lib {

    import net.liftweb.util.SimpleInjector
    import akka.actor.ActorSystem
    import akka.actor.Props

    object AkkaEnvironment extends SimpleInjector {
      env =>
      private val mySystem = ActorSystem("system")
      val system = new Inject(() => env.inject[ActorSystem] openOr mySystem) {}
      private lazy val myBroker = system.vend.actorOf(Props[SpielBroker], name = "broker")
      val broker = new Inject(() => myBroker) {}
    }

    /*
/**
 * Examples of changing the implementation
 */
sealed abstract class Changer {
  def changeDefaultImplementation() {
    DependencyFactory.time.default.set(() => new Date())
  }

  def changeSessionImplementation() {
    DependencyFactory.time.session.set(() => new Date())
  }

  def changeRequestImplementation() {
    DependencyFactory.time.request.set(() => new Date())
  }

  def changeJustForCall(d: Date) {
    DependencyFactory.time.doWith(d) {
      // perform some calculations here
    }
  }
}
*/
  }
}
