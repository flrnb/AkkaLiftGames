package de.fbo.games.lib
import net.liftweb.util.SimpleInjector
import de.fbo.games.model.SpielDescriptor
import de.fbo.games.model.ISpiel
import de.fbo.games.model.Scorer
import de.fbo.games.model.Spieler

object RegistryInjector extends SimpleInjector {
  val registry = new Inject(() => new GameRegistry) {}
}

class GameRegistry {

  type Creator[T <: ISpiel] = (Scorer, Seq[Spieler]) => T

  private var games = Map[SpielDescriptor[_], Creator[_]]()

  def getDescriptions = games.keys

  def addGame[T <: ISpiel](desr: SpielDescriptor[T], gameCreator: Creator[T]): Unit = {
    games = games + (desr -> gameCreator)
  }

  def createGame[T <: ISpiel](desc: SpielDescriptor[T], scorer: Scorer, spieler: Seq[Spieler]): T = {
    games.get(desc) match {
      case Some(f) => f(scorer, spieler).asInstanceOf[T]
      case _ => throw new IllegalArgumentException
    }
  }

}