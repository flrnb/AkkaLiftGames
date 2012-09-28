package de.fbo.games.comet
import net.liftweb.http.SessionVar
import net.liftweb.http.CometActor
import de.fbo.games.model.SchereSteinPapier
import net.liftweb.http.S

object GameState {
  object isRunning extends SessionVar[Boolean](false)
}

class SchereSteinPapierComet extends CometActor {
  def render = "@gameName" #> S.??(SchereSteinPapier.descriptor.name)
}