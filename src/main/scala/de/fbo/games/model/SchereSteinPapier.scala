package de.fbo.games.model

object SchereSteinPapier extends SpielSingleton[SchereSteinPapier] {

  override def descriptor = SpielDescriptor[SchereSteinPapier]("ssp", 2)

  sealed trait SSPZug {
    def schlaegt: SSPZug
  }
  case object Schere extends SSPZug {
    override val schlaegt = Papier
  }
  case object Papier extends SSPZug {
    override val schlaegt = Stein
  }
  case object Stein extends SSPZug {
    override val schlaegt = Schere
  }
}

abstract class SchereSteinPapier(spieler1: Spieler, spieler2: Spieler) extends Spiel {

  override val singleton: SpielSingleton[SchereSteinPapier] = SchereSteinPapier

  override val spieler = Seq(spieler1, spieler2)

  protected def getGewinner(runde: Runde) =
    (for (
      zug1 <- runde.get(spieler1);
      zug2 <- runde.get(spieler2);
      if !(zug1 == zug2)
    ) yield {
      if (zug1.schlaegt == zug2)
        spieler1
      else
        spieler2
    }).toSeq

  override type Zug = SchereSteinPapier.SSPZug
}