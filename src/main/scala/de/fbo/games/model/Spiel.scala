package de.fbo.games.model
import net.liftweb.common.{ Box, Full, Empty, Failure }

case class Spieler(name: String)

trait ISpiel {

  type Zug

  def getResult(runde: Map[Spieler, Zug]): Box[Seq[Spieler]]
  def spieler: Seq[Spieler]
}

trait Spiel extends ISpiel with HasScorer {

  type Runde = Map[Spieler, Zug]

  def spieler: Seq[Spieler]

  def scorer: Scorer

  protected def getGewinner(runde: Runde): Seq[Spieler]

  def getResult(runde: Runde): Box[Seq[Spieler]] =
    if (runde.size != spieler.size)
      Failure("Nicht alle Spieler haben Zuege gemacht")
    else
      getGewinner(runde) match {
        case Seq() =>
          scorer.unentschieden
          Empty
        case gewinner =>
          scorer.gewinner(gewinner)
          Full(gewinner)
      }

  def isEntschieden = scorer.isEntschieden
}

object SchereSteinPapier {

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