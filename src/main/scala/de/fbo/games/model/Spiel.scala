package de.fbo.games.model
import net.liftweb.common.{ Box, Full, Empty, Failure }

case class Spieler(name: String)

case class SpielDescriptor[+T <: ISpiel](name: String, noOfPlayers: Int) {
  override def toString = name + " with " + noOfPlayers + " Players"
}

case class Challenge(val creator: Spieler, val spiel: SpielDescriptor[_ <: ISpiel])

/**
 * Fully abstract interface for Spiel
 */
trait ISpiel {

  type Zug

  def spieler: Seq[Spieler]

  def descriptor: SpielDescriptor[ISpiel]

  def scorer: Scorer
}

private[model] trait Spiel extends ISpiel with HasScorer {

  type Runde = Map[Spieler, Zug]

  def spieler: Seq[Spieler]

  def singleton: SpielSingleton[Spiel]

  def descriptor = singleton.descriptor

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

trait SpielSingleton[+T <: Spiel] {
  def descriptor: SpielDescriptor[_ <: T]
}