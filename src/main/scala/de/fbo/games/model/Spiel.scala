package de.fbo.games.model
import net.liftweb.common.{ Box, Full, Empty, Failure }

case class Spieler(name: String)
case class SpielDescriptor[T <: ISpiel](name: String)

/**
 * Fully abstract interface for Spiel
 */
trait ISpiel {

  type Zug

  def getResult(runde: Map[Spieler, Zug]): Box[Seq[Spieler]]

  def spieler: Seq[Spieler]

  def scorer: Scorer
}

trait Spiel extends ISpiel with HasScorer {

  type Runde = Map[Spieler, Zug]

  def spieler: Seq[Spieler]

  def scorer: Scorer

  def singleton: SpielSingleton[Spiel]

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