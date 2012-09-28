package de.fbo.games.model
import net.liftweb.common.Empty
import net.liftweb.common.Box

trait Scorer {

  def spieler: Seq[Spieler]

  def isEntschieden: Boolean
  def unentschieden: Unit
  def gewinner(spieler: Seq[Spieler]): Unit
  def getFuehrende: Seq[Spieler]
  def getGewinner: Box[Spieler] = {
    if (!isEntschieden)
      Empty
    else {
      require(getFuehrende.size == 1)
      getFuehrende.headOption
    }
  }
}

trait HasScorer {
  this: ISpiel =>
  val scorer: Scorer
}

trait HasBestOfThreeScorer extends HasScorer {
  this: ISpiel =>
  override val scorer = new BestOfThreeScorer(spieler) {}
}

/**
 * Example Scorer-Implementation for Best-Of-Three
 */
class BestOfThreeScorer(val spieler: Seq[Spieler]) extends Scorer {

  import scala.collection.mutable

  private val stand: mutable.Map[Spieler, Int] = (mutable.Map[Spieler, Int]() /: spieler)((map, s) => map += s -> 0)

  private var runden = 0

  def gewinner(spieler: Seq[Spieler]) = {
    runden = runden + 1
    spieler.foreach(s => stand.update(s, stand.get(s).get + 1))
  }

  def unentschieden = {
    //tue nichts 
  }
  def isEntschieden = runden >= 3 && getFuehrende.size == 1
  def getFuehrende = (spieler.tail foldLeft List(spieler.head))((r, s) => {
    val (maxPunkte, sPunkte) = (stand(r.head), stand(s))
    if (sPunkte > maxPunkte) List(s)
    else if (sPunkte == maxPunkte) s :: r
    else r
  })
}