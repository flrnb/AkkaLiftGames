package de.fbo.games.model
import net.liftweb.common.{ Empty, Box }
import scala.collection.SortedMap

trait SpielTestHelper {

  trait DummyHasScorer extends HasScorer {
    this: ISpiel =>

    def scorer = new Scorer {
      def isEntschieden: Boolean = false
      def unentschieden: Unit = {}
      def gewinner(spieler: Seq[Spieler]): Unit = {}
      def getFuehrende: Seq[Spieler] = spieler
    }
  }

  abstract class DummySpiel(noOfSpieler: Int) extends Spiel {
    this: HasScorer =>
    override def singleton = new SpielSingleton[DummySpiel] {
      override def descriptor = SpielDescriptor[DummySpiel]("Dummy")
    }
    override def spieler = for (i <- 1 to noOfSpieler) yield (Spieler("Spieler" + i))
    override def getGewinner(runde: Runde) = runde.headOption map (_._1) toSeq

    case object DummyZug

    override type Zug = DummyZug.type
  }

  case class DummySpielAblauf(no: Int)(implicit val spiel: DummySpiel = new DummySpiel(no) with HasBestOfThreeScorer) {
    var lastResult: Box[Seq[Spieler]] = Empty
    def win(n: Int): DummySpielAblauf = {
      require(n > 0 && n <= no)
      var runde = Seq(spiel.spieler(n - 1) -> spiel.DummyZug)
      runde = ((1 to no).filter(_ != n).foldLeft(runde))((r, i) => (r :+ (spiel.spieler(i - 1) -> spiel.DummyZug)))
      spiel.getResult(runde.toMap)
      this
    }
    def unentschieden(): DummySpielAblauf = {
      lastResult = spiel.getResult(Map())
      this
    }
    def result = lastResult
  }

}