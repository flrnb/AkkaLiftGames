package de.fbo.games.model
import scala.Option.option2Iterable
import net.liftweb.common.{ Box, Empty }

trait SpielTestHelper {

  trait MockHasScorer extends HasScorer {
    spiel: ISpiel =>

    val scorer = new Scorer {
      val spieler = spiel.spieler
      def isEntschieden: Boolean = false
      def unentschieden: Unit = {}
      def gewinner(spieler: Seq[Spieler]): Unit = {}
      def getFuehrende: Seq[Spieler] = spieler
    }
  }

  abstract class MockSpiel(noOfSpieler: Int) extends Spiel {
    this: HasScorer =>
    override def singleton = new SpielSingleton[MockSpiel] {
      override def descriptor = SpielDescriptor[MockSpiel]("Dummy", noOfSpieler)
    }
    override def spieler = for (i <- 1 to noOfSpieler) yield (Spieler("Spieler" + i))
    override def getGewinner(runde: Runde) = runde.headOption map (_._1) toSeq

    case object DummyZug

    override type Zug = DummyZug.type
  }

  case class MockSpielAblauf(no: Int)(implicit val spiel: MockSpiel = new MockSpiel(no) with HasBestOfThreeScorer, val result: Box[Seq[Spieler]] = Empty) {
    def win(n: Int): MockSpielAblauf = {
      require(n > 0 && n <= no)
      val gewinner = Seq(spiel.spieler(n - 1) -> spiel.DummyZug)
      val result = spiel.getResult(((1 to no).filter(_ != n).foldLeft(gewinner))((r, i) => (r :+ (spiel.spieler(i - 1) -> spiel.DummyZug))).toMap)
      MockSpielAblauf(no)(spiel, result)
    }
    def unentschieden(): MockSpielAblauf = {
      MockSpielAblauf(no)(spiel, spiel.getResult(Map()))
    }
  }

}