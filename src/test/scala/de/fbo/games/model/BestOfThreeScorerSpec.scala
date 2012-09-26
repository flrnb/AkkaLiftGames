package de.fbo.games.model

import org.specs2.mutable.SpecificationWithJUnit
import net.liftweb.common.{ Empty, Box, Full }
import scala.collection.mutable
import org.specs2.runner.JUnitRunner
import org.junit.internal.runners.JUnit4ClassRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class BestOfThreeScorerSpec extends SpecificationWithJUnit with SpielTestHelper {

  val prefix = "A Best-Of-Three-Spiel with 2 players"

  prefix + " in the beginning" should {
    val spiel = DummySpielAblauf(2).spiel
    "be not entschieden" in {
      !spiel.isEntschieden
    }
    "have no Gewinner" in {
      spiel.scorer.getGewinner must be empty
    }
    "have 2 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (2)
    }
  }

  prefix + ", where player 1 wins the first round" should {
    val spiel = DummySpielAblauf(2).win(1).spiel
    "be not entschieden" in {
      !spiel.isEntschieden
    }
    "have no Gewinner" in {
      spiel.scorer.getGewinner must be empty
    }
    "have 1 Fuehrenden" in {
      spiel.scorer.getFuehrende must have size (1)
      spiel.scorer.getFuehrende must contain(spiel.spieler.head)
    }
  }

  prefix + ", where each player wins a round" should {
    val spiel = DummySpielAblauf(2).win(1).win(2).spiel
    "be not entschieden" in {
      !spiel.isEntschieden
    }
    "have no Gewinner" in {
      spiel.scorer.getGewinner must be empty
    }
    "have 2 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (2)
    }
  }

  prefix + ", with 3 times unentschieden" should {
    val spiel = DummySpielAblauf(2).unentschieden.unentschieden.unentschieden.spiel
    "be not entschieden" in {
      !spiel.isEntschieden
    }
    "have no Gewinner" in {
      spiel.scorer.getGewinner must be empty
    }
    "have 2 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (2)
    }
  }

  prefix + ", where player 1 wins 3 times" should {
    val spiel = DummySpielAblauf(2).win(1).win(1).win(1).spiel
    "be entschieden" in {
      spiel.isEntschieden
    }
    "have 1 Gewinner" in {
      spiel.scorer.getGewinner must not be empty
      spiel.scorer.getGewinner mustEqual Full(spiel.spieler.head)
    }
    "have 1 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (1)
      spiel.scorer.getFuehrende must contain(spiel.spieler.head)
    }
  }

  prefix + ", where player 1 wins 2 times and has 1 unentschieden" should {
    val spiel = DummySpielAblauf(2).win(1).unentschieden.win(1).spiel
    "not be entschieden" in {
      !spiel.isEntschieden
    }
    "have no Gewinner" in {
      spiel.scorer.getGewinner must be empty
    }
    "have 1 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (1)
      spiel.scorer.getFuehrende must contain(spiel.spieler.head)
    }
  }

  prefix + ", where player 1 wins 2 times and player 2 once" should {
    val spiel = DummySpielAblauf(2).win(1).win(1).unentschieden.win(2).spiel
    "be entschieden" in {
      spiel.isEntschieden
    }
    "have 1 Gewinner" in {
      spiel.scorer.getGewinner must not be empty
      spiel.scorer.getGewinner mustEqual Full(spiel.spieler.head)
    }
    "have 1 Fuehrende" in {
      spiel.scorer.getFuehrende must have size (1)
      spiel.scorer.getFuehrende must contain(spiel.spieler.head)
    }
  }
}