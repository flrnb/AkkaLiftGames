package de.fbo.games.model
import org.specs2.mutable._
import SchereSteinPapier._
import net.liftweb.common.Full
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SchereSteinPapierSpielTest extends SpecificationWithJUnit with SpielTestHelper {

  "In a Stein-Schere-Papier-Spiel" should {
    val s1 = Spieler("Spieler1")
    val s2 = Spieler("Spieler2")
    val spiel = new SchereSteinPapier(s1, s2) with MockHasScorer
    "Stein beat Schere" in {
      spiel.getResult(Map(s1 -> Stein, s2 -> Schere)) mustEqual Full(Seq(s1))
    }
    "Schere beat Papier" in {
      spiel.getResult(Map(s1 -> Schere, s2 -> Papier)) mustEqual Full(Seq(s1))
    }
    "Papier beat Stein" in {
      spiel.getResult(Map(s1 -> Stein, s2 -> Papier)) mustEqual Full(Seq(s2))
    }
    "Stein not beat Stein" in {
      spiel.getResult(Map(s1 -> Stein, s2 -> Stein)) must be empty
    }
    "Schere not beat Schere" in {
      spiel.getResult(Map(s1 -> Schere, s2 -> Schere)) must be empty
    }
    "Papier not beat Papier" in {
      spiel.getResult(Map(s1 -> Papier, s2 -> Papier)) must be empty
    }
  }

}