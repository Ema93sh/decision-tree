package com.decisiontree.specs.training

import scala.io.Source

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.FunSpec
import com.github.martincooper.datatable._

import com.decisiontree.training._
import com.decisiontree.models._
import org.scalactic._

class TreeTrainerSpec extends FunSpec with Matchers {
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

  describe("Tree Trainer") {
    describe("Entropy") {
      it("should return the entropy of the column") {
        val outlookData = List("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast",
             "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain")

        val classifierData = List("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
                "Yes", "Yes", "Yes", "Yes", "Yes", "No")

        val outlookColumn = new DataColumn[String]("StringColumn", outlookData)
        val classifierColumn = new DataColumn[String]("Play Tennis", classifierData)

        val datatableOption = DataTable("OutlookTable", Seq(outlookColumn, classifierColumn))
        val entropy = TreeTrainer.getEntropy(datatableOption.get)

        entropy should equal (0.6935361)
      }
    }

    describe("Information Gain") {
      it("should get attribute with maximum information gain") {
        val columnTypes = Map( "Day" -> classOf[String],
                               "Outlook" -> classOf[String],
                               "Temperature" -> classOf[String],
                               "Humidity" -> classOf[String],
                               "Wind" -> classOf[String],
                               "PlayTennis" -> classOf[String]
                             )
        val csvFile = Source.fromURL(getClass.getResource("/willJohnPlayTennis.csv"))
        val datatable = CSVParser.generateDataTable("Play Tennis", csvFile, columnTypes).get
        val attribute = TreeTrainer.getAttributeWithMaximumInformationGain(datatable)
        attribute should equal ("Outlook")
      }
    }

    it("should generate a decision tree from the datatable") {
      val columnTypes = Map( "Day" -> classOf[String],
                             "Outlook" -> classOf[String],
                             "Temperature" -> classOf[String],
                             "Humidity" -> classOf[String],
                             "Wind" -> classOf[String],
                             "PlayTennis" -> classOf[String]
                           )
      val csvFile = Source.fromURL(getClass.getResource("/willJohnPlayTennis.csv"))
      val datatable = CSVParser.generateDataTable("Play Tennis", csvFile, columnTypes).get
      val trainer = TreeTrainer(datatable, "PlayTennis")
      val decisionTree = trainer.generateDecisionTree
      val expectedDecisionTree = generateDecisionTree

      decisionTree.rootNode should equal (expectedDecisionTree.rootNode)
    }

    def generateDecisionTree = {
      val yes = ClassifierNode("Yes")
      val no = ClassifierNode("No")

      val strongDecision = Decision("wind", "strong")
      val weakDecision = Decision("wind", "weak")
      val weakLink = Link(weakDecision, yes)
      val strongLink = Link(strongDecision, no)
      val wind = DecisionNode("wind", List(strongLink, weakLink))

      val lowDecision = Decision("humidity", "low")
      val highDecision = Decision("humidity", "high")
      val lowLink = Link(lowDecision, yes)
      val highLink = Link(highDecision, no)
      val humidity = DecisionNode("humidity", List(highLink, lowLink))

      val sunnyDecision = Decision("outlook", "sunny")
      val rainyDecision = Decision("outlook", "rainy")
      val overcastDecision = Decision("outlook", "overcast")
      val sunnyLink = Link(sunnyDecision, humidity)
      val overcastLink = Link(overcastDecision, yes)
      val rainyLink = Link(rainyDecision, wind)
      val outlook = DecisionNode("outlook", List(sunnyLink, overcastLink, rainyLink))

      DecisionTree(outlook)
    }
  }
}
