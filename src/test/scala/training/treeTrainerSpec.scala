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
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.001)

  describe("Tree Trainer") {
    describe("Entropy") {
      it("should return the entropy of the column") {
        val classifierData = List("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
                "Yes", "Yes", "Yes", "Yes", "Yes", "No")

        val classifierColumn = new DataColumn[String]("Play Tennis", classifierData)

        val entropy = TreeTrainer.entropy(classifierColumn)

        entropy should equal (0.94)
      }
    }

    describe("Information Gain") {
      it("should get the information gain of the attribute"){
        val windData = List("Weak", "Strong", "Weak", "Weak", "Weak", "Strong",
            "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong")
        val classifierData = List("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
                "Yes", "Yes", "Yes", "Yes", "Yes", "No")

        val windColumn = new DataColumn[String]("StringColumn", windData)
        val classifierColumn = new DataColumn[String]("Play Tennis", classifierData)

        val datatableOption = DataTable("WindTable", Seq(windColumn, classifierColumn))
        val informationGain = TreeTrainer.informationGain(datatableOption.get)

        informationGain should equal (0.049)

      }

      it("should get attribute with maximum information gain") {
        val columnTypes = Map( "Outlook" -> classOf[String],
                               "Temperature" -> classOf[String],
                               "Humidity" -> classOf[String],
                               "Wind" -> classOf[String],
                               "PlayTennis" -> classOf[String]
                             )
        val csvFile = Source.fromURL(getClass.getResource("/willJohnPlayTennis.csv"))
        val datatable = CSVParser.generateDataTable("Play Tennis", csvFile, columnTypes).get
        val attribute = TreeTrainer.attributeWithMaximumInformationGain(datatable, datatable.columns("PlayTennis"))
        attribute should equal ("Outlook")
      }

      it("should return the non classifier column attribute") {
        val windData = List("Weak", "Strong", "Weak", "Weak", "Weak", "Strong",
            "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong")
        val classifierData = List("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
                "Yes", "Yes", "Yes", "Yes", "Yes", "No")

        val windColumn = new DataColumn[String]("Wind", windData)
        val classifierColumn = new DataColumn[String]("Play Tennis", classifierData)

        val datatable = DataTable("Wind Table", Seq(windColumn, classifierColumn)).get
        val attribute = TreeTrainer.attributeWithMaximumInformationGain(datatable, datatable.columns("Play Tennis"))

        attribute should equal ("Wind")
      }
    }

    it("should filter dataTable which contains the value for the column") {
      val windData = List("Weak", "Strong", "Weak", "Weak", "Weak", "Strong",
          "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong")
      val classifierData = List("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
              "Yes", "Yes", "Yes", "Yes", "Yes", "No")

      val windColumn = new DataColumn[String]("Wind", windData)
      val classifierColumn = new DataColumn[String]("Play Tennis", classifierData)

      val datatableOption = DataTable("WindTable", Seq(windColumn, classifierColumn))
      val filterDataTable = TreeTrainer.filterDataTable(datatableOption.get, "Wind", "Strong")

      filterDataTable.rows should have length (6)
      filterDataTable.columns(0).data.toList should be(List("Strong","Strong","Strong","Strong","Strong","Strong"))
      filterDataTable.columns(1).data.toList should be(List("No", "No", "Yes", "Yes", "Yes", "No"))
    }

    it("should generate a decision tree from the datatable") {
      val columnTypes = Map( "Outlook" -> classOf[String],
                             "Temperature" -> classOf[String],
                             "Humidity" -> classOf[String],
                             "Wind" -> classOf[String],
                             "PlayTennis" -> classOf[String]
                           )
      val csvFile = Source.fromURL(getClass.getResource("/willJohnPlayTennis.csv"))
      val datatable = CSVParser.generateDataTable("Play Tennis", csvFile, columnTypes).get.columns.remove("Temperature").get
      val trainer = TreeTrainer(datatable, datatable.columns("PlayTennis"))
      val decisionTree = trainer.generateDecisionTree
      val expectedDecisionTree = generateDecisionTree

      assert(validateDecisionTree(decisionTree.rootNode, expectedDecisionTree.rootNode))
    }

    def validateDecisionTree(rootNode1: Node, rootNode2: Node): Boolean  = {
      (rootNode1, rootNode2) match {
          case (DecisionNode(category1, links1),  DecisionNode(category2, links2))
                => {
                  category1 == category2 &&
                   links1.forall( link1 => {
                     val link2 = links2.find(link2 => link2.decision == link1.decision).get
                     validateDecisionTree(link1.node, link2.node)
                   })
                 }
          case (ClassifierNode(value1), ClassifierNode(value2)) => value1 == value2
          case _ => false
      }
    }

    def generateDecisionTree = {
      val yes = ClassifierNode("Yes")
      val no = ClassifierNode("No")

      val strongDecision = Decision("Wind", "Strong")
      val weakDecision = Decision("Wind", "Weak")
      val weakLink = Link(weakDecision, yes)
      val strongLink = Link(strongDecision, no)
      val wind = DecisionNode("Wind", List(strongLink, weakLink))

      val lowDecision = Decision("Humidity", "Normal")
      val highDecision = Decision("Humidity", "High")
      val lowLink = Link(lowDecision, yes)
      val highLink = Link(highDecision, no)
      val humidity = DecisionNode("Humidity", List(highLink, lowLink))

      val sunnyDecision = Decision("Outlook", "Sunny")
      val rainyDecision = Decision("Outlook", "Rain")
      val overcastDecision = Decision("Outlook", "Overcast")
      val sunnyLink = Link(sunnyDecision, humidity)
      val overcastLink = Link(overcastDecision, yes)
      val rainyLink = Link(rainyDecision, wind)
      val outlook = DecisionNode("Outlook", List(sunnyLink, overcastLink, rainyLink))

      DecisionTree(outlook)
    }
  }
}
