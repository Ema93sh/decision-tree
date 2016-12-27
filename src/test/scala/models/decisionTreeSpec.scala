package com.decisiontree.specs.models

import org.scalatest._
import com.decisiontree.models._

class DecisionSpec extends FunSpec with Matchers {
  describe("Decision Tree") {
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

    it("should create a decision tree with the root node") {
      val willJohnPlay = generateDecisionTree

      willJohnPlay.rootNode.category should be("outlook")
    }

    it("should not contain duplicate categories in any path") {
      val yes = ClassifierNode("Yes")
      val no = ClassifierNode("No")

      val sunnyDecision = Decision("outlook", "sunny")
      val rainyDecision = Decision("outlook", "rainy")
      val overcastDecision = Decision("outlook", "overcast")

      val sunnyLink2 = Link(sunnyDecision, yes)
      val overcastLink2 = Link(overcastDecision, no)
      val outlook2 = DecisionNode("outlook", List(sunnyLink2, overcastLink2))

      val strongDecision = Decision("wind", "strong")
      val weakDecision = Decision("wind", "weak")
      val weakLink = Link(weakDecision, outlook2)
      val strongLink = Link(strongDecision, no)
      val wind = DecisionNode("wind", List(strongLink, weakLink))

      val sunnyLink = Link(sunnyDecision, no)
      val overcastLink = Link(overcastDecision, yes)
      val rainyLink = Link(rainyDecision, wind)
      val outlook = DecisionNode("outlook", List(sunnyLink, overcastLink, rainyLink))

      assertThrows[IllegalArgumentException] {
        DecisionTree(outlook)
      }
    }

   it("should be able to extract paths from a decision tree") {
     val decisiontree = generateDecisionTree
     val paths = decisiontree.extractPaths
     paths.size should be (5)
     val cateoriesInPath = paths.map( path => path.map( l => l.decision.category) :+ path.last.node.asInstanceOf[ClassifierNode].value )
     cateoriesInPath should contain (List("outlook", "humidity", "No"))
     cateoriesInPath should contain (List("outlook", "humidity", "Yes"))
     cateoriesInPath should contain (List("outlook", "wind", "No"))
     cateoriesInPath should contain (List("outlook", "wind", "Yes"))
     cateoriesInPath should contain (List("outlook", "Yes"))
   }

   describe("Classification") {
     it("should classify based on the decision list") {
       val decisionTree = generateDecisionTree
       val decisions = List(
                        Decision("humidity", "low"),
                        Decision("outlook", "sunny")
                      )
       decisionTree.classify(decisions) should be(Some("Yes"))
     }

     it("should return none if decision does not exists") {
       val decisionTree = generateDecisionTree
       val decisions = List(
                        Decision("humidity", "NAN"),
                        Decision("outlook", "sunny")
                      )
       decisionTree.classify(decisions) should be(None)
     }
   }
  }
}
