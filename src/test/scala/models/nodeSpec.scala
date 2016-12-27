package com.decisiontree.specs.models

import org.scalatest._
import org.scalatest.FunSpec
import com.decisiontree.models._

class NodeSpec extends FunSpec with Matchers {
  describe("Decision") {
    it("should have a category and a value") {
      val decision = Decision("weather", "sunny")
      decision.category should be ("weather")
      decision.value should be ("sunny")
    }
  }

  describe("Link"){
    it("should have a decision and a node"){
      val sunnyDecision = Decision("weather","sunny")
      val windNode = DecisionNode("wind",List())
      val yesNode = ClassifierNode("Yes")

      val sunnyLink = Link(sunnyDecision, windNode)
      sunnyLink.decision should be(sunnyDecision)
      sunnyLink.node should be(windNode)

      val yesLink = Link(sunnyDecision, yesNode)
      yesLink.decision should be(sunnyDecision)
      yesLink.node should be(yesNode)
    }
  }

  describe("Decision Node") {
    it("should have a category") {
      val weatherNode = DecisionNode("weather", List())
      weatherNode.category shouldBe("weather")
    }

    it("should have a decision link") {
      val humidityCategory = DecisionNode("Humidity", List())
      val decision  = Decision("weather", "sunny")

      val sunnyDecision = Link(decision, humidityCategory)
      val weatherNode = DecisionNode("weather", List(sunnyDecision))


      weatherNode.links should be (List(sunnyDecision))
    }

    it("can have mulitple decision links") {
      val humidityCategory = DecisionNode("Humidity", List())
      val windCategory = DecisionNode("Wind", List())

      val sunny  = Decision("weather", "sunny")
      val cloudy  = Decision("weather", "cloudy")

      val sunnyLink = Link(sunny, humidityCategory)
      val cloudyLink = Link(cloudy, windCategory)

      val weatherCategory = DecisionNode("weather", List(sunnyLink, cloudyLink))
      weatherCategory.links should be (List(sunnyLink, cloudyLink))
    }

    it("should have only distinct decision links") {
      val humidityCategory = DecisionNode("Humidity", List())
      val sunny1  = Decision("weather", "sunny")
      val sunny2  = Decision("weather", "sunny")

      val sunnyLink1 = Link(sunny1, humidityCategory)
      val sunnyLink2 = Link(sunny2, humidityCategory)

      assertThrows[IllegalArgumentException] {
          DecisionNode("weather", List(sunnyLink1, sunnyLink2))
      }
    }
  }

  describe("Classifier Node") {
    it("should have a value") {
      val yesClassifier = ClassifierNode("Yes")
      yesClassifier.value should be ("Yes")
    }
  }
}
