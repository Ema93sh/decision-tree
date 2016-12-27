package com.decisiontree.models

trait Node {
}

case class Decision(category: String, value: String)

case class Link(decision:Decision, node:Node)

case class DecisionNode(category: String, links: List[Link]) extends Node {
  require(links.map(_.decision.value).distinct.size == links.size)
}

case class ClassifierNode(value: String) extends Node
