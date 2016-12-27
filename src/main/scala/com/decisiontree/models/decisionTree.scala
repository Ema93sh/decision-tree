package com.decisiontree.models


case class DecisionTree(rootNode: DecisionNode) {
  type Path = List[Link]

  require(uniqueCategoryInPath)

  def extractPaths = {
    def _extractPaths(node: Node): List[Path] = node match {
          case ClassifierNode(value) => List(List())
          case DecisionNode(_, links) => {
            val paths = for (link <- links)
               yield _extractPaths(link.node).map(path => link +: path)
            paths.flatten
        }
    }

    _extractPaths(rootNode)
  }

  //TODO: Refactor to compare paths without extracting categories
  def uniqueCategoryInPath = extractPaths.forall(path => {
    val categories = path.map( l => l.decision.category) :+ path.last.node.asInstanceOf[ClassifierNode].value
    categories.distinct.size == categories.size
  })

  def getLinkFor(category: String, links: List[Link], decisions: List[Decision]) = {
     decisions.find(d => d.category == category) match {
       case Some(d) => links.find(l => l.decision == d)
       case None => None
     }
  }

  def classify(decisions: List[Decision]): Option[String] = {
    def _classify(node: Node): Option[String] = {
        node match {
          case ClassifierNode(value) => Some(value)
          case DecisionNode(category, links) =>
             getLinkFor(category, links, decisions) match {
               case Some(l) => _classify(l.node)
               case None => None
             }
        }
    }
    _classify(rootNode)
  }
}
