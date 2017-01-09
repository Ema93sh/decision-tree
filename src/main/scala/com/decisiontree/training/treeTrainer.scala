package com.decisiontree.training

import com.github.martincooper.datatable._

import com.decisiontree.models._


case class TreeTrainer(datatable: DataTable, classifierColumn: String) {

  def generateDecisionTree = {
    val node = new DecisionNode("test", List())
    DecisionTree(node)
  }
}

object  TreeTrainer {
  val lnOf2 = scala.math.log(2)
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

  def getAttributeWithMaximumInformationGain(datatable: DataTable) = {

  }

  def getEntropy(datatable: DataTable) = {
    // datatable contains a table with two column and classifier column
    val columnGroupByValue = datatable.rows.rows.groupBy(dataRow => dataRow.get(0).get)
    columnGroupByValue.map{
      case(columnData, rows) => {
          val groupByClassifierValue = rows.groupBy(dataRow => dataRow.get(1))
          println(columnData)
          val entropyOfColumn = groupByClassifierValue.map{
            case(classifier, rowsForClassifier) => {
                  val p = rowsForClassifier.length.toDouble / rows.length
                  (-1) * p * log2(p)
                }
              }.foldRight(0.0)(_ + _)
          entropyOfColumn * rows.length / datatable.rows.length
      }
    }.foldRight(0.0)(_ + _)
  }
}
