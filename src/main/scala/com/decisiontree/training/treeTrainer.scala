package com.decisiontree.training

import com.github.martincooper.datatable._

import com.decisiontree.models._


case class TreeTrainer(trainingData: DataTable, classifierColumn: GenericColumn) {


  def generateDecisionTree = {
    def _generateDecisionTree(trainingSubSet: DataTable): Node = {
      if (trainingSubSet.columns.length == 1)
      {
          val classifiers = trainingSubSet.columns(0).data.distinct
          if(classifiers.length != 1) {
            throw new IllegalArgumentException("This shouldnt happen...")
          }
          return ClassifierNode(classifiers(0).toString) // TODO: Support non string values in classifier Node
      }
      val classifiers = trainingSubSet.columns(classifierColumn.name).data.distinct
      if (classifiers.length == 1) {
        return ClassifierNode(classifiers(0).toString)
      }
      val category = TreeTrainer.attributeWithMaximumInformationGain(trainingSubSet, trainingSubSet.columns(classifierColumn.name))
      val values = trainingSubSet.columns(category).data.distinct
      val decisions = values map (value => Decision(category, value.toString)) // TODO: Support non string values in Decision
      val links = decisions map (decision =>
        {
          val filteredDataTable = TreeTrainer.filterDataTable(trainingSubSet, category, decision.value)
          val dataTableWithoutCategory = filteredDataTable.columns.remove(category).get
          Link(decision, _generateDecisionTree(dataTableWithoutCategory))
        })
      DecisionNode(category, links.toList)

    }

    val rootNode = _generateDecisionTree(trainingData)
    DecisionTree(rootNode.asInstanceOf[DecisionNode])
  }

}

object  TreeTrainer {
  val lnOf2 = scala.math.log(2)
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

  def filterDataTable(dataTable: DataTable, columnName: String, value: Any): DataTable = {
    val dataView =  dataTable.filter( row =>
      row(columnName) == value
    )
    DataView.toDataTable(dataView).get
  }

  def attributeWithMaximumInformationGain(datatable: DataTable, classifierColumn: GenericColumn) = {
    if(datatable.columns.length == 2) {
        datatable.columns.filter(_.name != classifierColumn.name)(0).name
    }
    val informationGains = datatable.columns.filter(column => column.name != classifierColumn.name).map( column =>
       column.name ->  informationGain(DataTable("InformationGain", List(column, classifierColumn)).get)
    )
    informationGains.maxBy(_._2)._1
  }

  def entropy(classifierColumn: GenericColumn) =
    classifierColumn.data.groupBy(value => value).map
              {
                case (columnValue, rows) =>
                  val p = rows.size.toDouble/classifierColumn.data.size
                  (-1) * p * log2(p)
              }.foldRight(0.0)(_ + _)

  def informationGain(datatable: DataTable) = {
    val columnGroupByValue = datatable.rows.rows.groupBy(dataRow => dataRow.get(0).get)
    val columnEntropy = columnGroupByValue.map{
      case (columnValue, rows) => {
        val classifierColumn = datatable.columns(1).buildFromRows(rows.map(_.rowIndex)).get
        entropy(classifierColumn) * classifierColumn.data.length / datatable.rows.length
      }
    }.foldRight(0.0)(_ + _)

    entropy(datatable.columns(1)) - columnEntropy
  }
}
