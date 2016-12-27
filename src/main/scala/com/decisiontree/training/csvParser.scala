package com.decisiontree.training

import java.io._
import scala.io.Source

import com.github.tototoshi.csv._
import com.github.martincooper.datatable._
import TypedDataValueImplicits._

object CSVParser {
  type ColumnType = Class[ _ >: String with Float with Double with Int]

  def createDataColumnWith(columnName: String, columnType: ColumnType, colData: List[String]) = {
    val ClassOfInt = classOf[Int]
    val ClassOfString = classOf[String]
    val ClassOfFloat = classOf[Float]
    val ClassOfDouble = classOf[Double]

    columnType match {
      case ClassOfString => new DataColumn[String](columnName, colData)
      case ClassOfInt => new DataColumn[Int](columnName, Seq()) //TODO
    }
  }

  def convertToColSet(rowSet: List[List[String]]) = {
    var colSet = List[List[String]]()
    val colSize = rowSet(0).size
    val rowSize = rowSet.size
    for (col <- 0 to colSize) {
      var column = List[String]()
      for (row <- 0 to rowSize) {
        column = column :+ rowSet(row)(col)
      }
      colSet = colSet :+ column
    }
    colSet
  }

  def generateDataTable(tableName: String, csvFile: Source, columnTypes: Map[String, ColumnType]) = {
     val reader = CSVReader.open(csvFile)
     val listOfRows = reader.all()
     val listOfCols = convertToColSet(listOfRows)
     reader.close()
     println(listOfCols)
    //  val dataColumns = colset.zip(columnTypes).map{case (colData, (name, t)) => createDataColumnWith(name, t, colData)
    //  }
     //
     DataTable(tableName, Seq())
  }

  
}
