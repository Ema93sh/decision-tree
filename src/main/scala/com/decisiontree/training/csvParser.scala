package com.decisiontree.training

import java.io._
import scala.util.Try
import scala.io.Source

import com.github.tototoshi.csv._
import com.github.martincooper.datatable._

object CSVParser {
  type ColumnType = Class[ _ >: String with Double with Float with Int]
  val ClassOfInt = classOf[Int]
  val ClassOfString = classOf[String]
  val ClassOfFloat = classOf[Float]
  val ClassOfDouble = classOf[Double]


  def generateDataTable(tableName: String, csvFile: Source, columnTypes: Map[String, ColumnType]): Try[DataTable] = {
     val reader = CSVReader.open(csvFile)
     val (headers, dataWithHeaders) = reader.allWithOrderedHeaders()
     reader.close()
     val columnMap = headers map { columnName => columnName -> dataWithHeaders.map(row => row(columnName)) }
     val dataColumns = columnMap map {
       case (columnName, colData) =>
          columnTypes(columnName) match {
            case ClassOfString => new DataColumn[String](columnName, colData)
            case ClassOfFloat => new DataColumn[Float](columnName, colData map (x => x.toFloat))
            case ClassOfDouble => new DataColumn[Double](columnName, colData map (x => x.toDouble))
            case ClassOfInt => new DataColumn[Int](columnName, colData map (_.toInt))
          }
     }

     DataTable(tableName, dataColumns)
  }


}
