package com.decisiontree.specs.training

import scala.io.Source

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.FunSpec
import com.github.martincooper.datatable._

import com.decisiontree.training._

class CsvParerSpec extends FunSpec with Matchers {
  describe("CSV Parser") {
    describe("Datatable") {
      it("should generate datatable from csv file") {
        val columnTypes = Map( "Day" -> classOf[String],
                               "Outlook" -> classOf[String],
                               "Temperature" -> classOf[String],
                               "Humidity" -> classOf[String],
                               "Wind" -> classOf[String],
                               "PlayTennis" -> classOf[String]
                             )
        val csvFile = Source.fromURL(getClass.getResource("/willJohnPlayTennis.csv"))
        val datatable = CSVParser.generateDataTable("Table", csvFile, columnTypes).get

        val dayColumn = datatable.columns.get("Day").get
        val outlookColumn = datatable.columns.get("Outlook").get
        val tempratureColumn = datatable.columns.get("Temperature").get
        val windColumn = datatable.columns.get("Wind").get
        val humidityColumn = datatable.columns.get("Humidity").get
        val playTennisColumn = datatable.columns.get("PlayTennis").get

        dayColumn.data should have size 14
        outlookColumn.data should have size 14
        tempratureColumn.data should have size 14
        windColumn.data should have size 14
        humidityColumn.data should have size 14
        playTennisColumn.data should have size 14
      }

      it("should parse csv with double type") {
        val columnTypes = Map( "Sepal length" -> classOf[Double],
                               "Sepal width" -> classOf[Double],
                               "Petal length" -> classOf[Double],
                               "Petal width" -> classOf[Double],
                               "Class" -> classOf[String]
                             )
        val csvFile = Source.fromURL(getClass.getResource("/iris-data.csv"))
        val datatable = CSVParser.generateDataTable("Iris Data", csvFile, columnTypes).get

        val slength = datatable.columns.get("Sepal length").get
        val swidth = datatable.columns.get("Sepal width").get
        val plength = datatable.columns.get("Petal length").get
        val pwidth = datatable.columns.get("Petal width").get
        val irisclass = datatable.columns.get("Class").get

        slength.data should have size 150
        swidth.data should have size 150
        plength.data should have size 150
        pwidth.data should have size 150
        irisclass.data should have size 150

        val firstRow = datatable.rows(0)
        firstRow.getAs[Double]("Sepal length").get should equal (5.1)
        firstRow.getAs[Double]("Sepal width").get should equal (3.5)
        firstRow.getAs[Double]("Petal length").get should equal (1.4)
        firstRow.getAs[Double]("Petal width").get should equal (0.2)
        firstRow("Class") should equal ("Iris-setosa")
      }
    }
  }
}
