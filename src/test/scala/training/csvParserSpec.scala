package com.decisiontree.specs.training

import scala.io.Source

import org.scalatest._
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
        val datatable = CSVParser.generateDataTable("Table", csvFile, columnTypes)

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
    }

    ignore("should return a datatable") {
      // val CsvParser.parse("testFile.csv")
    }
  }
}
