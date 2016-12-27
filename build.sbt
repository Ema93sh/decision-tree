name := """minimal-scala"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.github.martincooper" %% "scala-datatable" % "0.7.0"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

fork in run := true
