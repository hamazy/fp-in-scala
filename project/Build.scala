import sbt._
import Keys._

object FpInScalaBuild extends Build {

  override lazy val settings =
    super.settings ++ Seq(
      scalaVersion := "2.10.0",
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
      libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-actors" % "2.10.0",
	"com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",
        "org.mockito" % "mockito-core" % "1.9.5" % "test")
    )

  lazy val root = Project(id = "fp-in-scala",
			  base = file("."))
}
