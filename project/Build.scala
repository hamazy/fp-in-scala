import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.suguruhamazaki",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint")
  )
}

object Dependencies {
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.2"
  val scalatest = "org.scalatest" %% "scalatest" % "2.1.4" % "test"
  val pegdown = "org.pegdown" % "pegdown" % "1.0.1" % "test"
  val mockito = "org.mockito" % "mockito-core" % "1.9.5" % "test"

  val commonDeps = Seq(
    scalaLogging,
    logback,
    scalatest,
    pegdown,
    mockito
  )
}

object FpInScalaBuild extends Build {

  import BuildSettings.buildSettings
  import Dependencies.commonDeps
  import org.scalastyle.sbt.ScalastylePlugin.{projectSettings ⇒ ScalastyleSettings}
  import ScctPlugin.instrumentSettings

  lazy val root = Project("fp-in-scala",
			  file("."),
                          settings = buildSettings ++ instrumentSettings ++ ScalastyleSettings) settings (
                    libraryDependencies ++= commonDeps,
                    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report", "-o"))
}
