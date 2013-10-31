import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.suguruhamazaki",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8")
  )
}

object Dependencies {
  val scalaLogging = "com.typesafe"        %% "scalalogging-slf4j" % "1.0.1"
  val logback = "ch.qos.logback" % "logback-classic" % "1.0.13"
  val scalatest = "org.scalatest"       %% "scalatest"          % "2.0.M8" % "test"
  val pegdown = "org.pegdown" % "pegdown" % "1.0.1" % "test"
  val mockito = "org.mockito"         % "mockito-core"        % "1.9.5" % "test"

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
  import org.scalastyle.sbt.ScalastylePlugin.{Settings => ScalastyleSettings}
  import ScctPlugin.instrumentSettings

  lazy val root = Project("fp-in-scala",
			  file("."),
                          settings = buildSettings ++ instrumentSettings ++ ScalastyleSettings) settings (
                    libraryDependencies ++= commonDeps,
                    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report", "-o"))
}
