name := "leetcode"

version := "0.1"

scalaVersion := "2.13.4"
val scalatestVersion = "3.2.9"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % scalatestVersion,
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)
