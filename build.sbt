name := """twc-assignments"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Dependencies.dependencies

fork in run := true

testOptions in Test +=
  Tests.Argument(
    TestFrameworks.ScalaCheck,
    "maxDiscardRatio",
    "5",
    "minSuccessfulTests",
    "1000"
  )