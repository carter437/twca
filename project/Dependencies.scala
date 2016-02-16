import sbt._

object Version{
  val scalaTest = "2.2.4"
  val scalaCheck = "1.12.5"
}

object Library{
  val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % "test"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % Version.scalaCheck % "test"
}

object Dependencies{
  import Library._
  val dependencies = Seq(scalaTest,scalaCheck)
}