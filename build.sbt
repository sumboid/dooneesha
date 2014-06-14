name := "looneesha"

version := "0.0.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")
