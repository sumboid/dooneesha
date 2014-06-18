name := "looneesha"

version := "0.0.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")
