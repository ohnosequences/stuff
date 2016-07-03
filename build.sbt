Nice.scalaProject

name          := "stuff"
organization  := "ohnosequences"
description   := "stuff project"

bucketSuffix  := "era7.com"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"functors.scala",
  baseDirectory.value/"src"/"main"/"scala"/"categories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monads.scala",
  baseDirectory.value/"src"/"main"/"scala"/"naturalTransformations.scala"
)
