
name          := "stuff"
organization  := "ohnosequences"
description   := "some stuff"

bucketSuffix  := "era7.com"

scalaVersion := "2.12.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"functors.scala",
  baseDirectory.value/"src"/"main"/"scala"/"categories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monoidalCategories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monads.scala",
  baseDirectory.value/"src"/"main"/"scala"/"kleisli.scala",
  baseDirectory.value/"src"/"main"/"scala"/"naturalTransformations.scala"
)
