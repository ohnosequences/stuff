Nice.scalaProject

name          := "stuff"
organization  := "ohnosequences"
description   := "stuff project"

bucketSuffix  := "era7.com"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test

addCompilerPlugin("com.github.mpilquist" %% "local-implicits" % "0.3.0")

incOptions := incOptions.value.withNameHashing(false)

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"functors.scala",
  baseDirectory.value/"src"/"main"/"scala"/"categories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monoidalCategories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monads.scala",
  baseDirectory.value/"src"/"main"/"scala"/"kleisli.scala",
  baseDirectory.value/"src"/"main"/"scala"/"naturalTransformations.scala"
)
