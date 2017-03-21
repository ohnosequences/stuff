
name          := "stuff"
organization  := "ohnosequences"
description   := "some stuff"

bucketSuffix  := "era7.com"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"scala"/"products.scala",
  baseDirectory.value/"src"/"main"/"scala"/"functors.scala",
  baseDirectory.value/"src"/"main"/"scala"/"categories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monoidalCategories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"monads.scala",
  baseDirectory.value/"src"/"main"/"scala"/"kleisli.scala",
  baseDirectory.value/"src"/"main"/"scala"/"naturalTransformations.scala"
)

incOptions := incOptions.value.withNameHashing(false)

scalacOptions ++= Seq(
  "-unchecked",
  // "-Xfatal-warnings",
  "-Xlint",
  "-Xstrict-inference",
  "-Ywarn-unused-import",
  "-Xfuture",
  // "-Ydebug", "-uniqid",
  "-Ywarn-unused-import",
  "-Yno-predef",
  "-Yno-imports",
  // "-Ylog:inliner",
  "-opt:inline-project",
  "-opt-warnings:_",
  "-opt:l:project",
  "-opt:l:method"
)
