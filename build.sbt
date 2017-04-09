
name          := "stuff"
organization  := "ohnosequences"
description   := "some stuff"

bucketSuffix  := "era7.com"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test

incOptions := incOptions.value.withNameHashing(false)

scalacOptions ++= Seq(
  // "-Xfatal-warnings",
  // "-Ydebug", "-uniqid",
  "-unchecked",
  "-Xlint",
  "-Xstrict-inference",
  "-Ywarn-unused-import",
  "-Xfuture",
  "-Ywarn-unused-import",
  "-Yno-predef",
  "-Yno-imports",
  "-opt:inline-project",
  "-opt-warnings:_",
  "-opt:l:project",
  "-opt:l:method"
)

wartremoverExcluded ++= Seq(
  baseDirectory.value/"src"/"main"/"scala"/"stuff"/"categories.scala",
  baseDirectory.value/"src"/"main"/"scala"/"stuff"/"functors.scala",
  baseDirectory.value/"src"/"main"/"scala"/"stuff"/"sums.scala", // isInstanceOf
  baseDirectory.value/"src"/"main"/"scala"/"stuff"/"tailrec.scala"
)


// removed will be
excludeFilter in unmanagedSources :=
  (excludeFilter in unmanagedSources).value ||
  new SimpleFileFilter(_.getCanonicalPath startsWith s"${baseDirectory.value}/src/main/scala/old")

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
