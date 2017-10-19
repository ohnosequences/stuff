name := "stuff"
organization := "ohnosequences"
description := "some stuff"
bucketSuffix := "era7.com"
scalaVersion := "2.12.3"

addCompilerPlugin("ohnosequences" %% "contexts" % "0.5.0")

scalacOptions ++= Seq(
  "-Xlint",
  "-Xfatal-warnings",
  "-Xlog-reflective-calls",
  "-Ywarn-unused",
  "-Ywarn-adapted-args",
  "-opt-warnings:_",
  "-unchecked",
  "-Xstrict-inference",
  "-Yno-predef",
  "-Yno-imports",
  "-Ywarn-unused-import",
  "-Yno-adapted-args",
  "-Ydelambdafy:method",
  "-opt:l:inline",
  "-opt-inline-from:<sources>",
  "-opt:l:method"
  // "-Xfuture",
  // "-Xlog-free-types",
  // "-Xlog-free-terms",
  // "-Ydebug",
  // "-explaintypes",
  // "-uniqid",
  // "-Yopt-log-inline", "_", // noisy
)

// scaladoc
scalacOptions in (Compile, doc) ++= Seq("-groups")
autoAPIMappings := true

// scalafmt
scalafmtVersion := "1.3.0"
scalafmtOnCompile := true

// all these exceptions come from not being able to only exclude `asInstanceOf`
wartremoverExcluded ++= Seq(
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "naturalTransformations.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "categories.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "monads.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "monoidalCategories.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "functors.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "sums.scala",
  baseDirectory.value / "src" / "main" / "scala" / "stuff" / "tailrec.scala"
)

wartremoverErrors in (Test, compile) := Seq()

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
