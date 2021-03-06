name := "stuff"
organization := "ohnosequences"
description := "some stuff"
bucketSuffix := "era7.com"
scalaVersion := "2.12.4"

addCompilerPlugin("ohnosequences" %% "contexts" % "0.5.0")

resolvers += Resolver.bintrayRepo("scalameta", "maven")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

dependencyOverrides += "org.scala-lang" % "scala-library" % "2.12.4"

scalacOptions ++= Seq(
  "-Xsource:2.13",
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
scalafmtVersion := "1.4.0-25-15b52145"
scalafmtOnCompile := true

wartremoverErrors in (Compile, compile) := Seq()

wartremoverWarnings in (Compile, compile) := Warts.allBut(
  Wart.Equals,
  Wart.FinalVal,
  Wart.ImplicitConversion,
  Wart.Nothing // needed because of the contexts compiler plugin
)

wartremoverExcluded ++= Seq(
  baseDirectory.value / "src" / "test" / "scala" / "tuples" / "stdComparison.scala",
  baseDirectory.value / "src" / "main" / "scala" / "distributiveCategories.scala"
)

// shows time for each test
testOptions in Test += Tests.Argument("-oD")
// disables parallel test execution
parallelExecution in Test := false
