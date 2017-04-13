resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"
resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += "repo.jenkins-ci.org" at "https://repo.jenkins-ci.org/public" // why??

addSbtPlugin("ohnosequences" % "nice-sbt-settings" % "0.8.0-RC5")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.0.2")

// addCompilerPlugin("com.softwaremill.clippy" %% "plugin" % "0.5.2" classifier "bundle")
// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
// addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.0")
