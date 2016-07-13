resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"
// resolvers += "Era7 maven snapshots" at "https://s3-eu-west-1.amazonaws.com/snapshots.era7.com"
resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("ohnosequences" % "nice-sbt-settings" % "0.7.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.0")
