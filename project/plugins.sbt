resolvers ++= Seq(
  "mmz-repository" at "http://maven.admin.srf.ch"
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.18")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.3.0")

//wartremover breaks this project! TODO why?
addSbtPlugin("ch.srf" % "srf-sbt-plugin" % "0.39")
