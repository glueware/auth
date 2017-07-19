name := "srf-auth"

version := "1.0"

scalaVersion in ThisBuild := "2.12.2"

lazy val root = project.in(file("."))
  .enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
  .settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++= Seq(
      "org.springframework.security.kerberos" % "spring-security-kerberos-core" % "1.0.1.RELEASE",
      "org.springframework.ldap" % "spring-ldap-core" % "2.0.4.RELEASE",
      SrfPlugin.Deps.Compile.scalazCore
    )
  )