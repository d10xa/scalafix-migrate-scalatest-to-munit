lazy val V = _root_.scalafix.sbt.BuildInfo

lazy val rulesCrossVersions = Seq(V.scala213)
lazy val scala3Version = "3.2.0"
lazy val testFrameworkDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.10",
  "org.scalameta" %% "munit" % "0.7.29"
)

inThisBuild(
  List(
    organization := "ru.d10xa",
    homepage := Some(url("https://github.com/d10xa/scalafix-migrate-scalatest-to-munit")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "d10xa",
        "Andrey Stolyarov",
        "d10xa@mail.ru",
        url("https://github.com/d10xa")
      )
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val `scalafix-migrate-scalatest-to-munit` = (project in file("."))
  .aggregate(
    rules.projectRefs ++
      input.projectRefs ++
      output.projectRefs ++
      tests.projectRefs: _*
  )
  .settings(
    publish / skip := true
  )

lazy val rules = projectMatrix
  .settings(
    moduleName := "scalafix",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
  )
  .defaultAxes(VirtualAxis.jvm)
  .jvmPlatform(rulesCrossVersions)

lazy val input = projectMatrix
  .settings(
    publish / skip := true,
    libraryDependencies ++= testFrameworkDependencies
  )
  .defaultAxes(VirtualAxis.jvm)
  .jvmPlatform(scalaVersions = rulesCrossVersions :+ scala3Version)

lazy val output = projectMatrix
  .settings(
    publish / skip := true,
    libraryDependencies ++= testFrameworkDependencies
  )
  .defaultAxes(VirtualAxis.jvm)
  .jvmPlatform(scalaVersions = rulesCrossVersions :+ scala3Version)

lazy val testsAggregate = Project("tests", file("target/testsAggregate"))
  .aggregate(tests.projectRefs: _*)

lazy val tests = projectMatrix
  .settings(
    publish / skip := true,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    scalafixTestkitOutputSourceDirectories :=
      TargetAxis
        .resolve(output, Compile / unmanagedSourceDirectories)
        .value,
    scalafixTestkitInputSourceDirectories :=
      TargetAxis
        .resolve(input, Compile / unmanagedSourceDirectories)
        .value,
    scalafixTestkitInputClasspath :=
      TargetAxis.resolve(input, Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions :=
      TargetAxis.resolve(input, Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion :=
      TargetAxis.resolve(input, Compile / scalaVersion).value
  )
  .defaultAxes(
    rulesCrossVersions.map(VirtualAxis.scalaABIVersion) :+ VirtualAxis.jvm: _*
  )
//  .customRow(
//    scalaVersions = Seq(V.scala212),
//    axisValues = Seq(TargetAxis(scala3Version), VirtualAxis.jvm),
//    settings = Seq()
//  )
  .customRow(
    scalaVersions = Seq(V.scala213),
    axisValues = Seq(TargetAxis(V.scala213), VirtualAxis.jvm),
    settings = Seq()
  )
//  .customRow(
//    scalaVersions = Seq(V.scala212),
//    axisValues = Seq(TargetAxis(V.scala212), VirtualAxis.jvm),
//    settings = Seq()
//  )
//  .customRow(
//    scalaVersions = Seq(V.scala211),
//    axisValues = Seq(TargetAxis(V.scala211), VirtualAxis.jvm),
//    settings = Seq()
//  )
  .dependsOn(rules)
  .enablePlugins(ScalafixTestkitPlugin)
