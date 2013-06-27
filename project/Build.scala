//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization        := "com.github.aloiscochard.shona",
    version             := "0.1-SNAPSHOT",
    scalaVersion        := "2.10.2",
    resolvers           ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    scalacOptions := Seq(
      "-feature",
      "-language:existentials",
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-deprecation",
      "-unchecked"
    ),
    libraryDependencies <+= scalaVersion(
      "org.scala-lang" % "scala-compiler" % _
    )
  )
}

object Dependencies {
  val testDependencies = Seq(
    libraryDependencies += "org.specs2" %% "specs2" % "1.13.1-SNAPSHOT" % "test"
  )
}

object ShonaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val shona = Project (
    "shona",
    file ("."),
    settings = buildSettings ++ Seq(publishArtifact := false)
  ) aggregate (core, demo)

  lazy val core = Project(
    "shona-core",
    file("shona-core"),
    settings = buildSettings ++ testDependencies ++ Seq(
      //libraryDependencies <++= (scalaVersion)(v => Seq("org.scala-lang" % "scala-reflect" % v, "org.scala-lang" % "scala-compiler" % v)),
      libraryDependencies ++= Seq(
        "com.chuusai" %% "shapeless" % "1.2.4"
      )
    )
  )

  lazy val demo = Project(
    "shona-demo",
    file("shona-demo"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core)
}
