/*
 * Copyright 2012 Simon Olofsson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import sbt._
import Keys._

/**
 * SBT BuildSettings.
 *
 * @author Simon Olofsson {@literal simon@olofsson.de}
 */
object BuildSettings {
  val buildOrganization = "orp"
  val buildScalaVersion = "2.9.2"
  val buildVersion = "1.0"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version := buildVersion,
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    shellPrompt := {
      Project.extract(_).currentProject.id + "> "
    }
  )

}

/**
 * SBT Dependencies.
 *
 * @author Simon Olofsson {@literal simon@olofsson.de}
 */
object Dependencies {

  import BuildSettings._

  val scalacompiler = "org.scala-lang" % "scala-compiler" % buildScalaVersion
}

/**
 * SBT {@link Build} definition.
 *
 * @author Simon Olofsson {@literal simon@olofsson.de}
 */
object OrpBuild extends Build {

  import BuildSettings._
  import Dependencies._

  // The main project
  lazy val orp = Project(
    "orp",
    file("."),
    settings = buildSettings
  ) aggregate(framework, plugin, examples)

  // The Framework
  lazy val framework = Project(
    "orp-framework",
    file("orp-framework"),
    settings = buildSettings
  )

  // The compiler plugin
  lazy val plugin = Project(
    "orp-plugin",
    file("orp-plugin"),
    settings = buildSettings ++ Seq(libraryDependencies := Seq(scalacompiler))
  ) dependsOn (framework) settings (sbtassembly.Plugin.assemblySettings: _*)

  // The examples
  lazy val examples = Project(
    "orp-examples",
    file("orp-examples"),
    settings = buildSettings ++ Seq(
      autoCompilerPlugins := true,
      scalacOptions ++= Seq(
        "-Xpluginsdir",
        file("orp-plugin").getAbsolutePath + "/target/",
        "-Xplugin-require:orp",
        "-Ylog:orp"
      )
    )
  ) dependsOn(framework, plugin)
}
