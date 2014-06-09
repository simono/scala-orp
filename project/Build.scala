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
import sbtassembly.Plugin._

/**
 * SBT {@link Build} definition.
 *
 * @author Simon Olofsson
 */
object OrpBuild extends Build {

  val buildOrganization = "orp"
  val buildScalaMajorVersion = "2.10"
  val buildScalaVersion = s"$buildScalaMajorVersion.1"
  val buildVersion = "1.0"

  override lazy val settings = super.settings ++ Seq(
    organization := buildOrganization,
    scalaVersion := buildScalaVersion,
    version := buildVersion,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    shellPrompt := {
      Project.extract(_).currentProject.id + "> "
    }
  )

  val scalacompiler = "org.scala-lang" % "scala-compiler" % buildScalaVersion

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
    settings = buildSettings ++
			Seq(libraryDependencies := Seq(scalacompiler)) ++
			assemblySettings
		) dependsOn(framework)

  // The examples
  lazy val examples = Project(
    "orp-examples",
    file("orp-examples"),
    settings = buildSettings ++ Seq(
      autoCompilerPlugins := true,
      scalacOptions ++= Seq(
        "-Xpluginsdir",
        file("orp-plugin").getAbsolutePath + s"/target/scala-$buildScalaMajorVersion",
        "-Xplugin-require:orp",
        "-Ylog:orp"
      )
    )
  ) dependsOn(framework, plugin)
}
