[![Build Status](https://secure.travis-ci.org/simono/scala-orp.png?branch=master)](http://travis-ci.org/simono/scala-orp)

# Setup instructions for Scala ORP

## What you need

* Java 7 or 8
* [sbt](http://www.scala-sbt.org) as build tool

## IDE setup

* Eclipse:
  * install the [Scala IDE](http://scala-ide.org)
  * run `sbt eclipse`
  * import the Project
* IntelliJ IDEA:
  * install the [Scala
    plugin](http://confluence.jetbrains.net/display/SCA/Scala+Plugin+for+IntelliJ+IDEA)
  * open the Project

## Running the plugin

To run the plugin just execute: `bin/run-plugin.sh`

## Directory layout

The main ORP project is aggregating three modules (_orp-framework_, _orp-plugin_
and _orp-examples_). The build settings, dependencies and build instructions are
described in _project/Build.scala_. _project/plugins.sbt_ and
_project/assembly.sbt_ describe the required plugins.

_bin_: contains executables and scripts

_orp-framework_: project for the ORP Framework, contains definitions that are
needed in client code and in the compiler plugin

_orp-plugin_: a plugin for the Scala compiler (_scalac_) that generates the
required code

_orp-examples_: examples that use the _orp-framework_ and the _orp-plugin_

_target_ folders: these folders are excluded from version control and contain
class-files and jars that sbt creates

# License

This software is licensed under the Apache 2 license, quoted below.

```
Copyright 2012 Simon Olofsson

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
