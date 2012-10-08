[![Build Status](https://secure.travis-ci.org/simono/scala-orp.png)](http://travis-ci.org/simono/scala-orp)

# Setup instructions for the Scala ORP Project

## What you need

* a recent version of Java
* [sbt](https://github.com/harrah/xsbt) as build tool
  * On OS X with Homebrew: `brew install sbt`
  * [On Debian or Ubuntu](http://typesafe.com/stack/download#deb)

## IDE setup

* IntelliJ IDEA:
  * install the
    [Scala](http://confluence.jetbrains.net/display/SCA/Scala+Plugin+for+IntelliJ+IDEA)
    and [SBT](https://github.com/orfjackal/idea-sbt-plugin/wiki) plugins and
    configure them
  * select `intellij/orp.ipr` in the "Open Project" dialog
* Eclipse:
  * install the [Scala](http://scala-ide.org) and
    [SBT](https://github.com/scalastuff/esbt) plugins

## Running the plugin

To run the plugin just execute: `bin/run-plugin.sh`

## Directory layout

The main ORP project is aggregating three modules (`orp-framework`,
`orp-plugin` and `orp-examples`). The build settings, dependencies and build
instructions are described in `project/Build.scala`. `project/plugins.sbt`
describes the required plugins.

`bin`: contains executables and scripts

`orp-framework`: project for the ORP framework, contains definitions that are
needed in client code and in the compiler plugin

`orp-plugin`: a plugin for the Scala compiler (`scalac`) that generates the
required code

`orp-examples`: examples that use the `orp-framework` and the `orp-plugin`

`target` folders: these folders are excluded from version control and contain
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
