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
package orp.plugin

import components._
import tools.nsc.Global
import tools.nsc.plugins.Plugin

/**
 * A Scala Compiler {@link Plugin} for ORP.
 *
 * @author Simon Olofsson
 */
class OrpPlugin(val global: Global) extends Plugin {

  override val name = "orp"
  override val description = "A plugin for Object-Relational Programming (ORP)."
  override val components = List(
    new OrpComponentRelationship(global),
    new OrpComponentPlays(global),
    new OrpComponentPlaysFor(global)
  )
}
