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
package orp.plugin.components

import scala.tools.nsc.Global
import orp.api.plays

/**
 * An {@link OrpComponent} for {@link plays} transformations.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
class OrpComponentPlays(val global: Global) extends OrpComponent {

  import global._

  override val runsRightAfter = Some("orptransformrelationship")
  override val phaseName = "orptransformplays"

  override protected def newTransformer(unit: CompilationUnit) = new OrpTransformerPlays

  /**
   * A {@link plays} {@link OrpTransformer}.
   */
  class OrpTransformerPlays extends OrpTransformer {

    override val transformTree: PartialFunction[Tree, Tree] = {

      case classDef@ClassDef(mods, _, _, template@Template(parents, _, _))
        if check.annotation(mods, classOf[plays]) =>
        val plays = extract.annotationValuesFlat(mods, classOf[plays])
        log(s"${classDef.name} plays ${plays.mkString(", ")}")
        copy.ClassDef(classDef)(impl = copy.Template(template)(parents = parents ::: plays))
    }
  }

}
