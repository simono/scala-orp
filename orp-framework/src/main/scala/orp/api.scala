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
package orp

import annotation.Annotation

/**
 * Annotations for the ORP framework.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object api extends Enumeration {

  type multiplicity = Value
  val One, Many = Value

  class relationship extends Annotation

  class role(multiplicity: multiplicity = Many) extends Annotation

  class plays(roles: ClassManifest[_ <: RoleTrait]*) extends Annotation

  class playsFor(role: ClassManifest[_ <: RoleTrait], forClass: Class[_]) extends Annotation

  trait RoleTrait

  def role[T <: RoleTrait : ClassManifest] = classManifest[T]

}
