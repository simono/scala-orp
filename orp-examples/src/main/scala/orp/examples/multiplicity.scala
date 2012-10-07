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
package orp.examples

import orp.api._

/**
 * Multiplicity.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object multiplicity {

  @relationship
  object ManyToMany {

    @role trait ManyFirst

    @role trait ManySecond

  }

  @relationship
  object OneToMany {

    @role(One) trait One

    @role trait Many

  }

  @relationship
  object OneToOne {

    @role(One) trait OneFirst

    @role(One) trait OneSecond

  }

  @plays(role[ManyToMany.ManyFirst]) class Reseller

  @plays(role[ManyToMany.ManySecond], role[OneToMany.One]) class Customer

  @plays(role[OneToMany.Many], role[OneToOne.OneFirst]) class Contract

  @plays(role[OneToOne.OneSecond]) class Domain

}
