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
 * Multiple plays annotations on a class.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
@relationship
object multrel {

  @role
  trait MultRole1 {}

  @role
  trait MultRole2 {}

}

@plays(role[multrel.MultRole1])
@plays(role[multrel.MultRole2])
class MultPlayer
