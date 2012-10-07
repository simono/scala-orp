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
 * Examples taken from the ORP paper.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
@relationship
object aggregation {

  @role
  trait Whole {
    def contains(part: Part): Boolean = {
      for (p <- getParts) {
        if (p == part) return true
        if (p.isInstanceOf[Whole]) {
          if (p.asInstanceOf[Whole] contains part)
            return true
        }
      }
      return false
    }
  }

  @role
  trait Part

  def contains(whole: Whole, part: Part): Boolean = whole contains part
}

@plays(role[aggregation.Whole], role[aggregation.Part])
class Assembly

@plays(role[aggregation.Part])
class Piece

@playsFor(role[aggregation.Whole], classOf[File])
class Directory

@playsFor(role[aggregation.Part], classOf[Directory])
class File
