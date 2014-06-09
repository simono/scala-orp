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

import java.lang.reflect.Modifier

/**
 * Some simple tests.
 *
 * @author Simon Olofsson
 */
object containerTest {

  // TODO so: Make this tests generally available

  import container._

  def testRel1 {
    assert(Modifier.isAbstract(classOf[ACrel1].getModifiers))
    assert(rel1.isInstanceOf[ACrel1])
  }

  def testRoles {
    for (c <- List((classOf[rel1.Role1], "Role2"), (classOf[rel1.Role2], "Role1"))) {
      assert(c._1.getMethods.exists(_.getName == s"add${c._2}"))
      assert(c._1.getMethods.exists(_.getName == s"remove${c._2}"))
      assert(c._1.getMethods.exists(_.getName == s"get${c._2}s"))
    }
  }

  def testPlays {
    new player().getRole2s
  }

  def testPlaysFor {
    new playsFor1().addPlaysFor2(new playsFor2())
  }

  def run() {

    testRel1
    testRoles
    testPlays
    testPlaysFor

    println(s"${this.getClass.getSimpleName}: Everything went fine :-)")
  }
}
