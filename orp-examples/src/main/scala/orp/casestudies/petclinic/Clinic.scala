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
package orp.casestudies.petclinic

import orp._

//import native._

import java.text.SimpleDateFormat

/**
 * Example Queries for the PetClinic.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object Clinic extends App {

  implicit def String2Date(s: String) = new SimpleDateFormat("yyyy-MM-dd").parse(s)

  val jeff = new Owner("Jeff", "Miller")
  val lassie = new Pet("1940-06-04", "Lassie")
  jeff.addPet(lassie)
  assert(jeff.getPet("lassie") == Some(lassie))

  val ownersVisit = new OwnersVisit("1943-10-07", "First Visit")
  lassie.addVisit(ownersVisit)
  assert(ownersVisit.getOwner == Some(jeff))

  val doctorsVisit = new DoctorsVisit()
  lassie.addVisit(doctorsVisit)

  val timmy = new Owner("Timmy", "Martin")
  lassie.changeOwner(timmy)
  assert(jeff.getPet("lassie").isEmpty)
  assert(timmy.getPet("lassie") == Some(lassie))
}
