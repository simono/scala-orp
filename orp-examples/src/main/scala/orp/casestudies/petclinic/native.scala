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

import collection.mutable.Set
import java.util.Date

/**
 * Native Implementation of the PetClinic.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object native {

  class Owner(var firstName: String, var lastName: String) {

    private val pets: Set[Pet] = Set()

    def addPet(pet: Pet) {
      pets += pet
      pet.owner = Some(this)
    }

    def removePet(pet: Pet) {
      pets -= pet
      pet.owner = None
    }

    def getPet(name: String): Option[Pet] = pets find {
      _.name.equalsIgnoreCase(name)
    }
  }

  class Pet(val birthDate: Date, var name: String) {

    private[native] var owner: Option[Owner] = None
    private val visits: Set[Visit] = Set()

    def addVisit(visit: Visit) {
      visits += visit
      visit.pet = Some(this)
    }

    def changeOwner(newOwner: Owner) {
      owner.foreach(_.removePet(this))
      newOwner.addPet(this)
    }
  }

  abstract class Visit() {
    val date: Date
    val description: String
    private[native] var pet: Option[Pet] = None
  }

  class DoctorsVisit() extends Visit {
    val date = new Date()
    val description = s"Doctor's Visit on $date"
  }

  class OwnersVisit(val date: Date, val description: String) extends Visit {
    def getOwner = pet.flatMap(_.owner)
  }

}
