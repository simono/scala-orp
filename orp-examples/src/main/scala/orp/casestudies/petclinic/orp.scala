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

import _root_.orp.api._
import java.util.Date

/**
 * ORP Implementation of the PetClinic.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object orp {

  @relationship
  object Ownable {

    @role(One) trait Owner

    @role trait Ownership {
      def changeOwner(newOwner: OwnerRole) {
        addOwner(newOwner)
      }
    }

  }

  @relationship
  object Visitable {

    @role(One) trait Receiver

    @role trait Visit {
      val date: Date
      val description: String
    }

  }

  @playsFor(role[Ownable.Owner], classOf[Pet])
  class Owner(var firstName: String, var lastName: String) {
    def getPet(name: String): Option[Pet] = getPets find {
      _.name.equalsIgnoreCase(name)
    }
  }

  @playsFor(role[Ownable.Ownership], classOf[Owner])
  @plays(role[Visitable.Receiver])
  class Pet(val birthDate: Date, var name: String)

  @plays(role[Visitable.Visit])
  class DoctorsVisit() {
    val date = new Date()
    val description = "Doctor's Visit on " + date
  }

  @plays(role[Visitable.Visit])
  class OwnersVisit(val date: Date, val description: String) {
    def getOwner = getReceivers.collectFirst {
      case p: Pet => p.getOwners.head
    }
  }

}
