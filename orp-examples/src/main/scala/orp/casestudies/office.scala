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
package orp.casestudies

import orp.api.{plays, role, relationship}

/**
 * Office Example.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
@relationship
object PlaceInventory {

  @role
  trait Place

  @role
  trait Inventory

}

@relationship
object EmployerEmployee {

  @role
  trait Employer

  @role
  trait Employee

}

@relationship
object OccupantOccupied {

  @role
  trait Occupant

  @role
  trait Occupied

}

@plays(role[PlaceInventory.Place], role[EmployerEmployee.Employer])
//@playsFor(role[PlaceInventory.Place], classOf[Desk])
//@playsFor(role[EmployerEmployee.Employer], classOf[Clerk])
class Office

@plays(role[PlaceInventory.Inventory], role[OccupantOccupied.Occupied])
//@playsFor(role[PlaceInventory.Inventory], classOf[Office])
//@playsFor(role[OccupantOccupied.Occupied], classOf[Clerk])
class Desk(var hasPhone: Boolean)

@plays(role[EmployerEmployee.Employee], role[OccupantOccupied.Occupant])
//@playsFor(role[EmployerEmployee.Employee], classOf[Office])
//@playsFor(role[OccupantOccupied.Occupant], classOf[Desk])
class Clerk

object OfficeMain extends App {
  val deskWithPhoneAndOccupied = new Desk(true)
  deskWithPhoneAndOccupied.addOccupant(new Clerk())
  val deskWithPhone = new Desk(true)
  val desk = new Desk(false)

  val desks = List(deskWithPhoneAndOccupied, deskWithPhone, desk)

  println("Found: " + findDesksWithPhonesAndUnoccupied.size)

  def findDesksWithPhonesAndUnoccupied = {

    desks filter {
      d =>
        d.hasPhone && d.getOccupants.isEmpty
    }

    //    Geht nicht, weil Zugriff über Supertyp
    //    office.getInventorys filter {
    //      d =>
    //        d.hasPhone && d.getOccupants.isEmpty
    //    }
  }
}
