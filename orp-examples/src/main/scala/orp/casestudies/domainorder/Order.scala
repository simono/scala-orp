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
package orp.casestudies.domainorder

import orp._

/**
 * Example Queries for the DomainOrder.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object Order extends App {

  val domain = new Domain("example.com")
  val contract = new Contract(domain)

  assert(contract eq contract.getDomains.head.getContracts.head)

  contract.getDomains.foreach(d => println(d.name))
}
