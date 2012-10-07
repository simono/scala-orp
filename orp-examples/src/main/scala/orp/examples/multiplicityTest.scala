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

import multiplicity._

/**
 * Some simple tests.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object multiplicityTest {

  def run() {

    val reseller = new Reseller()
    val customerOne = new Customer()
    reseller.addManySecond(customerOne)
    assert(reseller.getManySeconds == List(customerOne))
    assert(customerOne.getManyFirsts == List(reseller))

    val customerTwo = new Customer()
    customerTwo.addManyFirst(reseller)
    assert(reseller.getManySeconds == List(customerOne, customerTwo))
    assert(customerOne.getManyFirsts == List(reseller))
    assert(customerTwo.getManyFirsts == List(reseller))

    val contractOne = new Contract()
    customerOne.addMany(contractOne)
    assert(customerOne.getManys == List(contractOne))
    assert(contractOne.getOnes == List(customerOne))

    val contractTwo = new Contract()
    contractTwo.addOne(customerOne)
    assert(customerOne.getManys == List(contractOne, contractTwo))
    assert(contractOne.getOnes == List(customerOne))
    assert(contractTwo.getOnes == List(customerOne))

    contractTwo.addOne(customerTwo)
    assert(customerOne.getManys == List(contractOne))
    assert(contractOne.getOnes == List(customerOne))
    assert(customerTwo.getManys == List(contractTwo))
    assert(contractTwo.getOnes == List(customerTwo))

    val domainOne = new Domain()
    contractOne.addOneSecond(domainOne)
    assert(contractOne.getOneSeconds == List(domainOne))
    assert(domainOne.getOneFirsts == List(contractOne))

    val domainTwo = new Domain()
    domainTwo.addOneFirst(contractOne)
    assert(contractOne.getOneSeconds == List(domainTwo))
    assert(domainOne.getOneFirsts.isEmpty)
    assert(domainTwo.getOneFirsts == List(contractOne))

    println(this.getClass.getSimpleName + ": Everything went fine :-)")
  }
}
