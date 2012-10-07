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
package orp.plugin.components

import scala.tools.nsc.Global
import orp.api.playsFor

/**
 * An {@link OrpComponent} for {@link playsFor} transformations.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
class OrpComponentPlaysFor(val global: Global) extends OrpComponent {

  import global._

  override val runsRightAfter = Some("orptransformplays")
  override val phaseName = "orptransformplaysfor"

  override protected def newTransformer(unit: CompilationUnit) = new OrpTransformerPlaysFor

  /**
   * A {@link playsFor} {@link OrpTransformerMulti}.
   */
  class OrpTransformerPlaysFor extends OrpTransformerMulti {

    override val checkTree: PartialFunction[Tree, Boolean] = {
      case ClassDef(mods, _, _, _) => check.annotation(mods, classOf[playsFor])
    }

    override def transform(trees: List[Tree]) = {

      require(checkTrees(trees))

      // Get all ClassDefs with a playsFor-Annotation
      val playsForClasses = extract.classDefsWithAnnotation(trees, classOf[playsFor])

      // Map matching PlaysForClauses in Pairs
      val playsForClauses = createPlaysForClauses(playsForClasses)

      // Create the ModuleDefs
      val playsForRelationshipModules = playsForClauses.map(createPlaysForRelationshipModule)

      // Transform the classes
      val playsForClassesTransformed = transformPlaysForClasses(playsForRelationshipModules, playsForClauses)

      // Put everything together except the original classes
      trees.filterNot(playsForClasses contains) ::: playsForRelationshipModules ::: playsForClassesTransformed
    }

    private case class PlaysForClause(clazz: ClassDef, role: Select, forClass: Name)

    private def createPlaysForClauses(classDefs: List[ClassDef]): List[(PlaysForClause, PlaysForClause)] = {
      mapPlaysForClauses {
        classDefs flatMap {
          clazz =>
            extract.selectsAndNamesFromAnnotationValues(classOf[playsFor], clazz.mods) map {
              san =>
                val (role, forClass) = san
                PlaysForClause(clazz, role, forClass)
            }
        }
      }
    }

    private def mapPlaysForClauses(playsForClauses: List[PlaysForClause]): List[(PlaysForClause, PlaysForClause)] = {

      require(playsForClauses.length % 2 == 0, "Found " + playsForClauses.length + ", that won't match!")

      if (playsForClauses.isEmpty) Nil
      else {

        val pfcOne = playsForClauses.head
        val pfcTwo = playsForClauses find {
          pfc =>
            pfcOne.forClass == pfc.clazz.name && pfcOne.clazz.name == pfc.forClass
        } getOrElse {
          abort("Class " + pfcOne.clazz.name + " playsFor " + pfcOne.forClass + ", but this class wasn't found!")
        }
        assert(pfcOne != pfcTwo, "Class " + pfcOne.clazz.name + " plays itself!")
        (pfcOne, pfcTwo) :: mapPlaysForClauses(playsForClauses.filterNot(pfc => pfc == pfcOne || pfc == pfcTwo))
      }
    }

    private def createPlaysForRelationshipModule(pfcs: (PlaysForClause, PlaysForClause)): ModuleDef = {

      require(pfcs._1.role.name != pfcs._2.role.name, pfcs._1.role.name + " == " + pfcs._2.role.name)

      log(pfcs._1.clazz.name + " plays " + pfcs._1.role + " for " + pfcs._2.clazz.name)
      log(pfcs._2.clazz.name + " plays " + pfcs._2.role + " for " + pfcs._1.clazz.name)

      val relationshipQualifier = pfcs._1.role.qualifier
      require(relationshipQualifier.equalsStructure(pfcs._2.role.qualifier))

      val relationshipName = extract.lastName(relationshipQualifier)

      val relationshipModuleName = relationshipName.toString + pfcs._1.clazz.name + pfcs._2.clazz.name

      val parent = create.qualifierWithTypeName(relationshipQualifier, RelationshipClassPrefix + relationshipName)
      val self = create.selfVal()
      val body = List(
        create.initDef,
        create.roleType(pfcs._1.role.name, pfcs._1.clazz.name),
        create.roleType(pfcs._2.role.name, pfcs._2.clazz.name),
        createRoleClassWrapper(pfcs._1.role.name, pfcs._2.clazz.name, pfcs._2.role.name),
        createRoleClassWrapper(pfcs._2.role.name, pfcs._1.clazz.name, pfcs._1.role.name)
      )

      create.playsForRelationshipModule(relationshipModuleName, parent, self, body)
    }

    def createRoleClassWrapper(roleName: Name, counterClassName: Name, counterRoleName: Name) = {

      val actionWrapperDef = create.actionWrapperDef(counterClassName, counterRoleName) _
      val actionMultiWrapperDef = create.actionMultiWrapperDef(counterClassName, counterRoleName) _
      val replaceWrapperDef = create.replaceWrapperDef(counterClassName, counterRoleName) _

      val addWrapperDef = actionWrapperDef(AddPrefix)
      val addMultiWrapperDef = actionMultiWrapperDef(AddPrefix)
      val removeWrapperDef = actionWrapperDef(RemovePrefix)
      val removeMultiWrapperDef = actionMultiWrapperDef(RemovePrefix)
      val replaceOneWrapperDef = replaceWrapperDef(false)
      val replaceMultiWrapperDef = replaceWrapperDef(true)
      val clearWrapperDef = create.clearWrapperDef(counterClassName, counterRoleName)
      val getWrapperDef = create.getWrapperDef(counterClassName, counterRoleName)

      val parent = create.select(create.zuper, roleName)
      val self = create.roleSelf(roleName)
      val body = List(addWrapperDef, addMultiWrapperDef, removeWrapperDef, removeMultiWrapperDef, replaceOneWrapperDef,
        replaceMultiWrapperDef, clearWrapperDef, getWrapperDef)

      create.roleClassWrapper(roleName, parent, self, body)
    }

    private def transformPlaysForClasses(playsForRelationshipModuleName: List[ModuleDef],
                                         playsForClauses: List[(PlaysForClause, PlaysForClause)]): List[ClassDef] = {

      // Create a List with Pairs of Class and Parent
      val classesAndParents = playsForRelationshipModuleName.map(_.name).zip(playsForClauses).flatMap {
        z =>
          val (playsForRelationshipModuleName, (pfcOne, pfcTwo)) = z
          val createSelect = create.select(playsForRelationshipModuleName) _
          val classAndParentOne = (pfcOne.clazz, createSelect(pfcOne.role.name))
          val classAndParentTwo = (pfcTwo.clazz, createSelect(pfcTwo.role.name))
          List(classAndParentOne, classAndParentTwo)
      }

      // Group this list by class and transform the class with it's parents
      classesAndParents groupBy {
        _._1
      } map {
        m =>
          val (clazz, classAndParents) = m
          // Create a List of all the parents
          val parents = classAndParents.map(_._2)
          // And transform it
          transformPlaysForClass(clazz, parents)
      } toList
    }

    private def transformPlaysForClass(clazz: ClassDef, parents: List[Select]): ClassDef = {

      val oldParents = create.parentsWithoutAnyRef(clazz.impl.parents)

      for {
        pOne <- oldParents
        pTwo <- parents
        if check.sameLastName(pOne, pTwo)
      } abort("Class %s already plays role %s!".format(clazz.name, extract.lastName(pOne)))


      copy.ClassDef(clazz)(impl = copy.Template(clazz.impl)(parents = oldParents ::: parents))
    }
  }

}
