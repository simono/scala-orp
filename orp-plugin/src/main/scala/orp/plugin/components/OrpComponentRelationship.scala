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
import orp.api.{relationship, role, One}

/**
 * An {@link OrpComponent} for {@link relationship} transformations.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
class OrpComponentRelationship(val global: Global) extends OrpComponent {

  import global._

  override val runsRightAfter = Some("parser")
  override val phaseName = "orptransformrelationship"

  override protected def newTransformer(unit: CompilationUnit) = new OrpTransformerRelationship

  /**
   * A {@link relationship} {@link OrpTransformerMulti}.
   */
  class OrpTransformerRelationship extends OrpTransformerMulti {

    override val checkTree: PartialFunction[Tree, Boolean] = {
      case ModuleDef(mods, _, _) => check.annotation(mods, classOf[relationship])
    }

    override def transform(trees: List[Tree]) = {

      require(checkTrees(trees))

      val treesNew =
        trees flatMap {
          case moduleDef@ModuleDef(mods, name, _) if check.annotation(mods, classOf[relationship]) =>

            val roles = extract.classDefsWithAnnotation(moduleDef.impl.body, classOf[role])
            if (roles.size != 2) abort("Found %d roles in relationship '%s'. Expected two roles!".format(roles.size, name))
            val List(roleFirst, roleSecond) = roles;
            roles foreach {
              r => if (!check.isTrait(r)) abort("Role '%s' needs to be a trait!".format(r.name))
            }

            log("Relationship " + name + " with Roles " + roleFirst.name + " and " + roleSecond.name)

            val relationshipClass = createRelationshipClass(moduleDef, roleFirst, roleSecond)
            val relationshipModule = transformRelationshipModule(moduleDef, relationshipClass.name,
              roleFirst.name, roleSecond.name)

            List(relationshipClass, relationshipModule)
          case stat => List(stat)
        }

      assume(trees != treesNew, "Trees weren't transformed!")

      create.imports ::: treesNew
    }

    private def createRelationshipClass(relationshipModule: ModuleDef, roleFirst: ClassDef, roleSecond: ClassDef) = {

      require(relationshipModule.exists(roleFirst == _))
      require(relationshipModule.exists(roleSecond == _))

      val relationshipClassName = relationshipModule.name.prepend(RelationshipClassPrefix).toTypeName

      val roleTypeFirst = create.roleTypeAbstract(roleFirst.name.toString)
      val roleTypeSecond = create.roleTypeAbstract(roleSecond.name.toString)

      val trc = transformRoleClass(relationshipClassName) _
      val holdsOne = check.annotationValueName(classOf[role], One) _
      val roleFirstNew = trc(roleFirst, roleSecond.name.toString, holdsOne(roleSecond.mods))
      val roleSecondNew = trc(roleSecond, roleFirst.name.toString, holdsOne(roleFirst.mods))

      val body = roleTypeFirst :: roleTypeSecond :: roleFirstNew :: roleSecondNew ::
        (relationshipModule.impl.body filterNot {
          e => e == roleFirst || e == roleSecond
        })

      create.relationshipClass(relationshipClassName, copy.Template(relationshipModule.impl)(body = body))
    }

    private def transformRoleClass(relationshipClassName: TypeName)
                                  (roleClass: ClassDef, counterRoleName: String, holdsOne: Boolean) = {

      val actionDef = create.actionDef(roleClass.name.toString, counterRoleName) _
      val actionMultiDef = create.actionMultiDef(counterRoleName) _
      val replaceDef = create.replaceDef(counterRoleName) _
      val priActionDef = create.priActionDef(relationshipClassName, counterRoleName) _

      val othersVal = create.othersVal(counterRoleName)
      val addDef = actionDef(AddPrefix)
      val addMultiDef = actionMultiDef(AddPrefix)
      val removeDef = actionDef(RemovePrefix)
      val removeMultiDef = actionMultiDef(RemovePrefix)
      val replaceOneDef = replaceDef(false)
      val replaceMultiDef = replaceDef(true)
      val clearDef = create.clearDef(counterRoleName)
      val getDef = create.getDef(counterRoleName)
      val priAddDef = priActionDef(AddPrefix, OpPlusEq, if (holdsOne) create.priAddOne(counterRoleName) else identity)
      val priRemoveDef = priActionDef(RemovePrefix, OpMinusEq, identity)

      val parents = create.roleParents(roleClass.impl.parents)
      val self = create.roleSelf(roleClass.name.toString)
      val body = othersVal :: addDef :: addMultiDef :: removeDef :: removeMultiDef :: replaceOneDef :: replaceMultiDef ::
        clearDef :: getDef :: priAddDef :: priRemoveDef :: roleClass.impl.body
      val impl = copy.Template(roleClass.impl)(parents = parents, self = self, body = body)

      copy.ClassDef(roleClass)(mods = create.roleClassModifiers(roleClass.mods), impl = impl)
    }

    private def transformRelationshipModule(relationshipModule: ModuleDef, relationshipClassName: TypeName,
                                            roleFirstName: TypeName, roleSecondName: TypeName) = {

      def roleType(roleName: TypeName) = create.roleType(roleName.toString, roleName)
      val roleTypeFirst = roleType(roleFirstName)
      val roleTypeSecond = roleType(roleSecondName)

      val parents = create.ident(relationshipClassName) :: create.parentsWithoutAnyRef(relationshipModule.impl.parents)
      val body = List(create.initDef, roleTypeFirst, roleTypeSecond)

      copy.ModuleDef(relationshipModule)(impl = copy.Template(relationshipModule.impl)(parents = parents, body = body))
    }
  }

}
