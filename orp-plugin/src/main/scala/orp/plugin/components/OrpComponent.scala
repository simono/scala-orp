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

import annotation.StaticAnnotation
import tools.nsc.plugins.PluginComponent
import tools.nsc.ast.TreeDSL
import tools.nsc.transform.Transform
import java.beans.Introspector

/**
 * {@link PluginComponent} for ORP.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
private[components] trait OrpComponent extends PluginComponent with Transform with TreeDSL {

  import global._

  override val runsAfter = Nil

  /**
   * {@link OrpTransformer} that transforms multiple {@link Tree}s.
   */
  trait OrpTransformerMulti extends OrpTransformer {

    val checkTree: PartialFunction[Tree, Boolean]

    def transform(trees: List[Tree]): List[Tree]

    override val transformTree: PartialFunction[Tree, Tree] = {

      case packageDef@PackageDef(_, stats) if checkTrees(stats) =>
        copy.PackageDef(packageDef)(stats = transform(stats))

      case moduleDef@ModuleDef(_, _, impl@Template(_, _, body)) if checkTrees(body) =>
        copy.ModuleDef(moduleDef)(impl = copy.Template(impl)(body = transform(body)))
    }

    def checkTrees(trees: List[Tree]) = {
      check.exists(trees)(checkTree)
    }
  }

  /**
   * {@link Transformer} for ORP transformations.
   */
  trait OrpTransformer extends Transformer {

    val transformTree: PartialFunction[Tree, Tree]

    override def transform(tree: Tree): Tree = {
      super.transform(
        if (transformTree.isDefinedAt(tree)) {
          try {
            transformTree(tree)
          } catch {
            case x =>
              logThrowable(x)
              throw x
          }
        }
        else tree
      )
    }

    val OpMinusEq = mkEqlOp(nme.MINUS)
    val OpPlusEq = mkEqlOp(nme.PLUS)

    private def mkEqlOp(termName: TermName) = termName.append(nme.EQL)

    val AddPrefix = "add"
    val NewPrefix = "new"
    val OldPrefix = "old"
    val RelationshipClassPrefix = "AC"
    val RemovePrefix = "remove"
    val ReplacePrefix = "replace"

    object check {

      def annotation(mods: Modifiers, annotation: Class[_ <: StaticAnnotation]) = {
        extract.annotationArgs(mods, annotation).nonEmpty
      }

      def exists(trees: List[Tree])(fun: PartialFunction[Tree, Boolean]) = {
        trees exists {
          t => fun.isDefinedAt(t) && fun(t)
        }
      }

      def isTrait(classDef: ClassDef) = classDef.mods.hasFlag(Flag.TRAIT)

      def sameLastName(qOne: Tree, qTwo: Tree) = extract.lastName(qOne) == extract.lastName(qTwo)

      def annotationValueName(annotation: Class[_ <: StaticAnnotation], value: AnyRef)(mods: Modifiers) = {
        val values = extract.annotationValuesFlat(mods, annotation)
        assert(values.size <= 1)
        values.exists(extract.lastName(_).toString == value.toString)
      }

    }

    object extract {

      def annotationArgs(mods: Modifiers, annotation: Class[_ <: StaticAnnotation]): List[List[Tree]] = {
        mods.annotations collect {
          case Apply(Select(New(Ident(name)), _), args) if name.toString == annotation.getSimpleName => args
        }
      }

      def annotationValues(mods: Modifiers, annotation: Class[_ <: StaticAnnotation]): List[List[Tree]] = {

        val args = annotationArgs(mods, annotation)
        if (args.isEmpty) {
          abort(s"Annotation '${annotation.getSimpleName}' not found in '${mods.annotations}'!")
        }

        args map {
          _ map {
            case TypeApply(_, List(arg)) => arg
            case ident: Ident => ident
            case x => abort(s"Unkown annotation argument $x for annotation ${annotation.getName}!")
          }
        }
      }

      def annotationValuesFlat(mods: Modifiers, annotation: Class[_ <: StaticAnnotation]): List[Tree] = {
        annotationValues(mods, annotation).flatMap(identity)
      }

      def selectsAndNamesFromAnnotationValues(annotation: Class[_ <: StaticAnnotation], mods: Modifiers): List[(Select, TypeName)] = {
        extract.annotationValues(mods, annotation) map {
          v =>
            val List(select: Select, Ident(name: TypeName)) = v
            (select, name)
        }
      }

      def classDefsWithAnnotation(trees: List[Tree], annotation: Class[_ <: StaticAnnotation]): List[ClassDef] = {
        trees collect {
          case c: ClassDef if check.annotation(c.mods, annotation) => c
        }
      }

      def lastName(qualifier: Tree): Name = {
        qualifier match {
          case Ident(name) => name
          case Select(_, name) => name
          case x => abort(s"Unknown qualifier: $qualifier")
        }
      }
    }

    object copy {

      def ClassDef(classDef: ClassDef)(mods: Modifiers = classDef.mods, name: TypeName = classDef.name,
                                       tparams: List[TypeDef] = classDef.tparams, impl: Template = classDef.impl) = {
        treeCopy.ClassDef(classDef, mods, name, tparams, impl)
      }

      def ModuleDef(moduleDef: ModuleDef)(mods: Modifiers = moduleDef.mods, name: TermName = moduleDef.name,
                                          impl: Template = moduleDef.impl) = {
        treeCopy.ModuleDef(moduleDef, mods, name, impl)
      }

      def PackageDef(packageDef: PackageDef)(pid: RefTree = packageDef.pid, stats: List[Tree] = packageDef.stats) = {
        treeCopy.PackageDef(packageDef, pid, stats)
      }

      def Template(template: Template)(parents: List[Tree] = template.parents, self: ValDef = template.self,
                                       body: List[Tree] = template.body) = {
        treeCopy.Template(template, parents, self, body)
      }
    }

    object create {

      import CODE._

      private val Traversable: TypeName = "Traversable"
      private val ArrayBuffer: TypeName = "ArrayBuffer"
      private val RoleTrait: TypeName = "RoleTrait"

      private val mutable: TermName = "mutable"
      private val orp: TermName = "orp"
      private val api: TermName = "api"
      private val require: TermName = "require"
      private val size: TermName = "size"

      private def emptyTypeName = nme.EMPTY.toTypeName

      private def thiz = This(emptyTypeName)

      def zuper = Super(thiz, emptyTypeName)

      def ident(name: TypeName) = Ident(name)

      def select(firstName: TermName)(lastName: TypeName): Select = Ident(firstName) DOT lastName

      def select(qualifier: Tree, lastName: TypeName) = qualifier DOT lastName

      def selfVal(tpt: Tree = TypeTree()) = ValDef(NoMods, nme.USCOREkw, tpt, EmptyTree)

      def initDef = DefDef(NoMods, nme.CONSTRUCTOR, Nil, Nil, TypeTree(), BLOCK(fn(zuper, nme.CONSTRUCTOR), EmptyTree))

      def qualifierWithTypeName(qualifier: Tree, name: TypeName): Tree = {
        qualifier match {
          case _: Ident => Ident(name)
          case Select(qual, _) => Select(qual, name)
          case x => abort(s"Unknown qualifier: $qualifier")
        }
      }

      private def mkImport(select: Select, name: TermName) = Import(select, List(ImportSelector(name, -1, name, -1)))

      def imports = {
        val arrayBuffer = mkImport(Ident("collection") DOT mutable, "ArrayBuffer")
        val roleTrait = mkImport(Ident(nme.ROOTPKG) DOT orp DOT api, "RoleTrait")
        List(arrayBuffer, roleTrait)
      }

      def parentsWithoutAnyRef(parents: List[Tree]) = {
        parents filterNot {
          p =>
            p match {
              case Select(Ident(nme.scala_), tpnme.AnyRef) => true
              case _ => false
            }
        }
      }

      def roleTypeAbstract(roleName: TypeName) = {
        TypeDef(Modifiers(Flag.DEFERRED), name.withRoleSuffix(roleName.toString), Nil,
          TypeBoundsTree(Ident(nme.ROOTPKG) DOT nme.scala_ DOT tpnme.Nothing, Ident(roleName)))
      }

      def roleType(roleName: String, className: TypeName) = {
        TypeDef(NoMods, name.withRoleSuffix(roleName), Nil, Ident(className))
      }

      def relationshipClass(relationshipClassName: TypeName, impl: Template) = {
        ClassDef(Modifiers(Flag.ABSTRACT), relationshipClassName, Nil, impl)
      }

      def othersVal(counterRoleName: String) = {
        ValDef(Modifiers(Flag.PRIVATE | Flag.LOCAL), name.othersVal(counterRoleName), TypeTree(),
          NEW(AppliedTypeTree(Ident(ArrayBuffer), List(Ident(name.withRoleSuffix(counterRoleName))))))
      }

      def actionDef(roleName: String, counterRoleName: String)(action: String) = {

        val counterRoleNameDecapitalized = name.decapitalize(counterRoleName)

        val vparamss = List(
          List(
            ValDef(NoMods, counterRoleNameDecapitalized,
              Ident(name.withRoleSuffix(counterRoleName)), EmptyTree)
          )
        )

        val rhs = BLOCK(
          fn(thiz, name.priActionDef(action, counterRoleName), Ident(counterRoleNameDecapitalized)),
          fn(Ident(counterRoleNameDecapitalized), name.priActionDef(action, roleName), thiz)
        )

        DefDef(NoMods, name.actionDef(action, counterRoleName), Nil, vparamss, UNIT, rhs)
      }

      def actionMultiDef(counterRoleName: String)(action: String) = {

        val counterRoleNameDecapitalized = name.plural(name.decapitalize(counterRoleName))

        val vparamss = List(
          List(
            ValDef(NoMods, counterRoleNameDecapitalized,
              AppliedTypeTree(Ident(Traversable), List(Ident(name.withRoleSuffix(counterRoleName)))), EmptyTree)
          )
        )

        val actionDef = name.actionDef(action, counterRoleName)

        val rhs = fn(Ident(counterRoleNameDecapitalized), nme.foreach, Ident(actionDef))

        DefDef(NoMods, name.plural(actionDef), Nil, vparamss, UNIT, rhs)
      }

      def replaceDef(counterRoleName: String)(multi: Boolean) = {

        val crn = if (multi) name.plural(counterRoleName).toString else counterRoleName

        def createVparam(classifier: String) = {
          val crnrs = Ident(name.withRoleSuffix(counterRoleName))
          val crnatt = AppliedTypeTree(Ident(Traversable), List(crnrs))
          val tpt = if (multi) crnatt else crnrs

          ValDef(NoMods, classifier + crn, tpt, EmptyTree)
        }

        val vparamss = List(
          List(createVparam(OldPrefix), createVparam(NewPrefix))
        )

        val rhs = BLOCK(
          fn(thiz, name.actionDef(RemovePrefix, crn), Ident(OldPrefix + crn)),
          fn(thiz, name.actionDef(AddPrefix, crn), Ident(NewPrefix + crn))
        )

        DefDef(NoMods, name.actionDef(ReplacePrefix, crn), Nil, vparamss, UNIT, rhs)
      }

      def clearDef(counterRoleName: String) = {
        val rhs = fn(thiz, name.plural(name.actionDef(RemovePrefix, counterRoleName)), Ident(name.getDef(counterRoleName)))
        DefDef(NoMods, name.clearDef(counterRoleName), Nil, List(Nil), UNIT, rhs)
      }

      def getDef(counterRoleName: String) = {

        // We use toList here, to get an Immutable List
        // toSeq would return an ArrayBuffer
        val rhs = Ident(name.othersVal(counterRoleName)) DOT nme.toList

        DefDef(NoMods, name.getDef(counterRoleName), Nil, Nil, TypeTree(), rhs)
      }

      def priAddOne(counterRoleName: String)(rhs: Tree) = {
        BLOCK(
          // Require that there is max. One
          fn(Ident(nme.Predef), require,
            fn(Ident(name.othersVal(counterRoleName)) DOT size, nme.LE, LIT(1))),
          // Remove the old One
          fn(Ident(name.othersVal(counterRoleName)), nme.foreach,
            Ident(name.actionDef(RemovePrefix, counterRoleName))),
          // Assert it's empty
          fn(Ident(nme.Predef), nme.assert_,
            Ident(name.othersVal(counterRoleName)) DOT nme.isEmpty),
          // Add the new One
          rhs,
          // Assume that there's One
          fn(Ident(nme.Predef), nme.assume_,
            fn(Ident(name.othersVal(counterRoleName)) DOT size, nme.EQ, LIT(1)))
        )
      }

      def priActionDef(relationshipClassName: TypeName, counterRoleName: String)
                      (action: String, op: TermName, createRhs: Tree => Tree) = {

        val counterRoleNameDecapitalized = name.decapitalize(counterRoleName)

        val vparamss = List(
          List(
            ValDef(NoMods, counterRoleNameDecapitalized,
              Ident(name.withRoleSuffix(counterRoleName)), EmptyTree)
          )
        )

        val rhs = createRhs {
          fn(Ident(name.othersVal(counterRoleName)), op, Ident(counterRoleNameDecapitalized))
        }

        DefDef(Modifiers(0, relationshipClassName), name.priActionDef(action, counterRoleName),
          Nil, vparamss, UNIT, rhs)
      }

      def roleSelf(roleName: String) = selfVal(Ident(name.withRoleSuffix(roleName)))

      def roleParents(parents: List[Tree]) = {
        Ident(RoleTrait) :: (parentsWithoutAnyRef(parents) filterNot {
          // Remove RoleTrait from parents
          p =>
            p match {
              case _ => extract.lastName(p).toTypeName == RoleTrait
            }
        })
      }

      def roleClassModifiers(mods: Modifiers) = mods &~ Flag.INTERFACE

      def playsForRelationshipModule(relationshipModuleName: TermName, parent: Tree, self: ValDef, body: List[Tree]) = {
        ModuleDef(NoMods, relationshipModuleName, Template(List(parent), self, body))
      }

      def overrideIfNeeded(firstName: String, secondName: String): Modifiers = {
        if (name.decapitalize(firstName) == name.decapitalize(secondName)) {
          Modifiers(Flag.OVERRIDE)
        } else {
          NoMods
        }
      }

      def actionWrapperDef(counterClassName: String, counterRoleName: String)(action: String) = {
        val counterClassNameDecapitalized = name.decapitalize(counterClassName)
        val mods = overrideIfNeeded(counterClassName, counterRoleName)
        DefDef(mods, name.actionDef(action, counterClassName), Nil,
          List(List(ValDef(NoMods, counterClassNameDecapitalized, Ident(stringToTypeName(counterClassName)), EmptyTree))),
          UNIT, fn(create.zuper, name.actionDef(action, counterRoleName), Ident(counterClassNameDecapitalized)))
      }

      def actionMultiWrapperDef(counterClassName: String, counterRoleName: String)(action: String) = {
        val counterClassNameDecapitalized = name.plural(name.decapitalize(counterClassName))
        val mods = overrideIfNeeded(counterClassName, counterRoleName)
        DefDef(mods, name.plural(name.actionDef(action, counterClassName)), Nil,
          List(List(ValDef(NoMods, counterClassNameDecapitalized,
            AppliedTypeTree(Ident(Traversable), List(Ident(stringToTypeName(counterClassName)))), EmptyTree))),
          UNIT, fn(create.zuper, name.plural(name.actionDef(action, counterRoleName)), Ident(counterClassNameDecapitalized)))
      }

      def replaceWrapperDef(counterClassName: String, counterRoleName: String)(multi: Boolean) = {

        val ccn: String = if (multi) name.plural(counterClassName) else counterClassName
        val crn: String = if (multi) name.plural(counterRoleName) else counterRoleName

        def createVparam(classifier: String) = {
          val ccnrs = Ident(stringToTypeName(counterClassName))
          val ccnatt = AppliedTypeTree(Ident(Traversable), List(ccnrs))
          val tpt = if (multi) ccnatt else ccnrs

          ValDef(NoMods, classifier + ccn, tpt, EmptyTree)
        }

        val mods = overrideIfNeeded(counterClassName, counterRoleName)

        val vparamss = List(List(createVparam(OldPrefix), createVparam(NewPrefix)))

        val rhs = fn(create.zuper, name.actionDef(ReplacePrefix, crn),
          Ident(OldPrefix + ccn), Ident(NewPrefix + ccn))

        DefDef(mods, name.actionDef(ReplacePrefix, ccn), Nil, vparamss, UNIT, rhs)
      }

      def clearWrapperDef(counterClassName: String, counterRoleName: String) = {
        val mods = overrideIfNeeded(counterClassName, counterRoleName)
        val rhs = create.zuper DOT name.clearDef(counterRoleName)
        DefDef(mods, name.clearDef(counterClassName), Nil, List(Nil), UNIT, rhs)
      }

      def getWrapperDef(counterClassName: String, counterRoleName: String) = {
        val mods = overrideIfNeeded(counterClassName, counterRoleName)
        DefDef(mods, name.getDef(counterClassName), Nil, Nil, TypeTree(),
          create.zuper DOT name.getDef(counterRoleName))
      }

      def roleClassWrapper(roleName: TypeName, parent: Tree, self: ValDef, body: List[Tree]) = {
        ClassDef(Modifiers(Flag.ABSTRACT | Flag.TRAIT), roleName, Nil, Template(List(parent), self, body))
      }

      private object name {

        private val ClearPrefix = "clear"
        private val GetPrefix = "get"
        private val PriPrefix = "pri"
        private val PluralSuffix = "s"
        private val RoleSuffix = "Role"

        def othersVal(name: String): TermName = {
          decapitalize(name) + PluralSuffix
        }

        def actionDef(actionName: String, name: String): TermName = {
          actionName + capitalize(name)
        }

        def clearDef(name: String): TermName = {
          ClearPrefix + capitalize(name) + PluralSuffix
        }

        def getDef(name: String): TermName = {
          GetPrefix + capitalize(name) + PluralSuffix
        }

        def priActionDef(actionName: String, name: String): TermName = {
          PriPrefix + capitalize(actionName) + capitalize(name)
        }

        def withRoleSuffix(name: String): TypeName = {
          name + RoleSuffix
        }

        def decapitalize(name: String) = {
          Introspector.decapitalize(name)
        }

        def plural(name: String): TermName = {
          name + PluralSuffix
        }

        private def capitalize(name: String) = {
          name.capitalize
        }
      }

    }

  }

}
