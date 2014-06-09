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

import collection.mutable.ArrayBuffer
import orp.api.RoleTrait

/**
 * Examples taken from the ORP paper.
 *
 * @author Simon Olofsson
 */
abstract class ACaggregation {

  type WholeRole <: Whole
  type PartRole <: Part

  trait Whole extends RoleTrait {

    this: WholeRole =>

    private[this] val parts = new ArrayBuffer[PartRole]()

    def addPart(part: PartRole) {
      this priAddPart part
      part priAddWhole this
    }

    def addParts(parts: Traversable[PartRole]) {
      parts.foreach(addPart)
    }

    def removePart(part: PartRole) {
      this priRemovePart part
      part priRemoveWhole this
    }

    def removeParts(parts: Traversable[PartRole]) {
      parts.foreach(removePart)
    }

    def replacePart(oldPart: PartRole, newPart: PartRole) {
      removePart(oldPart)
      addPart(newPart)
    }

    def replaceParts(oldParts: Traversable[PartRole], newParts: Traversable[PartRole]) {
      removeParts(oldParts)
      addParts(newParts)
    }

    def clearParts() {
      removeParts(getParts)
    }

    def getParts = parts.toList

    private[ACaggregation] def priAddPart(part: PartRole) {
      parts += part
    }

    private[ACaggregation] def priRemovePart(part: PartRole) {
      parts -= part
    }

    def contains(part: Part): Boolean = {
      for (p <- getParts) {
        if (p == part) return true
        if (p.isInstanceOf[Whole]) {
          if (p.asInstanceOf[Whole] contains part)
            return true
        }
      }
      return false
    }
  }

  trait Part extends RoleTrait {

    this: PartRole =>

    private[this] val wholes = new ArrayBuffer[WholeRole]()

    def addWhole(whole: WholeRole) {
      this priAddWhole whole
      whole priAddPart this
    }

    def addWholes(wholes: Traversable[WholeRole]) {
      wholes.foreach(addWhole)
    }

    def removeWhole(whole: WholeRole) {
      this priRemoveWhole whole
      whole priRemovePart this
    }

    def removeWholes(wholes: Traversable[WholeRole]) {
      wholes.foreach(removeWhole)
    }

    def replaceWhole(oldWhole: WholeRole, newWhole: WholeRole) {
      this removeWhole oldWhole
      this addWhole newWhole
    }

    def replaceWholes(oldWholes: Traversable[WholeRole], newWholes: Traversable[WholeRole]) {
      this removeWholes oldWholes
      this addWholes newWholes
    }

    def clearWholes() {
      removeWholes(getWholes)
    }

    def getWholes = wholes.toList

    private[ACaggregation] def priAddWhole(whole: WholeRole) {
      // If Counterrole has multiplicity of One
      // wholes.foreach(removeWhole)
      wholes += whole
    }

    private[ACaggregation] def priRemoveWhole(whole: WholeRole) {
      wholes -= whole
    }
  }

  def contains(whole: Whole, part: Part): Boolean = whole contains part
}

object aggregation extends ACaggregation {
  type WholeRole = Whole
  type PartRole = Part
}

class Assembly extends aggregation.Whole with aggregation.Part

class Piece extends aggregation.Part

object aggregationDirectoryFile extends ACaggregation {

  type WholeRole = Directory
  type PartRole = File

  // We extend the trait here, to add some convenience methods
  // if this is considered useful, there are two todos:
  // TODO so: 1. Mark the get, add and remove methods in super.<Role> protected
  // TODO so: 2. add public Wrappers (like the ones below) in <relationship>.<Role>
  trait Whole extends super.Whole {

    this: WholeRole =>

    def addFile(file: File) {
      super.addPart(file)
    }

    def addFiles(files: Traversable[File]) {
      super.addParts(files)
    }

    def removeFile(file: File) {
      super.removePart(file)
    }

    def removeFiles(files: Traversable[File]) {
      super.removeParts(files)
    }

    def replaceFile(oldFile: File, newFile: File) {
      super.replacePart(oldFile, newFile)
    }

    def replaceFiles(oldFiles: Traversable[File], newFiles: Traversable[File]) {
      super.replaceParts(oldFiles, newFiles)
    }

    def clearFiles() {
      super.clearParts()
    }

    def getFiles = super.getParts
  }

  trait Part extends super.Part {

    this: PartRole =>

    def addDirectory(directory: Directory) {
      super.addWhole(directory)
    }

    def addDirectorys(directorys: Traversable[Directory]) {
      super.addWholes(directorys)
    }

    def removeDirectory(directory: Directory) {
      super.removeWhole(directory)
    }

    def removeDirectorys(directorys: Traversable[Directory]) {
      super.removeWholes(directorys)
    }

    def replaceDirectory(oldDirectory: Directory, newDirectory: Directory) {
      super.replaceWhole(oldDirectory, newDirectory)
    }

    def replaceDirectorys(oldDirectorys: Traversable[Directory], newDirectorys: Traversable[Directory]) {
      super.replaceWholes(oldDirectorys, newDirectorys)
    }

    def clearDirectorys() {
      super.clearWholes()
    }

    def getDirectorys = super.getWholes
  }

}

class Directory extends aggregationDirectoryFile.Whole

class File extends aggregationDirectoryFile.Part
