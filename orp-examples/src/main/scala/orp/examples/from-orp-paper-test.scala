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

import java.lang.reflect.Modifier

/**
 * Some simple tests.
 *
 * @author Simon Olofsson {@literal <simon@olofsson.de>}
 */
object test {

  def testRelationship() {
    assert(Modifier.isAbstract(classOf[ACaggregation].getModifiers))
    assert(classOf[ACaggregation].getMethods.exists(_.getName == "contains"))
    assert(aggregation.isInstanceOf[ACaggregation])
  }

  def testRole() {
    for (c <- List((classOf[aggregation.Whole], "Part"), (classOf[aggregation.Part], "Whole"),
      (classOf[Directory], "File"), (classOf[File], "Directory"))) {
      assert(c._1.getMethods.exists(_.getName == s"add${c._2}"))
      assert(c._1.getMethods.exists(_.getName == s"add${c._2}s"))
      assert(c._1.getMethods.exists(_.getName == s"remove${c._2}"))
      assert(c._1.getMethods.exists(_.getName == s"remove${c._2}s"))
      assert(c._1.getMethods.exists(_.getName == s"replace${c._2}"))
      assert(c._1.getMethods.exists(_.getName == s"replace${c._2}s"))
      assert(c._1.getMethods.exists(_.getName == s"clear${c._2}s"))
      assert(c._1.getMethods.exists(_.getName == s"get${c._2}s"))
    }
    assert(classOf[aggregation.Whole].getMethods.exists(_.getName == "contains"))
  }

  def testPlays() {
    assert(classOf[aggregation.Whole].isAssignableFrom(classOf[Assembly]))
    assert(classOf[aggregation.Part].isAssignableFrom(classOf[Assembly]))
    assert(classOf[aggregation.Part].isAssignableFrom(classOf[Piece]))

    val pieceFirst = new Piece()
    val pieceSecond = new Piece()
    val pieceThird = new Piece()
    val pieceFourth = new Piece()
    val assemblyFirst = new Assembly()

    assert(pieceFirst.getWholes.isInstanceOf[Immutable])
    assert(assemblyFirst.getParts.isInstanceOf[Immutable])

    assemblyFirst.addPart(pieceFirst)
    assert(pieceFirst.getWholes.size == 1)
    assert(pieceFirst.getWholes.contains(assemblyFirst))
    assert(assemblyFirst.getParts.size == 1)
    assert(assemblyFirst.getWholes.isEmpty)
    assert(assemblyFirst.contains(pieceFirst))

    val assemblySecond = new Assembly()
    assemblySecond.addPart(assemblyFirst)
    assert(assemblyFirst.getParts.size == 1)
    assert(assemblyFirst.getWholes.size == 1)
    assert(assemblyFirst.getWholes.contains(assemblySecond))
    assert(assemblySecond.getParts.size == 1)
    assert(assemblySecond.getWholes.isEmpty)
    assert(assemblySecond.contains(assemblyFirst))

    assemblyFirst.removePart(pieceFirst)
    assert(pieceFirst.getWholes.isEmpty)
    assert(assemblyFirst.getParts.isEmpty)
    assert(assemblyFirst.getWholes.size == 1)

    assemblySecond.removePart(assemblyFirst)
    assert(assemblyFirst.getParts.isEmpty)
    assert(assemblyFirst.getWholes.isEmpty)
    assert(assemblySecond.getParts.isEmpty)
    assert(assemblySecond.getWholes.isEmpty)

    assemblyFirst.addParts(List(pieceFirst, pieceSecond))
    assert(assemblyFirst.getParts.size == 2)
    assert(assemblyFirst.getParts.sameElements(List(pieceFirst, pieceSecond)))
    assemblyFirst.removeParts(List(pieceFirst, pieceSecond))
    assert(assemblyFirst.getParts.isEmpty)

    assemblyFirst.addPart(pieceFirst)
    assert(assemblyFirst.getParts.size == 1)
    assert(assemblyFirst.getParts.contains(pieceFirst))
    assemblyFirst.replacePart(pieceFirst, pieceSecond)
    assert(assemblyFirst.getParts.size == 1)
    assert(assemblyFirst.getParts.contains(pieceSecond))
    assemblyFirst.removePart(pieceSecond)
    assert(assemblyFirst.getParts.isEmpty)

    assemblyFirst.addParts(List(pieceFirst, pieceSecond))
    assert(assemblyFirst.getParts.size == 2, assemblyFirst.getParts.size)
    assert(assemblyFirst.getParts.sameElements(List(pieceFirst, pieceSecond)))
    assemblyFirst.replaceParts(List(pieceFirst, pieceSecond), List(pieceThird, pieceFourth))
    assert(assemblyFirst.getParts.size == 2)
    assert(assemblyFirst.getParts.sameElements(List(pieceThird, pieceFourth)))
    assemblyFirst.clearParts()
    assert(assemblyFirst.getParts.isEmpty)
  }

  def testPlaysFor() {
    assert(aggregationDirectoryFile.isInstanceOf[ACaggregation])
    assert(classOf[aggregationDirectoryFile.Whole].isAssignableFrom(classOf[Directory]))
    assert(classOf[aggregationDirectoryFile.Part].isAssignableFrom(classOf[File]))

    val file = new File()
    val directory = new Directory()
    directory.addPart(file)
    assert(file.getWholes.size == 1)
    assert(file.getWholes.contains(directory))
    assert(directory.getParts.size == 1)
    assert(directory.contains(file))

    directory.removePart(file)
    assert(file.getWholes.isEmpty)
    assert(directory.getParts.isEmpty)
  }

  def testPlaysForWrapperMethods() {

    val file = new File()
    val directory = new Directory()
    directory.addFile(file)
    assert(file.getDirectorys.size == 1)
    assert(file.getDirectorys.contains(directory))
    assert(directory.getFiles.size == 1)
    assert(directory.contains(file))

    directory.removeFile(file)
    assert(file.getDirectorys.isEmpty)
    assert(directory.getFiles.isEmpty)

    directory.addFiles(List(new File(), new File()))
    assert(directory.getFiles.size == 2)
    directory.clearFiles()
    assert(directory.getFiles.isEmpty)
  }

  def run() {

    testRelationship()
    testRole()
    testPlays()
    testPlaysFor()
    testPlaysForWrapperMethods()

    println("Everything went fine :-)")
  }
}
