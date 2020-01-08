/**
 * *****************************************************************************
 * Copyright (c) 2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.saf

import org.scalatest._
import java.io._

import kr.ac.kaist.saf.clone_detector.{ Node, VectorGenerator }

import scala.io.Source
import scala.util.{ Failure, Success, Try }
import scala.util.Random.shuffle
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.parser.Parser
import kr.ac.kaist.saf.phase._

object ParseTest extends Tag("ParseTest")
object CloneTest extends Tag("CloneTest")

class CoreTest extends FlatSpec with BeforeAndAfterAll {
  val SEP = File.separator
  val testDir = BASE_DIR + SEP + "tests" + SEP
  val solDir = testDir + "all" + SEP
  val fixedDir = testDir + "fixed" + SEP
  val resDir = testDir + "result" + SEP

  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator: Iterator[File] = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }

  val solFilter = new FilenameFilter() {
    def accept(dir: File, name: String): Boolean = name.endsWith(".sol")
  }

  def normalized(s: String): String = s.replaceAll("\\s+", "").replaceAll("\\n+", "")

  def readFile(filename: String): String = {
    assert(new File(filename).exists)
    normalized(Source.fromFile(filename).getLines.mkString(LINE_SEP))
  }

  private def parseTest(sourceunit: Try[SourceUnit]): Unit = {
    sourceunit match {
      case Failure(e) =>
        println(e.toString)
        assert(false)
      case Success(su) =>
        Parser.stringToAST(su.toString(0)) match {
          case Failure(_) => assert(false)
          case Success((sourceunit, _)) =>
            val pretty = sourceunit.toString(0)
            Parser.stringToAST(pretty) match {
              case Failure(_) => assert(false)
              case Success((p, _)) =>
                assert(normalized(p.toString(0)) == normalized(pretty))
            }
        }
    }
  }
  private def cloneTest(vectors: Try[List[Node]], name: String): Unit = {
    vectors match {
      case Failure(e) =>
        println(e.toString)
        assert(false)
      case Success(vv) =>
        val v = vv.map(VectorGenerator.toString)
        assert(readFile(name) == normalized(v.map(s => s + "\n").mkString))
    }
  }

  val testSafConfig: SafConfig = SafConfig(CmdBase, Nil)

  // Permute filenames for randomness
  for (filename <- shuffle(new File(solDir).list(solFilter).toSeq)) {
    val solName = solDir + filename
    val config = testSafConfig.copy(fileNames = List(solName))
    lazy val su = Parse((), config)
    registerTest("[Parse] " + filename, ParseTest) { parseTest(su) }
  }

  // test cases for solution files in fixed folder.
  for (filename <- shuffle(new File(fixedDir).list(solFilter).toSeq)) {
    val solName = fixedDir + filename
    val name = filename.substring(0, filename.lastIndexOf("."))
    val testName = resDir + name + ".clone.test"

    val config = testSafConfig.copy(fileNames = List(solName))
    val clonecfg = CloneConfig()
    lazy val su = Parse((), config)
    lazy val vectors = su.flatMap(Clone(_, config, clonecfg))
    registerTest("[CloneDetector] " + filename, CloneTest) { cloneTest(vectors, testName) }
  }
}
