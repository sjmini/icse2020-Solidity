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

package kr.ac.kaist.saf.phase

import scala.util.{ Success, Try }
import kr.ac.kaist.saf.SafConfig
import kr.ac.kaist.saf.clone_detector.{ Node, VectorGenerator }
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.util._

// Clone phase
case object Clone extends PhaseObj[SourceUnit, CloneConfig, List[Node]] {
  val name = "clone"
  val help = "Generates clone vectors in Solidity source files"

  def apply(
    su: SourceUnit,
    safConfig: SafConfig,
    config: CloneConfig
  ): Try[List[Node]] = {
    val vectors = VectorGenerator.generate(su)
    val v = vectors.map(VectorGenerator.toString)

    config.outFile match {
      case Some(o) =>
        val (fw, writer) = Useful.fileNameToWriters(o)
        v.foreach(p => writer.write(p + "\n"))
        writer.close()
        fw.close()
        println(s"Dumped clone vectors to $o")
      case None =>
    }

    Success(vectors)
  }

  def defaultConfig: CloneConfig = CloneConfig()
  val options: List[PhaseOption[CloneConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the generated clone vectors will be written to the outfile.")
  )
}

// Clone phase config
case class CloneConfig(
  var outFile: Option[String] = None
) extends Config
