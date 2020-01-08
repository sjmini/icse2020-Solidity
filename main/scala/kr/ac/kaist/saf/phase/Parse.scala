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

import scala.util.{ Try, Failure }
import kr.ac.kaist.saf.{ LINE_SEP, SafConfig }
import kr.ac.kaist.saf.parser.Parser
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.errors.error.NoFileError

// Parse phase
case object Parse extends PhaseObj[Unit, ParseConfig, SourceUnit] {
  val name = "parser"
  val help = "Parses files." + LINE_SEP +
    "If multiple files are given, they are concatenated in the given order before being parsed."

  def apply(
    unit: Unit,
    safConfig: SafConfig,
    config: ParseConfig
  ): Try[SourceUnit] = safConfig.fileNames match {
    case Nil => Failure(NoFileError("parse"))
    case _ => Parser.fileToAST(safConfig.fileNames).map {
      case (su, excLog) => {
        // Report errors.
        if (excLog.hasError) {
          println(su.relFileName + ":")
          println(excLog)
        }

        // Pretty print to file.
        config.outFile match {
          case Some(out) => {
            val (fw, writer) = Useful.fileNameToWriters(out)
            writer.write(su.toString(0))
            writer.close; fw.close
            println("Dumped parsed JavaScript code to " + out)
          }
          case None =>
        }

        su
      }
    }
  }

  def defaultConfig: ParseConfig = ParseConfig()
  val options: List[PhaseOption[ParseConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the parsed JavaScript code will be written to the outfile.")
  )
}

// Parse phase config
case class ParseConfig(
  var outFile: Option[String] = None
) extends Config
