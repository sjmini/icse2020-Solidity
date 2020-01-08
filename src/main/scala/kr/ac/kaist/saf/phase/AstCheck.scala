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

import java.io._
import scala.util.{ Try, Success, Failure }
import kr.ac.kaist.saf.{ LINE_SEP, SafConfig }
import kr.ac.kaist.saf.ast_checker.ASTChecker
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.errors.error.NoFileError
import scala.collection.mutable._

// AstCheck phase
case object AstCheck extends PhaseObj[SourceUnit, AstCheckConfig, Unit] {
  val name = "astchecker"
  val help = "AstChecker." + LINE_SEP +
    "It produces useful information in the result file after parsing."

  var usp = 0
  var pfwmsvm_n = 0

  def apply(
    unit: SourceUnit,
    safConfig: SafConfig,
    config: AstCheckConfig
  ): Try[Unit] = safConfig.fileNames match {
    case Nil => Failure(NoFileError("astcheck"))
    case List(f) =>
      ASTChecker.run(unit) match {
        case (s: (String, String, Int, Int)) =>
          val writer = new PrintWriter(new FileOutputStream(new File("USP.txt"), true))
          val writer2 = new PrintWriter(new FileOutputStream(new File("FV.txt"), true))
          if (s._1 != "") {
            usp = usp + s._3
            writer.write(f + "\n\n" + s._1 + "USP: " + usp.toString + "\n\n")
          }
          if (s._2 != "") {
            pfwmsvm_n = pfwmsvm_n + s._4
            writer2.write(f + "\n\n" + s._2 + "PFWMSVM: " + pfwmsvm_n.toString + "\n\n")
          }
          writer.close()
          writer2.close()
          Success(())
        case _ => Failure(new Error())
      }
  }

  def defaultConfig: AstCheckConfig = AstCheckConfig()
  val options: List[PhaseOption[AstCheckConfig]] = Nil
}

// Parse phase config
case class AstCheckConfig() extends Config
