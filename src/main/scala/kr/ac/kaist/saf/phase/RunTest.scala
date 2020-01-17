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
import kr.ac.kaist.saf.ast_checker.INHChecker
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.errors.error.NoFileError
import scala.collection.mutable._

// RunTest phase
case object RunTest extends PhaseObj[SourceUnit, RunTestConfig, Unit] {
  val name = "runtest"
  val help = "RunTest." + LINE_SEP +
    "It produces useful information in the result file after parsing."

  var final_string = ""

  def apply(
    unit: SourceUnit,
    safConfig: SafConfig,
    config: RunTestConfig
  ): Try[Unit] = safConfig.fileNames match {
    case Nil => Failure(NoFileError("runtest"))
    case List(f) =>

      val writer = new PrintWriter(new FileOutputStream(new File("test_result.txt"), false))

      ASTChecker.run(unit) match {
        case (s: (String, String, Int, Int)) =>
          if (s._1 != "") {
            final_string += "Uninitialized storage pointer" + "\n" + s._1 + "\n"
          }
          if (s._2 != "") {
            final_string += "Function without visibility" + "\n" + s._2 + "\n"
          }
          Success(())
        case _ => Failure(new Error())
      }

      val walker_order = new INHChecker()
      val (print_order, _, _, _, _, _, _) = walker_order.run(unit, 8)
      if (!print_order.isEmpty) {
        final_string += "Inheritance order confusion" + "\n" + print_order + "\n"
      }

      val walker = new INHChecker()
      val (print_depre, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(unit, 1)
      if (!print_unary.isEmpty) {
        final_string += "Typo of the += operator" + "\n" + print_unary + "\n"
      }
      if (!print_shadow.isEmpty) {
        final_string += "Storage variable shadowing confusion" + "\n" + print_shadow + "\n"
      }
      if (!print_constructor.isEmpty) {
        final_string += "Misuse of constructor" + "\n" + print_constructor + "\n"
      }
      if (!print_object_conver.isEmpty) {
        final_string += "Type casting to arbitrary contracts" + "\n" + print_object_conver + "\n"
      }
      if (!print_depre.isEmpty) {
        final_string += "Usage of Deprecated API" + "\n" + print_depre + "\n"
      }

      writer.write("Analyzed : " + f + "\n\n" + "Detail Information" + "\n\n" + final_string)
      writer.flush()
      writer.close()
      Success(())
  }

  def defaultConfig: RunTestConfig = RunTestConfig()
  val options: List[PhaseOption[RunTestConfig]] = Nil
}

// Parse phase config
case class RunTestConfig() extends Config
