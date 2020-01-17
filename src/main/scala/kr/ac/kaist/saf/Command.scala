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

import kr.ac.kaist.saf.errors.error.NoMode
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.phase._
import kr.ac.kaist.saf.util.ArgParser

import scala.collection.immutable.HashMap
import scala.util.Try

sealed trait Command {
  val name: String
  def apply(args: List[String], testMode: Boolean): Try[Any]
}

class CommandObj[Result](
    override val name: String,
    pList: PhaseList[Result],
    modeMap: Map[String, PhaseList[Result]] = HashMap[String, PhaseList[Result]]()
) extends Command {
  def apply(
    args: List[String],
    testMode: Boolean = false
  ): Try[Result] = {
    val safConfig = SafConfig(this, testMode = testMode)
    val parser = new ArgParser(this, safConfig)
    val modePattern = "--(.+)".r
    (args match {
      case modePattern(mode) :: remain =>
        println(mode); modeMap.get(mode) match {
          case Some(pl) => (pl, remain)
          case None => throw NoMode(name, mode)
        }
      case _ => (pList, args)
    }) match {
      case (pList, args) => pList.getRunner(parser).flatMap {
        case runner => parser(args).flatMap {
          case _ => Saf(this, runner(_), safConfig)
        }
      }
    }
  }

  def display(res: Result): Unit = ()

  override def toString: String = modeMap.foldLeft(pList.toString) {
    case (str, (mode, pList)) => s"$str$LINE_SEP--$mode: " + pList.toString
  }

  def >>[C <: Config, R](phase: PhaseObj[Result, C, R]): PhaseList[R] = pList >> phase
}

// base command
case object CmdBase extends CommandObj("", PhaseNil)

// parse
case object CmdParse extends CommandObj("parse", CmdBase >> Parse) {
  override def display(su: SourceUnit): Unit = println(su.toString(0))
}

// clone
case object CmdClone extends CommandObj("clone", CmdParse >> Clone)

// astCheck
case object CmdAstCheck extends CommandObj("astCheck", CmdParse >> AstCheck)
//case object CmdAstCheck extends CommandObj("astCheck", CmdBase >> AstCheck)

// inhCheck
case object CmdInhCheck extends CommandObj("inhCheck", CmdBase >> InhCheck)

// runTest
case object CmdRunTest extends CommandObj("runTest", CmdParse >> RunTest)

// help
case object CmdHelp extends CommandObj("help", CmdBase >> Help)
