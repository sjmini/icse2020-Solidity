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

import scala.util.{ Try, Success }
import kr.ac.kaist.saf.{ Saf, SafConfig }

// Help phase
case object Help extends PhaseObj[Unit, HelpConfig, Unit] {
  val name = "helper"
  val help = ""

  def apply(
    unit: Unit,
    safConfig: SafConfig,
    config: HelpConfig
  ): Try[Unit] = Success(println(Saf.help))
  def defaultConfig: HelpConfig = HelpConfig()
  val options: List[PhaseOption[HelpConfig]] = Nil
}

case class HelpConfig() extends Config
