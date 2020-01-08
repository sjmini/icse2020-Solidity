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

import scala.util.Try
import kr.ac.kaist.saf.util.OptionKind

package object phase {
  type Regex = scala.util.matching.Regex
  type ArgRegex[PhaseConfig <: Config] = (Regex, Regex, (PhaseConfig, String) => Try[Unit])
  type PhaseOption[PhaseConfig <: Config] = (String, OptionKind[PhaseConfig], String)
}
