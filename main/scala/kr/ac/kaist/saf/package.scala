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

package kr.ac.kaist

import java.io.File
import kr.ac.kaist.saf.util.{ NodeUtil => NU }

package object saf {
  // Line seperator
  val LINE_SEP = System.getProperty("line.separator")

  // Path seperator
  val SEP = File.separator

  // Number of significant bits
  val SIGNIFICANT_BITS = 13

  // Maximum length of printable instruction of CFGBlock
  val MAX_INST_PRINT_SIZE = 10000

  // Base project directory root
  val BASE_DIR = System.getenv("SAF_HOME")

  // Base project directory root
  val CUR_DIR = System.getProperty("user.dir")
}
