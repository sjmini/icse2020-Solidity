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

package kr.ac.kaist.saf.util

sealed abstract class FileKind
object FileKind {
  def apply(fileName: String): FileKind = {
    if (fileName.endsWith(".sol")) SOLFile
    else if (fileName.endsWith(".sol.err")) SOLErrFile
    else if (fileName.endsWith(".sol.todo")) SOLTodoFile
    else NormalFile
  }
}

case object SOLFile extends FileKind
case object SOLErrFile extends FileKind
case object SOLTodoFile extends FileKind
case object NormalFile extends FileKind
