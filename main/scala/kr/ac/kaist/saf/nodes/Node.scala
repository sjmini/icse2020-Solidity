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

package kr.ac.kaist.saf.nodes

import kr.ac.kaist.saf.util.{ Span, SourceLoc, Useful }

abstract class Node {
  def toString(indent: Int): String

  // helper for info
  def span: Span
  def fileName: String
  def relFileName: String = Useful.toRelativePath(fileName)
  def begin: SourceLoc
  def end: SourceLoc
  def line: Int
  def offset: Int
}
