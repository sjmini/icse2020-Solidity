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

package kr.ac.kaist.saf.nodes.ast

import kr.ac.kaist.saf.nodes.Node
import kr.ac.kaist.saf.util.{ SourceLoc, Span }

/**
 * *************************
 * Solidity AST
 * *************************
 */

// AST Node
trait ASTNode extends Node {
  val info: ASTNodeInfo
  def span: Span = info.span
  def fileName: String = span.fileName
  def begin: SourceLoc = span.begin
  def end: SourceLoc = span.end
  def line: Int = begin.line
  def offset: Int = begin.offset
  override def toString(indent: Int): String = ""
}

// AST Node Information
case class ASTNodeInfo(
  span: Span
)
