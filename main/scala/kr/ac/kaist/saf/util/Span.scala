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

import java.io.File
import java.io.IOException
import java.io.Serializable
import java.lang.StringBuilder
import xtc.tree.Location
import kr.ac.kaist.saf.nodes.Node

case class Span(
    fileName: String = "defaultSpan",
    begin: SourceLoc = SourceLoc(),
    end: SourceLoc = SourceLoc()
) {
  def addLines(line: Int, offset: Int): Span =
    Span(fileName, begin.addLines(line, offset), end.addLines(line, offset))

  override def toString: String =
    appendToStr(new StringBuilder).toString

  def toStringWithoutFiles: String =
    appendToStr(new StringBuilder, false).toString

  private def appendToStr(w: StringBuilder, doFiles: Boolean = true): String = {
    if (doFiles) {
      // TODO Need to add escapes to the file name
      w.append(Useful.toRelativePath(fileName))
      w.append(":")
    }
    w.append(begin.toString)
    begin.line == end.line match {
      case true => begin.column == end.column match {
        case true =>
        case false => w.append("-").append(end.column)
      }
      case false => w.append("-").append(end.toString)
    }
    w.toString
  }

  // constructor
  def this(
    fileName: String,
    startLine: Int,
    endLine: Int,
    startC: Int,
    endC: Int,
    startOffset: Int,
    endOffset: Int
  ) = this(
    fileName,
    SourceLoc(startLine, startC, startOffset),
    SourceLoc(endLine, endC, endOffset)
  )

  def +(o: Span): Span = fileName == o.fileName match {
    case true => Span(fileName, begin, o.end)
    case false => Span(NodeUtil.MERGED_FILE_NAME)
  }
}

object Span {
  def merge(left: Span, right: Span): Span = left + right

  def merge(nodes: List[Node], default: Span): Span = nodes match {
    case Nil => default
    case first :: _ => first.span + nodes.last.span
  }
}
