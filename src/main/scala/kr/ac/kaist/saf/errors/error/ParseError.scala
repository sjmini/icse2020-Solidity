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

package kr.ac.kaist.saf.errors.error

import kr.ac.kaist.saf.util.{ Span, Useful }

sealed abstract class ParseError(msg: String, spanOpt: Option[Span]) extends SafError(spanOpt match {
  case Some(span) =>
    val loc = span.toString
    s"$loc: $msg"
  case None =>
    s"$msg"
})

case class NoFileError(cmd: String) extends ParseError({
  s"Need a file to $cmd."
}, None)

case class NotSOLFileError(fileName: String) extends ParseError({
  val relFileName = Useful.toRelativePath(fileName)
  s"Need a Solidity file instead of '$relFileName'."
}, None)

case class ParserError(msg: String, span: Span) extends ParseError(msg, Some(span))

case class NoNumeralError(msg: String) extends ParseError({
  s"Expected a numeral but got $msg."
}, None)

case class NumeralPrefixZeroError() extends ParseError({
  "A numeral should not begin with 0."
}, None)
