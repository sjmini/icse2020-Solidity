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

package kr.ac.kaist.saf.errors

import kr.ac.kaist.saf.errors.error.SafError
import kr.ac.kaist.saf.LINE_SEP

class ExcLog(es: List[SafError] = Nil) {
  private var errs: List[SafError] = es
  override def toString: String = {
    (errs.length match {
      case 0 => "No error"
      case l =>
        l + " error" + (if (l > 1) "s" else "") + ":" + LINE_SEP +
          "    " + errs.reverse.mkString(LINE_SEP + "    ")
    })
  }
  def hasError: Boolean = !errs.isEmpty
  def signal(err: SafError): Unit = errs ::= err
  def +(other: ExcLog): ExcLog = new ExcLog(other.errs ++ errs)
}
