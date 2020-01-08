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

package kr.ac.kaist.saf.parser

import java.io._
import java.nio.charset.Charset
import scala.util.{ Try, Success, Failure }
import xtc.parser.{ Result, ParseError, SemanticValue }
import kr.ac.kaist.saf.errors.ExcLog
import kr.ac.kaist.saf.errors.error.{ ParserError, NotSOLFileError }
import kr.ac.kaist.saf.nodes.ast._
import kr.ac.kaist.saf.util.{ NodeUtil => NU, _ }

object Parser {
  // Used by CoreTest
  def stringToAST(str: String): Try[(SourceUnit, ExcLog)] = {
    val sr = new StringReader(str)
    val in = new BufferedReader(sr)
    val result = resultToAST[SourceUnit](new SOL(in, "stringParse"), _.SOLmain(0))
    in.close; sr.close
    result
  }

  // Used by phase/Parse.scala
  def fileToAST(fs: List[String]): Try[(SourceUnit, ExcLog)] = fs match {
    case List(f) =>
      var fileName = new File(f).getCanonicalPath
      if (File.separatorChar == '\\') {
        // convert path string to linux style for windows
        fileName = fileName.charAt(0).toLower + fileName.replace('\\', '/').substring(1)
      }
      FileKind(fileName) match {
        case SOLFile | SOLErrFile => {
          val fs = new FileInputStream(new File(f))
          val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
          val in = new BufferedReader(sr)
          val pair = resultToAST[SourceUnit](new SOL(in, fileName), _.SOLmain(0))
          in.close; sr.close; fs.close
          pair
        }
        case SOLTodoFile | NormalFile => Failure(NotSOLFileError(fileName))
      }

    case files => Failure(new Error()) // TODO more exact error type
  }

  private def resultToAST[T <: ASTNode](
    parser: SOL,
    doit: SOL => Result
  ): Try[(T, ExcLog)] = {
    doit(parser) match {
      case (result: ParseError) =>
        val span = decodeSpan(parser.format(result))
        Failure(ParserError(result.msg, span))
      case (semV: SemanticValue) => Try((semV.value.asInstanceOf[T], parser.excLog))
      case _ => Failure(new Error()) // TODO more exact error type
    }
  }

  // xtc.parser.ParserBase
  // public final String format(ParseError error) throws IOException
  private def decodeSpan(formatted: String): Span = {
    val array = formatted.split(":")
    val (file, line, column) = (array(0), array(1).toInt, array(2).toInt)
    val loc = new SourceLoc(line, column, 0)
    new Span(file, loc, loc)
  }
}
