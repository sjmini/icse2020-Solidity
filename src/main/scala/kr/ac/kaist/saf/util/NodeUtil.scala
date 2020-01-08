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

import kr.ac.kaist.saf.nodes.Node
import kr.ac.kaist.saf.nodes.ast._
import kr.ac.kaist.saf.BASE_DIR
import kr.ac.kaist.saf.LINE_SEP
import java.io.BufferedWriter
import java.io.IOException
import scala.collection.immutable.{ HashMap, HashSet }

object NodeUtil {
  ////////////////////////////////////////////////////////////////
  // local mutable (TODO have to handle)
  ////////////////////////////////////////////////////////////////

  var iid = 0
  var nodesPrintId = 0
  var nodesPrintIdEnv: Map[String, String] = HashMap()

  val INTERNAL_SYMBOL = "<>"
  val GLOBAL_PREFIX = "<>Global<>"
  val GENERATED_STR = "<>generated String Literal"

  val MERGED_FILE_NAME = freshFile("Merged")

  val PRINT_WIDTH = 50

  // Names ///////////////////////////////////////////////////////
  // unique name generation
  def getIId: Int = { iid += 1; iid }
  def freshName(n: String): String =
    INTERNAL_SYMBOL + n + INTERNAL_SYMBOL + "%013d".format(getIId)
  // unique name generation for global names
  def freshGlobalName(n: String): String = GLOBAL_PREFIX + n
  def funexprName(span: Span): String = freshName("funexpr@" + span.toStringWithoutFiles)

  ////////////////////////////////////////////////////////////////
  // For all AST, IR, and CFG
  ////////////////////////////////////////////////////////////////
  def isInternal(s: String): Boolean = s.containsSlice(INTERNAL_SYMBOL)
  def isGlobalName(s: String): Boolean = s.startsWith(GLOBAL_PREFIX)
  def isFunExprName(name: String): Boolean = name.containsSlice("<>funexpr")

  def convertUnicode(s: String): String =
    s.replace("\\u0041", "A")
      .replace("\\u0042", "B")
      .replace("\\u0043", "C")
      .replace("\\u0044", "D")
      .replace("\\u0045", "E")
      .replace("\\u0046", "F")
      .replace("\\u0047", "G")
      .replace("\\u0048", "H")
      .replace("\\u0049", "I")
      .replace("\\u004a", "J")
      .replace("\\u004b", "K")
      .replace("\\u004c", "L")
      .replace("\\u004d", "M")
      .replace("\\u004e", "N")
      .replace("\\u004f", "O")
      .replace("\\u0050", "P")
      .replace("\\u0051", "Q")
      .replace("\\u0052", "R")
      .replace("\\u0053", "S")
      .replace("\\u0054", "T")
      .replace("\\u0055", "U")
      .replace("\\u0056", "V")
      .replace("\\u0057", "W")
      .replace("\\u0058", "X")
      .replace("\\u0059", "Y")
      .replace("\\u005a", "Z")
      .replace("\\u0061", "a")
      .replace("\\u0062", "b")
      .replace("\\u0063", "c")
      .replace("\\u0064", "d")
      .replace("\\u0065", "e")
      .replace("\\u0066", "f")
      .replace("\\u0067", "g")
      .replace("\\u0068", "h")
      .replace("\\u0069", "i")
      .replace("\\u006a", "j")
      .replace("\\u006b", "k")
      .replace("\\u006c", "l")
      .replace("\\u006d", "m")
      .replace("\\u006e", "n")
      .replace("\\u006f", "o")
      .replace("\\u0070", "p")
      .replace("\\u0071", "q")
      .replace("\\u0072", "r")
      .replace("\\u0073", "s")
      .replace("\\u0074", "t")
      .replace("\\u0075", "u")
      .replace("\\u0076", "v")
      .replace("\\u0077", "w")
      .replace("\\u0078", "x")
      .replace("\\u0079", "y")
      .replace("\\u007a", "z")
      .replace("\\u0030", "0")
      .replace("\\u0031", "1")
      .replace("\\u0032", "2")
      .replace("\\u0033", "3")
      .replace("\\u0034", "4")
      .replace("\\u0035", "5")
      .replace("\\u0036", "6")
      .replace("\\u0037", "7")
      .replace("\\u0038", "8")
      .replace("\\u0039", "9")
      .replace("\\u005f", "_")

  // Defaults ////////////////////////////////////////////////////
  // dummy file name for source location information
  def freshFile(f: String): String = INTERNAL_SYMBOL + f

  def initNodesPrint: Unit = {
    nodesPrintId = 0
    nodesPrintIdEnv = HashMap()
  }

  def getNodesE(uniq: String): String = nodesPrintIdEnv.get(uniq) match {
    case Some(newUniq) => newUniq
    case None =>
      val newUniq: String = { nodesPrintId += 1; nodesPrintId.toString }
      nodesPrintIdEnv += (uniq -> newUniq)
      newUniq
  }

  def ppAST(s: StringBuilder, str: String): Unit =
    s.append(str.foldLeft("")((res, c) => c match {
      case '\u0008' => res + '\b'
      case '\t' => res + '\t'
      case '\n' => res + '\n'
      case '\f' => res + '\f'
      case '\r' => res + '\r'
      case '\u000b' => res + '\u000b'
      case '"' => res + '"'
      case '\'' => res + "'"
      case '\\' => res + '\\'
      case c => res + c
    }))

  def pp(str: String): String =
    str.foldLeft("")((res, c) => c match {
      case '\u0008' => res + "\\b"
      case '\t' => res + "\\t"
      case '\n' => res + "\\n"
      case '\f' => res + "\\f"
      case '\r' => res + "\\r"
      case '\u000b' => res + "\\v"
      case '"' => res + "\\\""
      case '\'' => res + "'"
      case '\\' => res + "\\"
      case c => res + c
    })

  def getIndent(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    for (i <- 0 to indent - 1) s.append("  ")
    s.toString
  }
  def join(indent: Int, all: List[Node], sep: String, result: StringBuilder): StringBuilder = all match {
    case Nil => result
    case _ => result.length match {
      case 0 => {
        join(indent, all.tail, sep, result.append(all.head.toString(indent)))
      }
      case _ =>
        if (result.length > PRINT_WIDTH && sep.equals(", "))
          join(indent, all.tail, sep, result.append(", " + LINE_SEP + getIndent(indent)).append(all.head.toString(indent)))
        else
          join(indent, all.tail, sep, result.append(sep).append(all.head.toString(indent)))
    }
  }

  ////////////////////////////////////////////////////////////////
  // AST
  ////////////////////////////////////////////////////////////////
  def makeASTNodeInfo(span: Span): ASTNodeInfo = new ASTNodeInfo(span)

  def escape(s: String): String = s.replaceAll("\\\\", "\\\\\\\\")
  def unescape(s: String): String = s.replaceAll("\\\\", "")

  def lineTerminating(c: Char): Boolean =
    List('\u000a', '\u2028', '\u2029', '\u000d').contains(c)
}
