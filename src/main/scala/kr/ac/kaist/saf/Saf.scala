/*
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

//import java.io._
import scala.collection.immutable.HashMap
import scala.util.{ Try, Success, Failure }
import kr.ac.kaist.saf.errors.SafException
import kr.ac.kaist.saf.errors.error.{ NoCmdError, NoInputError }
import kr.ac.kaist.saf.phase._
import kr.ac.kaist.saf.util._

object Saf {
  ////////////////////////////////////////////////////////////////////////////////
  // Main entry point
  ////////////////////////////////////////////////////////////////////////////////

  def main(tokens: Array[String]): Unit = {
    (tokens.toList match {
      case str :: args => cmdMap.get(str) match {
        case Some(cmd) => cmd(args, false)
        case None => Failure(NoCmdError(str))
      }
      case Nil => Failure(NoInputError)
    }) recover {
      // SafException: print the usage message.
      case ex: SafException =>
        Console.err.println(ex.getMessage)
      // Unexpected: print the stack trace.
      case ex =>
        Console.err.println("* Unexpected error occurred.")
        Console.err.println(ex.toString)
        Console.err.println(ex.getStackTrace.mkString(LINE_SEP))
    }
  }

  def apply[Result](
    command: CommandObj[Result],
    runner: SafConfig => Try[Result],
    config: SafConfig
  ): Try[Result] = {
    // set the start time.
    val startTime = System.currentTimeMillis

    // execute the command.
    val result: Try[Result] = runner(config)

    if (!config.silent) {
      result.map(res => {
        // display the result.
        command.display(res)
      })

      // display the time.
      val duration = System.currentTimeMillis - startTime
      val name = config.command.name
      println(s"The command '$name' took $duration ms.")
    }

    // return result
    result
  }

  // commands
  val commands: List[Command] = List(
    CmdParse,
    CmdClone,
    CmdAstCheck,
    CmdInhCheck,
    CmdRunTest,
    CmdHelp
  )
  val cmdMap = commands.foldLeft[Map[String, Command]](HashMap()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }

  // phases
  var phases: List[Phase] = List(
    Parse,
    Clone,
    AstCheck,
    InhCheck,
    RunTest,
    Help
  )

  // global options
  val options: List[PhaseOption[SafConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "all messages are muted."),
    ("testMode", BoolOption(c => c.testMode = true),
      "switch on the test mode.")
  )

  // indentation 
  private val INDENT = 15

  // print usage message.
  val usage: String = {
    val s: StringBuilder = new StringBuilder
    val prefix = " " * (INDENT + 4)
    s.append("Usage:").append(LINE_SEP)
      .append("  saf {command} [-{option}]* [-{phase}:{option}[={input}]]* {filename}+").append(LINE_SEP)
      .append("  example: saf parse -silent -parser:out=out test.sol").append(LINE_SEP)
      .append(LINE_SEP)
      .append("* command list:").append(LINE_SEP)
      .append("    Each command consists of the following phases.").append(LINE_SEP)
      .append("    format: {command} {phase} [>> {phase}]*").append(LINE_SEP).append(LINE_SEP)
    commands foreach (cmd => s.append(s"    %-${INDENT}s".format(cmd.name)).append(cmd).append(LINE_SEP))
    s.append(LINE_SEP)
      .append("* phase list:").append(LINE_SEP)
      .append("    Each phase has the following options.").append(LINE_SEP)
      .append("    format: {phase} [-{phase}:{option}[={input}]]*").append(LINE_SEP).append(LINE_SEP)
    phases foreach (phase => {
      s.append(s"    %-${INDENT}s".format(phase.name))
      val names = phase.getOptShapes
      val len = names.length
      s.append(names.mkString(LINE_SEP + prefix))
      s.append(LINE_SEP)
    })
    s.append(LINE_SEP)
      .append("* global options:")
      .append(options.map { case (opt, kind, _) => s"-${opt}${kind.postfix}" }
        .mkString(" " * 3, LINE_SEP + prefix, LINE_SEP))
    s.toString
  }

  // print help message.
  val help: String = {
    val s: StringBuilder = new StringBuilder
    s.append("Invoked as script: saf args").append(LINE_SEP)
      .append("Invoked by java: java ... kr.ac.kaist.saf.Saf args").append(LINE_SEP)
      .append(LINE_SEP)
      .append("* command list:").append(LINE_SEP)
      .append("    Each command consists of following phases.").append(LINE_SEP)
      .append("    format: {command} {phase} [>> {phase}]*").append(LINE_SEP).append(LINE_SEP)
    commands foreach (cmd => {
      s.append(s"    %-${INDENT}s".format(cmd.name))
        .append(cmd.toString.replace(LINE_SEP, LINE_SEP + "    " + " " * INDENT))
        .append(LINE_SEP)
    })
    s.append(LINE_SEP)
      .append("* phase list:").append(LINE_SEP)
      .append("    Each phase has following options.").append(LINE_SEP)
      .append("    format: {phase} [-{phase}:{option}[={input}]]*").append(LINE_SEP).append(LINE_SEP)
    phases foreach (phase => {
      s.append(s"    %-${INDENT}s".format(phase.name))
      Useful.indentation(s, phase.help, INDENT + 4)
      s.append(LINE_SEP)
        .append(LINE_SEP)
      phase.getOptDescs foreach {
        case (name, desc) =>
          s.append(s"    If $name is given, $desc").append(LINE_SEP)
      }
      s.append(LINE_SEP)
    })
    s.append("* global option:").append(LINE_SEP).append(LINE_SEP)
    options.foreach {
      case (opt, kind, desc) =>
        val name = s"-${opt}${kind.postfix}"
        s.append(s"    If $name is given, $desc").append(LINE_SEP)
    }
    s.toString
  }
}

case class SafConfig(
  var command: Command,
  var fileNames: List[String] = Nil,
  var silent: Boolean = false,
  var testMode: Boolean = false
) extends Config
