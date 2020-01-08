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

package kr.ac.kaist.saf.phase

import java.io._
import kr.ac.kaist.saf.ast_checker.INHChecker
import kr.ac.kaist.saf.ast_checker.ASTChecker
import kr.ac.kaist.saf.errors.error.NoFileError
import kr.ac.kaist.saf.nodes.ast.SourceUnit
import kr.ac.kaist.saf.parser.Parser
import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.{ LINE_SEP, SafConfig }
import scala.collection.mutable._
import scala.util.{ Try, Success, Failure }

// InhCheck phase
case object InhCheck extends PhaseObj[Unit, InhCheckConfig, Unit] {
  val name = "inhchecker"
  val help = "InhChecker." + LINE_SEP +
    "It produces useful information in the result file after parsing."

  def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { file =>
      extensions.exists(file.getName.endsWith(_))
    }
  }

  var usp = 0
  var pfwmsvm_n = 0

  def apply(
    unit: Unit,
    safConfig: SafConfig,
    config: InhCheckConfig
  ): Try[Unit] = safConfig.fileNames match {
    case Nil => Failure(NoFileError("inhcheck"))
    case List(f) =>
      val extension = List("sol")

      //HSJ
      val files = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/merge_source_destruct_remove"), extension)

      val files_throw = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/413"), extension)
      val files_sucide = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/417"), extension)
      val files_block = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/422"), extension)
      val files_callcode = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/412"), extension)
      val files_sha3 = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/417"), extension)
      val files_gas = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/deprecated/421"), extension)
      val files_order = getListOfFiles(new File("/Users/sungjaehwang/blockchain/emperical/data/ordering_source"), extension)

      val writer_type_address = new PrintWriter(new FileOutputStream(new File("type_result_address.txt"), false))
      val writer_type_object = new PrintWriter(new FileOutputStream(new File("type_result_object.txt"), false))
      val writer_type_error = new PrintWriter(new FileOutputStream(new File("type_result_error.txt"), false))
      val writer_constructor = new PrintWriter(new FileOutputStream(new File("result_constructor.txt"), false))
      val writer_shadow = new PrintWriter(new FileOutputStream(new File("result_shadow.txt"), false))
      val writer_unary = new PrintWriter(new FileOutputStream(new File("result_unary.txt"), false))

      val writer_throw = new PrintWriter(new FileOutputStream(new File("result_throw.txt"), false))
      val writer_sucide = new PrintWriter(new FileOutputStream(new File("result_sucide.txt"), false))
      val writer_block = new PrintWriter(new FileOutputStream(new File("result_block.txt"), false))
      val writer_callcode = new PrintWriter(new FileOutputStream(new File("result_callcode.txt"), false))
      val writer_sha3 = new PrintWriter(new FileOutputStream(new File("result_sha3.txt"), false))
      val writer_gas = new PrintWriter(new FileOutputStream(new File("result_gas.txt"), false))

      val writer_order = new PrintWriter(new FileOutputStream(new File("result_order.txt"), false))

      
      files_throw.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_throw, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 2)

            if (!print_throw.isEmpty) writer_throw.write(filename + "\n" + print_throw)

          case Failure(e) => // TODO exception handling
        }
      })

      files_sucide.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_sucide, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 3)

            if (!print_sucide.isEmpty) writer_sucide.write(filename + "\n" + print_sucide)

          case Failure(e) => // TODO exception handling
        }
      })

      files_block.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_block, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 4)

            if (!print_block.isEmpty) writer_block.write(filename + "\n" + print_block)

          case Failure(e) => // TODO exception handling
        }
      })

      files_callcode.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_callcode, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 5)

            if (!print_callcode.isEmpty) writer_callcode.write(filename + "\n" + print_callcode)

          case Failure(e) => // TODO exception handling
        }
      })

      files_sha3.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_sha3, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 6)

            if (!print_sha3.isEmpty) writer_sha3.write(filename + "\n" + print_sha3)

          case Failure(e) => // TODO exception handling
        }
      })

      files_gas.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_gas, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 7)

            if (!print_gas.isEmpty) writer_gas.write(filename + "\n" + print_gas)

          case Failure(e) => // TODO exception handling
        }
      })

      files_order.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_order, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 8)

            if (!print_order.isEmpty) writer_order.write(filename + "\n" + print_order)

          case Failure(e) => // TODO exception handling
        }
      })

      files.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_depre, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 1)

            if (!print_unary.isEmpty) writer_unary.write(filename + "\n" + print_unary)

            if (!print_shadow.isEmpty) writer_shadow.write(filename + "\n" + print_shadow)

            if (!print_constructor.isEmpty) writer_constructor.write(filename + "\n" + print_constructor)
            if (!print_address_conver.isEmpty) writer_type_address.write(filename + "\n" + print_address_conver)
            if (!print_object_conver.isEmpty) writer_type_object.write(filename + "\n" + print_object_conver)
            if (!print_error_conver.isEmpty) writer_type_error.write(filename + "\n" + print_error_conver)

            Try(ASTChecker.run(su) match {
              case (s: (String, String, Int, Int)) =>
                val writer = new PrintWriter(new FileOutputStream(new File("result_usp.txt"), false))
                val writer2 = new PrintWriter(new FileOutputStream(new File("result_fv.txt"), false))
                if (s._1 != "") {
                  usp = usp + s._3
                  writer.write(filename + "\n\n" + s._1 + "USP: " + usp.toString + "\n\n")
                }
                if (s._2 != "") {
                  pfwmsvm_n = pfwmsvm_n + s._4
                  writer2.write(filename + "\n\n" + s._2 + "PFWMSVM: " + pfwmsvm_n.toString + "\n\n")
                }
                writer.close()
                writer2.close()
                Success(())
              case _ => Failure(new Error())
            }) recover {
              case _ =>
                Success(())
            }

          case Failure(e) => // TODO exception handling
        }
      })

      writer_order.flush()
      writer_order.close()

      writer_throw.flush()
      writer_throw.close()

      writer_sucide.flush()
      writer_sucide.close()

      writer_block.flush()
      writer_block.close()

      writer_callcode.flush()
      writer_callcode.close()

      writer_sha3.flush()
      writer_sha3.close()

      writer_gas.flush()
      writer_gas.close()

      writer_unary.flush()
      writer_unary.close()

      writer_shadow.flush()
      writer_shadow.close()

      writer_constructor.flush()
      writer_constructor.close()

      writer_type_address.flush()
      writer_type_address.close()

      writer_type_object.flush()
      writer_type_object.close()

      writer_type_error.flush()
      writer_type_error.close()

      Success(())
  }

  def defaultConfig: InhCheckConfig = InhCheckConfig()
  val options: List[PhaseOption[InhCheckConfig]] = Nil
}

// Parse phase config
case class InhCheckConfig() extends Config
