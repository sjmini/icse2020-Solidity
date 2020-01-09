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

  var throw_file = 0
  var sucide_file = 0
  var block_file = 0
  var callcode_file = 0
  var sha3_file = 0
  var gas_file = 0
  var order_file = 0
  var typeObject_file = 0
  var typeError_file = 0
  var typeAddr_file = 0
  var constructor_file = 0
  var shadow_file = 0
  var unary_file = 0
  var usp_file = 0
  var fv_file = 0

  var throw_final_string = ""
  var sucide_final_string = ""
  var block_final_string = ""
  var callcode_final_string = ""
  var sha3_final_string = ""
  var gas_final_string = ""
  var order_final_string = ""
  var typeObject_final_string = ""
  var typeError_final_string = ""
  var typeAddr_final_string = ""
  var constructor_final_string = ""
  var shadow_final_string = ""
  var unary_final_string = ""
  var usp_final_string = ""
  var fv_final_string = ""

  def apply(
    unit: Unit,
    safConfig: SafConfig,
    config: InhCheckConfig
  ): Try[Unit] = safConfig.fileNames match {
    case Nil => Failure(NoFileError("inhcheck"))
    case List(f) =>
      val extension = List("sol")

      //HSJ
      val files = getListOfFiles(new File("test/live"), extension)

      val files_throw = getListOfFiles(new File("test/413"), extension)

      val files_sucide = getListOfFiles(new File("test/417"), extension)
      val files_block = getListOfFiles(new File("test/422"), extension)
      val files_callcode = getListOfFiles(new File("test/412"), extension)
      val files_sha3 = getListOfFiles(new File("test/417"), extension)
      val files_gas = getListOfFiles(new File("test/421"), extension)
      val files_order = getListOfFiles(new File("test/ordering_source"), extension)

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

      val writer_usp = new PrintWriter(new FileOutputStream(new File("result_usp.txt"), false))
      val writer_fv = new PrintWriter(new FileOutputStream(new File("result_fv.txt"), false))

      files_throw.foreach(file => {
        val filename = file.getAbsolutePath
        println(s"Analyzing filename: $filename")
        Parser.fileToAST(List(filename)) match {
          case Success((su, _)) =>
            val walker = new INHChecker()
            val (print_throw, print_unary, print_shadow, print_constructor, print_address_conver, print_object_conver, print_error_conver) = walker.run(su, 2)

            if (!print_throw.isEmpty) {
              throw_final_string += filename + "\n" + print_throw + "\n"
              throw_file += 1
            }

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

            if (!print_sucide.isEmpty) {
              sucide_final_string += filename + "\n" + print_sucide + "\n"
              sucide_file += 1
            }
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

            if (!print_block.isEmpty) {
              block_final_string += filename + "\n" + print_block + "\n"
              block_file += 1
            }

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

            if (!print_callcode.isEmpty) {
              callcode_final_string += filename + "\n" + print_callcode + "\n"
              callcode_file += 1
            }

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

            if (!print_sha3.isEmpty) {
              sha3_final_string += filename + "\n" + print_sha3 + "\n"
              sha3_file += 1
            }

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

            if (!print_gas.isEmpty) {
              gas_final_string += filename + "\n" + print_gas + "\n"
              gas_file += 1
            }

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

            if (!print_order.isEmpty) {
              order_final_string += filename + "\n" + print_order + "\n"
              order_file += 1
            }

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

            if (!print_unary.isEmpty) {
              unary_final_string += filename + "\n" + print_unary + "\n"
              unary_file += 1
            }

            if (!print_shadow.isEmpty) {
              shadow_final_string += filename + "\n" + print_shadow + "\n"
              shadow_file += 1
            }

            if (!print_constructor.isEmpty) {
              constructor_final_string += filename + "\n" + print_constructor + "\n"
              constructor_file += 1
            }

            if (!print_address_conver.isEmpty) {
              typeAddr_final_string += filename + "\n" + print_address_conver + "\n"
              typeAddr_file += 1
            }

            if (!print_object_conver.isEmpty) {
              typeObject_final_string += filename + "\n" + print_object_conver + "\n"
              typeObject_file += 1
            }

            if (!print_error_conver.isEmpty) {
              typeError_final_string += filename + "\n" + print_error_conver + "\n"
              typeError_file += 1
            }

            Try(ASTChecker.run(su) match {
              case (s: (String, String, Int, Int)) =>
                if (s._1 != "") {
                  usp = usp + s._3
                  usp_final_string += filename + "\n\n" + s._1 + "USP: " + usp.toString + "\n\n"
                  usp_file += 1
                }
                if (s._2 != "") {
                  pfwmsvm_n = pfwmsvm_n + s._4
                  fv_final_string += filename + "\n\n" + s._2 + "PFWMSVM: " + pfwmsvm_n.toString + "\n\n"
                  fv_file += 1
                }
                Success(())
              case _ => Failure(new Error())
            }) recover {
              case _ =>
                Success(())
            }

          case Failure(e) => // TODO exception handling
        }
      })

      writer_fv.write("Number of Vulnerable Contracts (Functions without visibility): " + fv_file + "\n\n" + "Detail Information" + "\n\n" + fv_final_string)
      writer_fv.flush()
      writer_fv.close()

      writer_usp.write("Number of Vulnerable Contracts (Uninitialized Storage Pointer): " + usp_file + "\n\n" + "Detail Information" + "\n\n" + usp_final_string)
      writer_usp.flush()
      writer_usp.close()

      writer_order.write("Number of Vulnerable Contracts (Inheritance Order Confusion): " + order_file + "\n\n" + "Detail Information" + "\n\n" + order_final_string)
      writer_order.flush()
      writer_order.close()

      writer_throw.write("Number of Vulnerable Contracts: (Deprecated Throw): " + throw_file + "\n\n" + "Detail Information" + "\n\n" + throw_final_string)
      writer_throw.flush()
      writer_throw.close()

      writer_sucide.write("Number of Vulnerable Contracts: (Deprecated Sucide): " + sucide_file + "\n\n" + "Detail Information" + "\n\n" + sucide_final_string)
      writer_sucide.flush()
      writer_sucide.close()

      writer_block.write("Number of Vulnerable Contracts: (Deprecated block.blockhash): " + block_file + "\n\n" + "Detail Information" + "\n\n" + block_final_string)
      writer_block.flush()
      writer_block.close()

      writer_callcode.write("Number of Vulnerable Contracts: (Deprecated Callcode): " + callcode_file + "\n\n" + "Detail Information" + "\n\n" + callcode_final_string)
      writer_callcode.flush()
      writer_callcode.close()

      writer_sha3.write("Number of Vulnerable Contracts: (Deprecated Sha3): " + sha3_file + "\n\n" + "Detail Information" + "\n\n" + sha3_final_string)
      writer_sha3.flush()
      writer_sha3.close()

      writer_gas.write("Number of Vulnerable Contracts: (Deprecated msg.gas): " + gas_file + "\n\n" + "Detail Information" + "\n\n" + gas_final_string)
      writer_gas.flush()
      writer_gas.close()

      writer_unary.write("Number of Vulnerable Contracts: (Typo of the += operator): " + unary_file + "\n\n" + "Detail Information" + "\n\n" + unary_final_string)
      writer_unary.flush()
      writer_unary.close()

      writer_shadow.write("Number of Vulnerable Contracts: (Storage variable shadowing confusion): " + shadow_file + "\n\n" + "Detail Information" + "\n\n" + shadow_final_string)
      writer_shadow.flush()
      writer_shadow.close()

      writer_constructor.write("Number of Vulnerable Contracts: (Misuse of constructors): " + constructor_file + "\n\n" + "Detail Information" + "\n\n" + constructor_final_string)
      writer_constructor.flush()
      writer_constructor.close()

      writer_type_address.write("Number of Vulnerable Contracts: (Type casting to arbitrary contracts with address type): " + typeAddr_file + "\n\n" + "Detail Information" + "\n\n" + typeAddr_final_string)
      writer_type_address.flush()
      writer_type_address.close()

      writer_type_object.write("Number of Vulnerable Contracts: (Type casting to arbitrary contracts without address type): " + typeObject_file + "\n\n" + "Detail Information" + "\n\n" + typeObject_final_string)
      writer_type_object.flush()
      writer_type_object.close()

      writer_type_error.write("Number of Vulnerable Contracts: (Type casting to arbitrary contracts with error): " + typeError_file + "\n\n" + "Detail Information" + "\n\n" + typeError_final_string)
      writer_type_error.flush()
      writer_type_error.close()

      Success(())
  }

  def defaultConfig: InhCheckConfig = InhCheckConfig()
  val options: List[PhaseOption[InhCheckConfig]] = Nil
}

// Parse phase config
case class InhCheckConfig() extends Config
