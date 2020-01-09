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

package kr.ac.kaist.saf.ast_checker

import java.io._
import scala.util.{ Try, Success }
import scala.collection.mutable._
import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.nodes.ast._
import kr.ac.kaist.saf._
import kr.ac.kaist.saf.phase.InhCheck

import scala.collection.JavaConversions._

import scala.collection.mutable.ListBuffer
import scala.math.{ max, min }
import scala.util.control._

class INHChecker() extends ASTWalker {

  var writer: Option[PrintWriter] = None

  //saves all the contract name
  var con_name: List[String] = List[String]()

  //saves current contract name for the type conversion check
  var con_name_type = new String("")

  //this flag is for removing modifer for vcp
  var flag: Boolean = false

  //this flag is for assignOpApp, prefixOpApp, unaryAssignOpApp
  //this is for inheritance check
  //deals with def and use together
  var flag_use: Boolean = false

  //this flag is for inherited check
  //for function relation
  var flag_funcs: Boolean = false

  //this saves all the striage name and it's type
  //This is for type checking
  var svars_all: HashMap[String, String] = new HashMap[String, String]();

  sealed trait Visibility

  case object PublicV extends Visibility
  case object PrivateV extends Visibility
  case object InternalV extends Visibility
  case object ExternalV extends Visibility
  case object NoneV extends Visibility

  def to_visib[T](visib: T): Visibility = visib match {
    case Public(_) => PublicV
    case Private(_) => PrivateV
    case Internal(_) => InternalV
    case External(_) => ExternalV
    case _ => NoneV
  }

  class Contract(val name: String = "") {
    //var svars: HashMap[String, Visibility] = new HashMap[String, Visibility]();
    var svars: HashMap[String, String] = new HashMap[String, String]();
    var svars_vis: HashMap[String, String] = new HashMap[String, String]();

    var supers = List[String]()
    var funcs = Set[String]()

    //list of potential typo functions
    var funcs_typo = Set[String]()

    //*****************
    //for deprecated API
    //*****************
    var flag_throw: Boolean = false
    var flag_callcode: Boolean = false
    var flag_sha3: Boolean = false
    var flag_suicide: Boolean = false
    var flag_blockhash: Boolean = false
    var flag_msggas: Boolean = false

    //End deprecated API

    //******
    //for inheritance check
    //******
    var variable_hiding = List[String]()
    var unaryOperatorTypo = List[String]()

    var funcs_uses = new HashMap[String, List[String]].withDefaultValue(Nil)
    var funcs_defs = new HashMap[String, List[String]].withDefaultValue(Nil)
    var funcs_calls = new HashMap[String, List[String]].withDefaultValue(Nil)

    //type conversion violation
    var convers_addr: HashMap[String, String] = new HashMap[String, String]();

    //type conversion violation
    var convers_obj: HashMap[String, String] = new HashMap[String, String]();

    //type conversion violation
    var convers_error: HashMap[String, String] = new HashMap[String, String]();

    //temporary store function local variables
    // var fvar_decl = Set[String]()
    var fvar_decl: HashMap[String, String] = new HashMap[String, String]();

    //save variables in state variable use in function
    var f_s_vars: HashMap[String, String] = new HashMap[String, String]();

    // var supers = new java.util.ArrayList[String]()
    def add_sv(name: String, typ: String): Unit = svars.put(name, typ)

    def add_sv_vis(name: String, vis: String): Unit = svars_vis.put(name, vis)

    def add_fsv(name: String, fun: String): Unit = f_s_vars.put(name, fun)

    //add conversion violation
    //name : inner function name
    //func : outer function name
    def add_conver_addr(name: String, func: String): Unit = convers_addr.put(name, func)
    def add_conver_obj(name: String, func: String): Unit = convers_obj.put(name, func)
    def add_conver_error(name: String, func: String): Unit = convers_error.put(name, func)
  }

  //var libraries = new HashMap[String, Library]();
  var contracts = new HashMap[String, Contract]();

  //saves all the storage variable for each contracts.
  //this is for type conversion check
  var contracts_type = new HashMap[String, Contract]();

  var contract_name = new String("")
  var function_name = new String("")

  //for ordering
  var function_total = new String("")

  //contains all super classes for each contracts
  var supers_list = new HashMap[String, List[String]]();
  var supers_tmp = List[String]()

  var final_string_address_type: String = ""
  var final_string_object_type: String = ""
  var final_string_error_type: String = ""
  var final_string_constructor: String = ""
  var final_string_shadow: String = ""
  var final_string_unary: String = ""
  var final_string_depre_throw: String = ""
  var final_string_depre_block: String = ""
  var final_string_depre_sha3: String = ""
  var final_string_depre_callcode: String = ""
  var final_string_depre_sucide: String = ""
  var final_string_depre_gas: String = ""
  var final_string_order: String = ""
  var final_string: String = ""

  var DSC_yes: Boolean = false
  //var target_funcs = List[String]()

  var target_funcs = new ListBuffer[String]()

  def initialize(): Unit = {
    contracts = new HashMap[String, Contract]()
  }

  object EditDistance {
    val ONE = 10
    val INS = ONE
    val DEL = ONE
    val SWAP = ONE

    def apply(source: String, dest: String): Double = {
      val N = source.length
      val M = dest.length
      val s = " " + source
      val d = " " + dest

      if (source == "isPricingStrategy" || source == "isFinalizeAgent" || source == "isUpgradeAgent" || source == "dividendsOf" || source == "endCrowdsale" || source == "_parseIntScientific" || source == "_bytes32" || source == "setInterval" || source == "setCurrencyExchangeRate" || source == "setIdentityExtended" || source == "setInterval" || source == "setBinanceCoinPrice" || source == "setCreditsTokenPrice" || source == "addCurrencyExchangeRate" || source == "addWhiteListedContracts" || source == "addTeamAndAdvisorsAllocation" || source == "addLockTokenAllocation" || source == "addVestTokenAllocation" || source == "addContributors" || source == "addHash" || source == "isUpgradeTarget" || source == "isAffiliateProgram" || source == "isTokenUpgrader" || source == "isWhiteListed" || source == "isWhitelisted" || source == "isCeilingStrategy" || source == "isContract" || source == "isGetWhiteList" || source == "isGeneScience" || source == "isAuthorized" || source == "isTransformAgent" || source == "isFundRequestToken" || source == "isSaleClockAuction" || source == "isController" || source == "isSafeHavenToken" || source == "isDeklaToken" || source == "isMigrationAgent" || source == "doMidnightRun" || source == "doDistribution" || source == "mintToken" || source == "unFrozenToken" || source == " unwhitelist" || source == "isMintableNFT" || source == "isUpgradeInterface" || source == "endCrowdFund" || source == "_getManyBalances" || source == "doDistribution")
        return 0.0

      val Ours = Array.ofDim[Int](N + 1, M + 1)
      (1 to N).foreach((i) => {
        Ours(i)(0) = Ours(i - 1)(0) + DEL
      })
      (1 to M).foreach((j) => {
        Ours(0)(j) = Ours(0)(j - 1) + INS
      })
      (1 to N).foreach((i) => (1 to M).foreach((j) => {
        Ours(i)(j) = Ours(i - 1)(j) + DEL

        if (Ours(i)(j) > Ours(i)(j - 1) + INS) {
          Ours(i)(j) = Ours(i)(j - 1) + INS
        }

        var c = 0
        if (s(i).toString == d(j).toString) {
          c = 0
        } else if (s(i).toString.toLowerCase == d(j).toString.toLowerCase) {
          c = 1
        } else {
          c = ONE
        }

        if (Ours(i)(j) > Ours(i - 1)(j - 1) + c) {
          Ours(i)(j) = Ours(i - 1)(j - 1) + c
        }
        if (i > 1 && j > 1) {
          val sw1 = s(i).toString + s(i - 1)
          val sw2 = d(j - 1).toString + d(j)
          if (sw1 == sw2) {
            if (Ours(i)(j) > Ours(i - 2)(j - 2) + SWAP) {
              Ours(i)(j) = Ours(i - 2)(j - 2) + SWAP
            }
          }
        }
      }))
      Ours(N)(M).toDouble / ONE
    }
  }

  def getSupers(contName: String): List[String] = {
    contracts.get(contName) match {
      case Some(cont) =>
        cont.supers.foldLeft(List[String]())((acc, superName) => {
          val supersOfSuper = superName :: getSupers(superName)
          acc ::: supersOfSuper
        })
      case None =>
        println(s"No contract $contName")
        List[String]()
    }
  }

  def getSupers_type(contName: String): List[String] = {
    contracts_type.get(contName) match {
      case Some(cont) =>
        cont.supers.foldLeft(List[String]())((acc, superName) => {
          val supersOfSuper = superName :: getSupers_type(superName)
          acc ::: supersOfSuper
        })
      case None =>
        println(s"No contract $contName")
        List[String]()
    }
  }

  private object setEnv {

    def set(node: SourceUnit): Unit = node match {
      case SourceUnit(info, body) =>
        body.foreach(set(_))
    } //end set SourceUnit

    def set(node: SourceElement): Unit = node match {
      case ContractDefinition(info, kind, name, supers, body) =>
        kind match {
          case ContractKind(_) =>
            con_name = name.name :: con_name
            con_name_type = name.name
            contracts_type.put(name.name, new Contract(name.name))
            supers.foreach(set(_))
            body.foreach(set(_))

          case _ =>
        }
      case _ =>
    } //end set SourceElement

    def set(node: ContractPart): Unit = node match {

      case StateVariableDeclaration(info, typ, mods, name, body) =>

        contracts_type.get(con_name_type) match {
          case Some(cont) =>
            if (typ.toString(0) == "uint256") {
              cont.add_sv(name.name, "uint")
            } else {
              cont.add_sv(name.name, typ.toString(0))
            }
          case None =>
            println("error in saving storage for type conversion")
        }
      case _ =>
    }

    def set(node: InheritanceSpecifier): Unit = node match {
      case InheritanceSpecifier(info, name, args) =>
        var cont = contracts_type.get(con_name_type).get
        cont.supers = name.toString(0) :: cont.supers
      case _ =>
    }
  }

  def run(node: SourceUnit, mode: Int): (String, String, String, String, String, String, String) = {
    setEnv.set(node)
    walk(node)
    if (mode == 1) {
      //do nothing
    } //throw
    else if (mode == 2) {
      final_string = final_string_depre_throw
    } //sucide
    else if (mode == 3) {
      final_string = final_string_depre_sucide
    } //block
    else if (mode == 4) {
      final_string = final_string_depre_block
    } //callcode
    else if (mode == 5) {
      final_string = final_string_depre_callcode
    } //sha3
    else if (mode == 6) {
      final_string = final_string_depre_sha3
    } //gas
    else if (mode == 7) {
      final_string = final_string_depre_gas
    } //order
    else if (mode == 8) {
      final_string = final_string_order
    }

    (final_string, final_string_unary, final_string_shadow, final_string_constructor, final_string_address_type, final_string_object_type, final_string_error_type)
  }

  override def walk(node: SourceUnit): SourceUnit = node match {
    case SourceUnit(info, body) =>
      initialize()
      body.map(x => walk(x))

      contracts.keys.toList.foreach(k => {
        val tmp = getSupers(k).toSet.toList
        supers_list.put(k, tmp)
      })

      contracts.keys.toList.foreach(k => {
        contracts.get(k) match {
          case Some(i) =>
            //**************for deprecated start
            if (i.flag_callcode)
              final_string_depre_callcode = final_string_depre_callcode.concat("DEP: -> contract: " + k + " deprecated callcode" + "\n")

            if (i.flag_sha3)
              final_string_depre_sha3 = final_string_depre_sha3.concat("DEP: -> contract: " + k + " deprecated sha3" + "\n")

            if (i.flag_suicide)
              final_string_depre_sucide = final_string_depre_sucide.concat("DEP: -> contract: " + k + " deprecated suicide" + "\n")

            if (i.flag_blockhash)
              final_string_depre_block = final_string_depre_block.concat("DEP: -> contract: " + k + " deprecated blockhash" + "\n")

            if (i.flag_msggas)
              final_string_depre_gas = final_string_depre_gas.concat("DEP: -> contract: " + k + " msg gas" + "\n")

            if (i.flag_throw)
              final_string_depre_throw = final_string_depre_throw.concat("DEP: -> contract: " + k + " deprecated throw" + "\n")
            //***************for deprecated end

            //!!!!!!!detection of typo of unary!!!!!!!
            if (!i.unaryOperatorTypo.isEmpty) {
              println("Unary Operator Typo Detect Here")
              final_string_unary = final_string_unary.concat("UNT: -> contract: " + k + " Unary Typo" + i.unaryOperatorTypo + "\n")
            }
            var typo_flag = false
            val loop = new Breaks;

            for ((n) <- i.funcs_typo) {
              var target_string = n.split(":")
              var lev_result = EditDistance(i.name, target_string(0))
              var hold = n.length.min(i.name.length).toDouble * 20 / 100

              if (lev_result <= hold && lev_result > 0 && target_string(1) == "YES") {
                //!!!!!!!detection of constructor typo!!!!!!!!
                final_string_constructor = final_string_constructor.concat("COT: -> contract: " + i.name + " Constructor Typo " + target_string(0) + " lev: " + lev_result + " hold: " + hold + " Comment: Yes" + "\n")
              } else if (lev_result <= hold && lev_result > 0 && target_string(1) == "None") {
                final_string_constructor = final_string_constructor.concat("COT: -> contract: " + i.name + " Constructor Typo " + target_string(0) + " lev: " + lev_result + " hold: " + hold + " Comment: None" + "\n")
              } else if (lev_result > hold && target_string(1) == "YES") {
                final_string_constructor = final_string_constructor.concat("COTUp: -> contract: " + i.name + " Constructor Typo " + target_string(0) + " lev: " + lev_result + " hold: " + hold + " Comment: YES" + "\n")
              }
            }
          case None =>
            println("error Mis-use of constructor")
        }
      })

      for ((k, v) <- supers_list) {
        //*******for finding extension, inherited, refining function*******
        var inh_funcs = List[String]() //super functions
        var base_funcs = List[String]() //most child functions

        //*******for finding inherited storage variables*******
        var inh_storage = List[String]() //super storages defined by parents
        var inh_storage_use = List[String]() //super storages used by parents

        for (name <- v) {
          contracts.get(name) match {
            case Some(i) =>
              for ((n) <- i.funcs) { //save super functions
                target_funcs += n
                inh_funcs = n.split(",")(1) :: inh_funcs
              }

              for ((nam, typ) <- i.svars) { //save super storages
                if (i.svars_vis(nam) != "private" && i.svars_vis(nam) != "HSJ" && i.svars_vis(nam) != "constant")
                  inh_storage = nam :: inh_storage
              }
              inh_storage = inh_storage.toSet.toList

              //************************************
              // * This is for variable hiding start****
              // * collect all the use and def storage from parent
              // * so that we can find the same storage variables which is used by parents
              //" use variables: " + i.funcs_uses + "\n")
              //modifiy variables in the function  def variables: " + i.funcs_defs + "\n")
              //call function in the function Call funcs: " + i.funcs_calls + "\n")
              //access state variable
              for (v <- i.funcs_uses.values) {
                inh_storage_use = v ::: inh_storage_use
              }

              //modify state variable
              for (v <- i.funcs_defs.values) {
                inh_storage_use = v ::: inh_storage_use
              }
              //variable hiding ending
              inh_storage_use = inh_storage_use.toSet.toList

            case None =>
          }
        } //end merging funcs and storage variables

        //***
        //checking inheritance functions
        //***

        //1.getting final contract functions
        var super_exist = 0
        contracts.get(k) match {
          case Some(i) =>
            for ((n) <- i.funcs) {
              base_funcs = n.split(",")(1) :: base_funcs

              if (i.supers.size > 0)
                super_exist = 1
            }

            //For variable hiding : storage in both child and parent
            for (j <- 0 to inh_storage.size - 1) {
              if (i.svars.contains(inh_storage(j)) && inh_storage_use.contains(inh_storage(j))) {
                i.variable_hiding = inh_storage(j) :: i.variable_hiding
              }
            }
            if (!i.variable_hiding.isEmpty) {
              println("Variable Hiding Detect Here")
              final_string_shadow = final_string_shadow.concat("DSV: -> contract: " + k + " variable_hiding" + i.variable_hiding + "\n")
            }

          case None =>
        }

        base_funcs = base_funcs.toSet.toList
        inh_funcs = inh_funcs.toSet.toList

        //***
        //checking ordering functions
        //***
        for (i <- 0 to target_funcs.size - 2) {
          var left = target_funcs(i).split(",");
          for (j <- i + 1 to target_funcs.size - 1) {
            var right = target_funcs(j).split(",");
            if (left(1) == right(1)) {
              supers_list.get(left(0)) match {
                case Some(l) =>
                  if (l.contains(right(0))) {
                  } else {
                    supers_list.get(right(0)) match {
                      case Some(ll) =>
                        if (ll.contains(left(0))) {
                        } else {
                          if (left(0) != right(0) && left(1) != "constructor") {
                            if (left(1).contains("!")) {
                              var tmp_name = left(1).split("!");
                              if (tmp_name(0) != "constructor") {
                                //this is the final version of ORD
                                final_string_order = final_string_order.concat("ORD -> class :" + k + " Function: " + left(1) + " contract: " + left(0) + "," + right(0) + "\n")
                              }
                            }
                          }
                        }
                      case None =>
                        println(s"No contract ${right(0)}")
                    }
                  }

                case None =>
                  println(s"No contract ${left(0)}")
              }
            }
          }
        }
        target_funcs.clear()
      }

      for ((k, v) <- contracts) {
        //violation conversion print
        //address and error only
        for ((inf, ouf) <- v.convers_addr) {
          final_string_address_type = final_string_address_type.concat("VCP: inner function " + inf + ", outer function " + ouf + "\n")
        }

        for ((inf, ouf) <- v.convers_error) {
          final_string_error_type = final_string_error_type.concat("VCP: inner function " + inf + ", outer function " + ouf + "\n")
        }

        //explicit type conversion
        for ((inf, ouf) <- v.convers_obj) {
          if (ouf.contains("HSJ")) {
            final_string_object_type = final_string_object_type.concat("VCP: double inner function " + inf + ", outer function " + ouf + "\n")
          } else {
            val tmp_target = inf.split("!")

            supers_list.get(tmp_target(1)) match {
              case Some(tmp_supers) =>
                if (ouf.contains("$")) {
                  var tmp_ouf = ouf.split("\\$")
                  if (tmp_supers.contains(tmp_ouf(2))) {
                    final_string_object_type = final_string_object_type.concat("VCP: downcasting inner function " + inf + ", outer function " + ouf + "\n")
                  } else {
                    supers_list.get(tmp_ouf(2)) match {
                      case Some(tmp_supers_up) =>
                        if (tmp_supers_up.contains(tmp_target(1))) {
                          final_string_object_type = final_string_object_type.concat("VCP: upcasting inner function " + inf + ", outer function " + ouf + "\n")
                        } else {
                          final_string_object_type = final_string_object_type.concat("VCP: dffcasting inner function " + inf + ", outer function " + ouf + "\n")
                        }
                      case None =>
                        final_string_error_type = final_string_error_type.concat("VCP: inner function " + inf + ", outer function " + ouf + "\n")
                    }
                  }
                } else {
                  if (tmp_supers.contains(ouf)) {
                    final_string_object_type = final_string_object_type.concat("VCP: downcasting inner function " + inf + ", outer function " + ouf + "\n")
                  } else {
                    supers_list.get(ouf) match {
                      case Some(tmp_supers_up) =>
                        if (tmp_supers_up.contains(tmp_target(1))) {
                          final_string_object_type = final_string_object_type.concat("VCP: upcasting inner function " + inf + ", outer function " + ouf + "\n")
                        } else {
                          final_string_object_type = final_string_object_type.concat("VCP: dffcasting inner function " + inf + ", outer function " + ouf + "\n")
                        }
                      case None =>
                        final_string_error_type = final_string_error_type.concat("VCP: inner function " + inf + ", outer function " + ouf + "\n")
                    }
                  }
                }

              case None =>
                final_string_error_type = final_string_error_type.concat("VCP: inner function " + inf + ", outer function " + ouf + "\n")
            }
          }
        } //end explicit type

      }

      node
  }

  override def walk(node: SourceElement): SourceElement = node match {
    case PragmaDirective(info, name) =>
      PragmaDirective(walk(info), walk(name))
    case ContractDefinition(info, kind, name, supers, body) =>
      kind match {
        case ContractKind(_) =>
          contracts.put(name.name, new Contract(name.name))
          contract_name = name.name
          ContractDefinition(walk(info), walk(kind), walk(name),
            supers.map(walk), body.map(walk))
        case LibraryKind(_) =>
          contract_name = "library"
          ContractDefinition(walk(info), walk(kind), walk(name),
            supers.map(walk), body.map(walk))
        case _ => node
      }
  }
  /*
  override def walk(node: ImportDirective): ImportDirective = node match {
    case Import(info, alias) =>
      Import(walk(info), walk(alias))
    case ImportAll(info, to, module) =>
      ImportAll(walk(info), to.map(walk), module)
    case ImportFrom(info, aliases, from) =>
      ImportFrom(walk(info), aliases.map(walk), from)
  }


  override def walk(node: FtnMod): FtnMod = node match {
    case ModifierInvocation(info, name, args) =>
      ModifierInvocation(walk(info), walk(name), args.map(walk))
    case m: StateMutability => walk(m)
    case External(info) => External(walk(info))
    case Internal(info) => Internal(walk(info))
    case Private(info) => Private(walk(info))
    case Public(info) => Public(walk(info))
                  irintln(s"ouf split ${ouf} tmp ${tmp_ouf(0)} ${tmp_ouf(1)}")
  }

  override def walk(node: StateMutability): StateMutability = node match {
    case Payable(info) => Payable(walk(info))
    case Pure(info) => Pure(walk(info))
    case View(info) => View(walk(info))
    case Constant(info) => Constant(walk(info))
  }

  override def walk(node: Mod): Mod = node match {
    case Constant(info) => Constant(walk(info))
    case External(info) => External(walk(info))
    case Internal(info) => Internal(walk(info))
    case Private(info) => Private(walk(info))
    case Public(info) => Public(walk(info))
  }

  override def walk(node: DefinitionKind): DefinitionKind = node match {
    case ContractKind(info) => ContractKind(walk(info))
    case LibraryKind(info) => LibraryKind(walk(info))
    case InterfaceKind(info) => InterfaceKind(walk(info))
  }
*/

  override def walk(node: ContractPart): ContractPart = {
    node match {
      case StateVariableDeclaration(info, typ, mods, name, body) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (typ.toString(0) == "uint256") {
              cont.add_sv(name.name, "uint")
            } else {
              cont.add_sv(name.name, typ.toString(0))
            }
            if (mods != Nil) {
              mods match {
                case h :: t =>
                  h match {
                    case Public(_) | External(_) | Internal(_) | Private(_) =>
                      cont.add_sv_vis(name.name, h.toString(0))
                    case _ =>
                      cont.add_sv_vis(name.name, "HSJ")
                  }
              } //end mod
            } else {
              cont.add_sv_vis(name.name, "none")
            }
          case None =>
            println(s"error : StateVariable ${name.name} contract name: $contract_name")
        }
        StateVariableDeclaration(walk(info), walk(typ), mods.map(walk),
          name, body.map(walk))
      case UsingForDeclaration(info, name, usingfor) =>
        UsingForDeclaration(walk(info), walk(name), usingfor)
      case StructDefinition(info, name, vds) =>
        StructDefinition(walk(info), walk(name), vds.map(walk))
      case ModifierDefinition(info, name, params, body) =>
        ModifierDefinition(walk(info), walk(name), params.map(walk),
          body.map(walk))
      case FunctionDefinition(info, name, params, mods, result, body, comment) =>
        flag = false

        for (mod <- mods) {
          if (mod.isInstanceOf[ModifierInvocation])
            flag = true
        }

        contracts.get(contract_name) match {
          case Some(cont) =>
            var target = true
            //*********For Typo constructor start *********
            mods.foreach { mod =>
              mod match {
                case Private(_) | External(_) | Constant(_) =>
                  target = false
                case (_: ModifierInvocation) =>
                  target = false
                case _ =>
              }
            }

            if (name != None) {
              //if (target == true && result.isEmpty && comment.isEmpty && name.get.name != "constructor" && !body.isEmpty) {
              if (target == true && result.isEmpty && comment.isEmpty && name.get.name != "constructor") {
                cont.funcs_typo.add(name.get.name + ":None")
              } else if (target == true && result.isEmpty && !comment.isEmpty && name.get.name != "constructor") {
                cont.funcs_typo.add(name.get.name + ":YES")
              }
            }

            //*********End**********

            if (name == None) {
              function_name = "fallback"
              function_total = contract_name + ",fallback"
            } else {
              function_name = name.get.name
              function_total = contract_name + "," + name.get.name
            }

            cont.fvar_decl.clear

            for (parameter <- params) {
              parameter.name match {
                case Some(i) =>
                  cont.fvar_decl.put(i.name, parameter.typ.toString(0))
                  function_total = function_total + "!" + parameter.typ.toString(0)
                case None =>
              }
            } //end parameter loop

            if (body.isEmpty != true)
              cont.funcs.add(function_total)

            for (res <- result) {
              res.name match {
                case Some(i) =>
                  cont.fvar_decl.put(i.name, res.typ.toString(0))
                case None =>
              }
            } //end parameter loop

          case None =>
            println(s"Error in Function Definition ${name}")
        }
        FunctionDefinition(walk(info), name.map(walk), params.map(walk),
          mods.map(walk), result.map(walk), body.map(walk), comment)
      case EventDefinition(info, name, params) =>
        EventDefinition(walk(info), walk(name), params.map(walk))
      case EnumDefinition(info, name, vals) =>
        EnumDefinition(walk(info), walk(name), vals.map(walk))
    }
  }

  override def walk(node: Statement): Statement = node match {
    case s: SimpleStatement =>
      walk(s)
    case FtnDefStmt(info, name, params, mods, result, body) =>
      FtnDefStmt(walk(info), name.map(walk), params.map(walk),
        mods.map(walk), result.map(walk), walk(body))
    case If(info, cond, trueB, falseB) =>
      If(walk(info), walk(cond), walk(trueB), falseB.map(walk))
    case While(info, cond, body) =>
      While(walk(info), walk(cond), walk(body))
    case For(info, init, test, update, body) =>
      For(walk(info), init.map(walk), test.map(walk), update.map(walk),
        walk(body))
    case ABlock(info, body) =>
      ABlock(walk(info), body.map(walk))
    case InlineAssembly(info, name, body) =>
      InlineAssembly(walk(info), name, body.map(walk))
    case DoWhile(info, body, cond) =>
      DoWhile(walk(info), walk(body), walk(cond))
    case Continue(info) => Continue(walk(info))
    case Break(info) => Break(walk(info))
    case Return(info, result) => Return(walk(info), result.map(walk))
    case Throw(info) =>
      contracts.get(contract_name) match {
        case Some(cont) =>
          cont.flag_throw = true

        case None =>
          println("Error in throw")
      }
      Throw(walk(info))
    case Emit(info, emit) => Emit(walk(info), walk(emit))
    case Underscore(info) => Underscore(walk(info))
  }

  override def walk(node: SimpleStatement): SimpleStatement = node match {
    case SimpleVarX(info, name, body) =>
      SimpleVarX(walk(info), name.map(_.map(walk)), walk(body))
    case SimpleVar(info, name, body) =>
      SimpleVar(walk(info), walk(name), body.map(walk))
    case VarDecl(info, dec, body) =>
      VarDecl(walk(info), dec.map(walk), body.map(walk))
    case ExprStmt(info, body) =>
      /*
      if (body.isInstanceOf[AssignOpApp]) {
       // println("!!!AssignOpApp!!!")
        //println(s"left is ${body.left.toString(0)}")
      } else if (body.isInstanceOf[Cond])
       // println("!!!Cond!!!")
      else if (body.isInstanceOf[InfixOpApp])
       // println("!!!InfixOpApp!!!")
      else if (body.isInstanceOf[PrefixOpApp])
       // println("!!!PrefixOpApp!!!")
      else if (body.isInstanceOf[UnaryAssignOpApp])
       // println("!!!UnaryAssignOpApp!!!")
      else if (body.isInstanceOf[FunctionCall])
       // println("!!!FunctionCall!!!")
      else if (body.isInstanceOf[Bracket])
       // println("!!!Bracket!!!")
      else if (body.isInstanceOf[Dot])
       // println("!!!Dot!!!")
      else if (body.isInstanceOf[NewCall])
       // println("!!!NewCall!!!")
      else if (body.isInstanceOf[New])
       // println("!!!New!!!")
      else if (body.isInstanceOf[TypeRef])
       // println("!!!TypeRef!!!")
      else if (body.isInstanceOf[VarRef])
       // println("!!!VarRef!!!")
      else if (body.isInstanceOf[ArrayLiteral])
       // println("!!!ArrayLiteral!!!")
      else if (body.isInstanceOf[TupleLiteral])
       // println("!!!TupleLiteral!!!")
      else if (body.isInstanceOf[IntLiteral])
       // println("!!!IntLiteral!!!")
*/
      ExprStmt(walk(info), walk(body))
  }

  override def walk(node: Expression): Expression = {
    node match {
      case AssignOpApp(info, left, op, right) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (!cont.fvar_decl.contains(left.toString(0)) && contract_name != function_name && function_name != "constructor") {
              //remove bracket. 
              //ex. a[0] array
              //only saves a
              if (left.toString(0).contains("[")) {
                val index = left.toString(0).indexOf("[")
                val newName = left.toString(0).substring(0, index)
                cont.funcs_defs(function_name) ::= newName
              } else
                cont.funcs_defs(function_name) ::= left.toString(0)
            }
            //remove = ++a
            if (node.toString(0).contains("= +") && (!right.toString(0).contains("++"))) {
              cont.unaryOperatorTypo = left.toString(0) :: cont.unaryOperatorTypo
            }
          case None =>
            println(s"error in AssignOpApp")
        }
        flag_use = true
        AssignOpApp(walk(info), left, walk(op), walk(right)) //left is def and right is use
      //AssignOpApp(walk(info), walk(left), walk(op), walk(right))
      case Cond(info, cond, trueE, falseE) =>
        Cond(walk(info), walk(cond), walk(trueE), walk(falseE))
      case InfixOpApp(info, left, op, right) =>
        //stores var uses in VARREF * For Inheritance Check
        InfixOpApp(walk(info), walk(left), walk(op), walk(right))
      case PrefixOpApp(info, op, right) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (!cont.fvar_decl.contains(right.toString(0)) && function_name != contract_name && function_name != "constructor") {
              cont.funcs_defs(function_name) ::= right.toString(0)
            }
          case None =>
            println(s"error in PrefixOpApp")
        }

        if (flag_use) {
          flag_use = false
          PrefixOpApp(walk(info), walk(op), walk(right))
        } else {
          flag_use = false
          PrefixOpApp(walk(info), walk(op), right)
        }

      case UnaryAssignOpApp(info, lhs, op) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (!cont.fvar_decl.contains(lhs.toString(0)) && function_name != contract_name && function_name != "constructor") {
              cont.funcs_defs(function_name) ::= lhs.toString(0)
            }
          case None =>
            println(s"error in UnaryAssignOpApp")
        }
        if (flag_use) {
          flag_use = false
          UnaryAssignOpApp(walk(info), walk(lhs), walk(op))
        } else {
          flag_use = false
          UnaryAssignOpApp(walk(info), lhs, walk(op))
        }
      //UnaryAssignOpApp(walk(info), walk(lhs), walk(op))
      case FunctionCall(info, ftn, args) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (ftn.toString(0) == "sha3")
              cont.flag_sha3 = true

            if (ftn.toString(0) == "suicide")
              cont.flag_suicide = true

            if (ftn.toString(0) == "block.blockhash")
              cont.flag_blockhash = true

            if (con_name.contains(ftn.toString(0))) {
              if (args.length == 1) {
                if (args(0).isInstanceOf[FunctionCall]) {

                  args(0) match {
                    case FunctionCall(info_sub, ftn_sub, args_sub) =>
                      if (ftn_sub.toString(0) == "address") {
                        cont.add_conver_addr(info_sub.toString(), "address()")
                      }
                  }
                } //end if args.isInstance
                else {
                  var tmp_arg = "0"
                  var tmp_origin = args(0).toString(0)
                  if (tmp_origin.contains("[")) {
                    tmp_arg = tmp_origin.substring(0, tmp_origin.indexOf("["))
                  } else {
                    tmp_arg = args(0).toString(0)
                  }
                  cont.fvar_decl.get(tmp_arg) match {
                    case Some(arg_type) =>
                      var tmp_arg_type = "0"
                      if (arg_type.contains("[")) {
                        tmp_arg_type = arg_type.substring(0, arg_type.indexOf("["))
                      } else {
                        tmp_arg_type = arg_type
                      }
                      if (tmp_arg_type != ftn.toString(0)) {
                        if (tmp_arg_type.count(_ == '>') > 1) {
                          cont.add_conver_error(info.toString(), tmp_arg_type)
                        } else if (tmp_arg_type.contains("=> address")) {
                          cont.add_conver_addr(info.toString(), tmp_arg_type)
                        } else if (tmp_arg_type.contains("uint")) {
                          cont.add_conver_addr(info.toString(), tmp_arg_type)
                        } else if (tmp_arg_type != "address") {
                          cont.add_conver_obj(info.toString() + "!" + ftn.toString(0), tmp_arg_type)
                        } else if (tmp_arg_type == "address") {
                          cont.add_conver_addr(info.toString(), tmp_arg_type)
                        }
                      }

                    case None =>
                      contracts_type(contract_name).svars.get(tmp_arg) match {
                        case Some(arg_svr) =>
                          var tmp_arg_val = "0"
                          if (arg_svr.contains("[")) {
                            tmp_arg_val = arg_svr.substring(0, arg_svr.indexOf("["))
                          } else {
                            tmp_arg_val = arg_svr
                          }
                          if (tmp_arg_val != ftn.toString(0)) {

                            if (tmp_arg_val.count(_ == '>') > 1) {
                              cont.add_conver_error(info.toString(), tmp_arg_val)
                            } else if (tmp_arg_val.contains("=> address")) {
                              cont.add_conver_addr(info.toString(), tmp_arg_val)
                            } else if (tmp_arg_val == "uint") {
                              cont.add_conver_addr(info.toString(), tmp_arg_val)
                            } else if (tmp_arg_val != "address") {
                              cont.add_conver_obj(info.toString() + "!" + ftn.toString(0), tmp_arg_val)
                            } else if (tmp_arg_val == "address") {
                              cont.add_conver_addr(info.toString(), tmp_arg_val)
                            }
                          }
                        case None =>
                          if (tmp_arg.startsWith("0x") || tmp_arg.startsWith("0X") || tmp_arg.startsWith("\'0x") || tmp_arg.startsWith("\'0X'")) {
                            cont.add_conver_addr(info.toString(), "address")
                          } else if (tmp_arg == "0") {
                            cont.add_conver_addr(info.toString(), "address")
                          } else if (tmp_arg == "msg.sender") {
                            cont.add_conver_addr(info.toString(), "address")
                          } //checking parent storage
                          else if (tmp_arg == "this") {
                            cont.add_conver_addr(info.toString(), "address")
                          } //else if (svars_all.contains(tmp_arg))
                          else {
                            val tmp = getSupers(contract_name).toSet.toList
                            var all_storages = List[String]() // got from supers' storage

                            var flag_type_one: Boolean = false
                            var flag_type_double: Boolean = false
                            var flag_type_address: Boolean = false
                            var flag_type_object: Boolean = false
                            var flag_type_hoisting: Boolean = false

                            for (super_name <- tmp) {
                              contracts_type.get(super_name) match {
                                case Some(i) =>
                                  for ((k_s, v_s) <- i.svars) {
                                    //matched in the parent
                                    if (k_s == tmp_arg) {
                                      flag_type_hoisting = true
                                      if (flag_type_one) {
                                        flag_type_double = true
                                      }
                                      var tmp_v = "0"
                                      if (v_s.contains("[")) {
                                        tmp_v = v_s.substring(0, v_s.indexOf("["))
                                      } else { tmp_v = v_s }

                                      if (tmp_v.contains("=> address")) {
                                        flag_type_address = true
                                      } else if (tmp_v == "uint") {
                                        flag_type_address = true
                                      } else if (tmp_v == "address") {
                                        flag_type_address = true
                                      } else if (tmp_v != "address") {
                                        flag_type_object = true
                                      }
                                      //cont.add_conver(info.toString() + "!" + ftn.toString(0), tmp_arg_val)
                                      var new_val = i.name + "$" + k_s + "$" + tmp_v
                                      all_storages = new_val :: all_storages
                                      flag_type_one = true
                                    }
                                  }
                                case None =>
                                  println("error in conversion : due to interface")
                              } //end get contract
                            } //end for

                            val result_tmp = all_storages.mkString(" ")

                            if (flag_type_address && flag_type_double) {
                              //yes address and two same name storages
                              cont.add_conver_addr(info.toString(), result_tmp + "Check")
                            } else if (flag_type_address) {
                              cont.add_conver_addr(info.toString(), result_tmp)
                            } else if (flag_type_object && flag_type_double) {
                              //no address and two same name storages
                              cont.add_conver_obj(info.toString() + "!" + ftn.toString(0), result_tmp + "Check")
                            } else if (flag_type_object) {
                              cont.add_conver_obj(info.toString() + "!" + ftn.toString(0), result_tmp)
                            } else if (flag_type_hoisting == false) {
                              cont.add_conver_error(info.toString(), tmp_arg)
                            }
                          } //finish checking parent's storages
                      } //finish matching storages
                  } //finish matching local variable
                } //end else
              } //end if args.length == 1 
            }
          case None =>
            println("Funcationcall: no contract")

        } //end get contract

        flag_funcs = true
        FunctionCall(walk(info), walk(ftn), args.map(walk))
      case Bracket(info, ftn, args) =>
        Bracket(walk(info), walk(ftn), args.map(walk))
      case Dot(info, base, name) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (flag_funcs) {
              flag_funcs = false
              cont.funcs_calls(function_name) ::= name.toString(0)
            }

            if (name.toString(0) == "callcode")
              cont.flag_callcode = true

            if (base.toString(0) == "msg" && name.toString(0) == "gas")
              cont.flag_msggas = true

          case None =>
            println("Error in Dot")
        }

        Dot(walk(info), walk(base), walk(name))
      case NewCall(info, call) =>
        NewCall(walk(info), call)
      case New(info, name) =>
        New(walk(info), walk(name))
      case TypeRef(info, name) =>
        TypeRef(walk(info), walk(name))
      case VarRef(info, name) =>
        contracts.get(contract_name) match {
          case Some(cont) =>
            if (cont.svars.contains(name.name) && !cont.fvar_decl.contains(name.name)) {
              if (contract_name != function_name)
                cont.add_fsv(name.name, function_name)
            }
            //for inheritance check
            if (flag_funcs) {
              flag_funcs = false
              cont.funcs_calls(function_name) ::= name.name
            } else {
              if (!cont.fvar_decl.contains(name.name) && function_name != contract_name && function_name != "constructor") {
                cont.funcs_uses(function_name) ::= name.name
              }
            }
          case None =>
            println(s"error VarRef ${name.name} contract name: $contract_name")
        }

        VarRef(walk(info), walk(name))
      case ArrayLiteral(info, elems) =>
        ArrayLiteral(walk(info), elems.map(walk))
      case TupleLiteral(info, elems) =>
        TupleLiteral(walk(info), elems.map(walk))
      case TupleOptLiteral(info, elems) =>
        TupleOptLiteral(walk(info), elems.map(_.map(walk)))
      case l: Literal =>
        super.walk(l)
    }
  }
  /*
  override def walk(node: Literal): Literal = node match {
    case Bool(info, bool) => Bool(walk(info), bool)
    case s: StringLiteral => walk(s)
    case n: NumberLiteral => walk(n)
  }

  override def walk(node: StringLiteral): StringLiteral = node match {
    case StringLiteral(info, quote, str) =>
      StringLiteral(walk(info), quote, str)
  }

  override def walk(node: NumberLiteral): NumberLiteral = node match {
    case IntLiteral(info, intVal, radix) =>
      IntLiteral(walk(info), intVal, radix)
    case DoubleLiteral(info, text, num) =>
      DoubleLiteral(walk(info), text, num)
  }

  override def walk(node: AssemblyItem): AssemblyItem = node match {
    case AssemblyBlock(info, body) =>
      AssemblyBlock(walk(info), body.map(walk))
    case AssemblyLetX(info, name, body) =>
      AssemblyLetX(walk(info), name.map(walk), walk(body))
    case AssemblyLet(info, name, body) =>
      AssemblyLet(walk(info), name.map(walk), body.map(walk))
    case AssemblyIf(info, cond, body) =>
      AssemblyIf(walk(info), walk(cond), body.map(walk))
    case AssemblySwitch(info, cond, cases, default) =>
      AssemblySwitch(walk(info), walk(cond), cases.map(walk),
        default.map(walk))
    case AssemblyFtn(info, name, param, body) =>
      AssemblyFtn(walk(info), walk(name), param.map(walk),
        body.map(walk))
    case AssemblyFor(info, init, change, cond, body) =>
      AssemblyFor(walk(info), init.map(walk), walk(change),
        cond.map(walk), body.map(walk))
    case AssemblyAssignX(info, name, body) =>
      AssemblyAssignX(walk(info), walk(name), walk(body))
    case AssemblyAssign(info, name, body) =>
      AssemblyAssign(walk(info), walk(name), body.map(walk))
    case AssemblyLabel(info, label) =>
      AssemblyLabel(walk(info), walk(label))
    case AssemblyNumber(info, num) =>
      AssemblyNumber(walk(info), walk(num))
    case AssemblyString(info, str) =>
      AssemblyString(walk(info), walk(str))
    case AssemblyBreak(info) => AssemblyBreak(walk(info))
    case AssemblyContinue(info) => AssemblyContinue(walk(info))
    case SubAssembly(info, name, body) =>
      SubAssembly(walk(info), walk(name), body.map(walk))
    case f: FtnAssembly => walk(f)
    case AssemblyExprStmt(info, body) =>
      AssemblyExprStmt(walk(info), walk(body))
    case AssemblyId(info, name) =>
      AssemblyId(walk(info), walk(name))
    case a: AssemblyExpression => walk(a)
  }

  override def walk(node: FtnAssembly): FtnAssembly = node match {
    case FtnAssembly(info, name, body) =>
      FtnAssembly(walk(info), walk(name), body.map(walk))
  }
*/
  override def walk(node: AssemblyExpression): AssemblyExpression = node match {
    case AssemblyCall(info, name, args) =>
      AssemblyCall(walk(info), walk(name), args.map(walk))
    case AssemblyNumber(info, num) =>
      AssemblyNumber(walk(info), walk(num))
    case AssemblyString(info, str) =>
      AssemblyString(walk(info), walk(str))
    case AssemblyId(info, name) => AssemblyId(walk(info), walk(name))
  }
  /*
  override def walk(node: TypeName): TypeName = node match {
    case ArrayTypeName(info, name, size) =>
      ArrayTypeName(walk(info), walk(name), size.map(walk))
    case ElementaryTypeName(info, typ) =>
      ElementaryTypeName(walk(info), typ)
    case UserDefinedTypeName(info, names) =>
      UserDefinedTypeName(walk(info), names.map(walk))
    case Mapping(info, domain, range) =>
      Mapping(walk(info), domain, walk(range))
    case FunctionTypeName(info) =>
      FunctionTypeName(walk(info))
  }

  override def walk(node: Alias): Alias = node match {
    case Alias(info, from, to) =>
      Alias(walk(info), walk(from), to.map(walk))
  }
*/
  override def walk(node: InheritanceSpecifier): InheritanceSpecifier = node match {
    case InheritanceSpecifier(info, name, args) =>
      var cont = contracts.get(contract_name).get
      cont.supers = name.toString(0) :: cont.supers
      InheritanceSpecifier(walk(info), walk(name), args.map(walk))
  }

  override def walk(node: VariableDeclaration): VariableDeclaration = node match {
    case VariableDeclaration(info, typ, loc, name) =>
      contracts.get(contract_name) match {
        case Some(cont) =>
          cont.fvar_decl.put(name.name, typ.toString(0))
        case None =>
          println(s"Error in VariableDeclaration")
      }

      VariableDeclaration(walk(info), walk(typ), loc, walk(name))
  }

  override def walk(node: Parameter): Parameter = node match {
    case Parameter(info, typ, loc, name) =>
      Parameter(walk(info), walk(typ), loc, name.map(walk))
  }
  override def walk(node: Id): Id = node match {
    case Id(info, name) =>
      Id(walk(info), name)
  }
}
