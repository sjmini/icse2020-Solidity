/**
 *
 * *****************************************************************************
 * *****************************************************************************
 * Copyright (c) 2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * *****************************************************************************
 */

package kr.ac.kaist.saf.ast_checker

import kr.ac.kaist.saf.util._
import kr.ac.kaist.saf.nodes.ast._
import scala.collection.mutable._

object ASTChecker {

  sealed trait Val

  type Env = HashMap[String, Var]

  case class StoRef() extends StorageLocation {
    def toString(indent: Int): String = "storage pointer"
  }

  case class Var(loc: StorageLocation, init: Boolean) extends Val
  case class VarT(tup: List[Var]) extends Val

  case class P(typ: TypeName, loc: StorageLocation, id: Option[String])

  case class F(
    id: Option[String],
    mod: FtnMod,
    param: List[P],
    res: List[P],
    stmt: List[Statement],
    comment: String
  )
  case class C(env: HashMap[String, Var], st: List[String], func: List[F])
  case class L(st: List[String], func: List[F])

  case class Ctx(cl: String, fm: String, svu: Boolean)

  case class VarExp(info: ASTNodeInfo, v: Val) extends Expression

  private var cont: HashMap[String, C] = new HashMap[String, C]()
  private var lib: HashMap[String, L] = new HashMap[String, L]()

  private var ctx: Ctx = Ctx("", "", false)
  private var cur: String = ""
  private var cf: String = ""
  private var csvu: String = ""

  private var clf: Option[String] = None
  private var llfm: Option[String] = None
  private var clm: Option[String] = None

  private var res: Array[Int] = Array.fill(39)(0)
  private var usp: List[String] = List[String]()
  private var svu: List[String] = List[String]()
  private var fchain: List[(String, Int)] = List[(String, Int)]()

  private var retv: Option[Val] = None

  private var ilflag: Boolean = false
  private var mflag: Boolean = false

  private var modsv: HashMap[String, List[String]] = HashMap[String, List[String]]()
  private var pfsv: HashMap[String, HashMap[String, List[String]]] = HashMap[String, HashMap[String, List[String]]]()
  private var pfwmsvm: List[(String, String)] = List[(String, String)]()

  private var rel: Array[String] = Array("LIBRARY", "USP", "LIB_FUNC", "INT_LIB_FUNC", "NINT_LIB_FUNC",
    "ASM_BLOCK_LIB", "LIB_ASM_BLOCK", "CONTRACT", "CONT_FUNC", "FUNC_NO_VISIB", "FUNC_NO_VISIB_SV", "ALL_SV",
    "PUBLIC_SV", "PRIVATE_SV", "INT_SV", "CONT_ASM_BLOCK_F", "ASM_BLOCK_CONT_F", "ASM_DELEGATECALL_F",
    "DELEGATECALLS_F", "ASM_CALLCODE_F", "CALLCODES_F", "MSTORES_F", "CONT_ASM_BLOCK_M", "ASM_BLOCK_CONT_M",
    "ASM_DELEGATECALL_M", "DELEGATECALLS_M", "ASM_CALLCODE_M", "CALLCODES_M", "MSTORES_M",
    "CONT_ASM_BLOCK", "ASM_BLOCK_CONT", "ASM_DELEGATECALL", "DELEGATECALLS", "ASM_CALLCODE",
    "CALLCODES", "MSTORES", "PFWMSVM", "PUB_LIB_FUNC", "ILFIM")

  def update(n: List[Int]): Unit = n.foreach { ne => res.update(ne, res(ne) + 1) }

  private object GEnvSetter {
    def set(node: SourceUnit): Unit = node match {
      case SourceUnit(info, body) =>
        body.foreach(set(_))
    }
    def set(node: SourceElement): Unit = node match {
      case ContractDefinition(info, kind, name, supers, body) =>
        kind match {
          case ContractKind(_) =>
            val newc = C(HashMap[String, Var](), List[String](), List[F]())
            val newic = supers.foldLeft(body.foldLeft(newc) { set(_, _) }) { set(_, _) }
            cont += (name.name -> newic)
            modsv += (name.name -> List[String]())
            pfsv += (name.name -> HashMap[String, List[String]]())
          case LibraryKind(_) =>
            val newl = L(List[String](), List[F]())
            lib += (name.name -> body.foldLeft(newl) { set(_, _) })
            modsv += (name.name -> List[String]())
            pfsv += (name.name -> HashMap[String, List[String]]())
          case _ =>
        }
      case _ =>
    }
    def set(newc: C, node: InheritanceSpecifier): C = node match {
      case InheritanceSpecifier(info, name, args) =>
        val icn = name.toString(0)
        if (cont.contains(icn)) {
          val icnc = cont(icn)
          C(newc.env ++: icnc.env, (newc.st ++: icnc.st).distinct, (newc.func ++: icnc.func).distinct)
        } else
          newc
    }
    def set(newc: C, node: ContractPart): C = node match {
      case StateVariableDeclaration(info, typ, mods, name, body) =>
        modCheck(mods)
        newc.copy(env = (newc.env += (name.name -> Var(Storage(), true))))
      case StructDefinition(info, name, vds) =>
        newc.copy(st = newc.st :+ name.name)
      case ModifierDefinition(info, name, params, body) =>
        set(newc, FunctionDefinition(info, Some(name), params, List[FtnMod](), List[Parameter](), body, ""))
      case FunctionDefinition(info, name, params, mods, result, body, comment) =>
        val fid = name.map { x => x.name }
        val nf = F(fid, getM(mods), params.map(getP), result.map(getP), body, comment)
        newc.copy(func = newc.func :+ nf)
      case _ => newc
    }
    def set(newl: L, node: ContractPart): L = node match {
      case StructDefinition(info, name, vds) =>
        newl.copy(st = newl.st :+ name.name)
      case ModifierDefinition(info, name, params, body) =>
        set(newl, FunctionDefinition(info, Some(name), params, List[FtnMod](), List[Parameter](), body, ""))
      case FunctionDefinition(info, name, params, mods, result, body, comment) =>
        val fid = name.map { x => x.name }
        val nf = F(fid, getM(mods), params.map(getP), result.map(getP), body, comment)
        newl.copy(func = newl.func :+ nf)
      case _ => newl
    }
    def getP(p: Parameter): P = p match {
      case Parameter(info, typ, loc, name) =>
        loc match {
          case Some(Storage()) => P(typ, StoRef(), name.map { x => x.name })
          case _ => P(typ, Memory(), name.map { x => x.name })
        }
    }
    def getM(mods: List[FtnMod]): FtnMod = mods match {
      case h :: t =>
        h match {
          case Public(_) | External(_) | Internal(_) | Private(_) => h
          case _ => getM(t)
        }
      case _ => Public(ASTNodeInfo(Span()))
    }
    def modCheck(mods: List[Mod]): Unit = mods match {
      case h :: t =>
        h match {
          case Public(_) => update(List(11, 12))
          case Private(_) => update(List(11, 13))
          case Internal(_) => update(List(11, 14))
          case _ => modCheck(t)
        }
      case _ => update(List(11))
    }
  }

  def setFixP(unit: SourceUnit): Unit = {
    val pcont = cont; val plib = lib
    GEnvSetter.set(unit)
    if (pcont != cont || plib != lib)
      setFixP(unit)
  }
  def initialize(): Unit = {
    cont = HashMap[String, C]()
    lib = HashMap[String, L]()
    ctx = Ctx("", "", false)
    cur = ""; cf = ""; csvu = ""; clf = None; llfm = None; clm = None
    res = Array.fill(39)(0)
    usp = List[String](); svu = List[String](); retv = None; ilflag = false; mflag = false
    fchain = List[(String, Int)]()
    modsv = HashMap[String, List[String]]()
    pfsv = HashMap[String, HashMap[String, List[String]]]()
    pfwmsvm = List[(String, String)]()
  }
  def run(unit: SourceUnit): (String, String, Int, Int) = {
    initialize()
    setFixP(unit)
    walk(unit)
    pfsv.foreach { c =>
      c._2.foreach { f =>
        if (f._2.intersect(modsv(c._1)).length != 0 && f._1 != c._1 &&
          f._1 != "" && f._1 != "constructor") {
          update(List(36))
          pfwmsvm = pfwmsvm :+ (c._1, f._1)
        }
      }
    }

    val r1 = if (usp.length != 0)
      usp.foldLeft("USP || ")((cr, us) => cr + us + " ") + "\n\n" else ""
    val r2 = if (pfwmsvm.length != 0)
      pfwmsvm.foldLeft("PFWMSVM || ")((rr, sv) => rr + "[" + sv._1 + " -> " + sv._2 + "] ") + "\n\n"
    else ""
    (r1, r2, res(1), res(36))
  }
  def setCtx(x: String, y: String): Unit = x match {
    case "Init" => ctx = Ctx("", "", false)
    case "C" =>
      ctx = Ctx("C", "", false); cur = y
    case "L" =>
      ctx = Ctx("L", "", false); cur = y
    case "F" =>
      ctx = ctx.copy(fm = "F"); cf = y
    case "M" =>
      ctx = ctx.copy(fm = "M"); cf = y
    case "YV" => ctx = ctx.copy(svu = false)
    case "NV" => ctx = ctx.copy(svu = true)
    case _ =>
  }
  def walk(node: SourceUnit): Unit = node match {
    case SourceUnit(info, body) =>
      setCtx("Init", "")
      body.foreach(walk(_))
  }
  def walk(node: SourceElement): Unit = node match {
    case ContractDefinition(info, kind, name, supers, body) =>
      kind match {
        case ContractKind(_) =>
          update(List(7)); setCtx("C", name.name)
          body.foreach(walk(_))
        case LibraryKind(_) =>
          update(List(0)); setCtx("L", name.name)
          body.foreach(walk(_))
        case _ =>
      }
    case _ =>
  }
  def walk(node: FtnMod): String = node match {
    case External(info) => "External"
    case Internal(info) => "Internal"
    case Private(info) => "Private"
    case Public(info) => "Public"
    case _ => "None"
  }
  def getThis: C = {
    if (cont.contains(cur))
      cont(cur)
    else if (lib.contains(cur))
      C(HashMap[String, Var](), lib(cur).st, lib(cur).func)
    else
      C(HashMap[String, Var](), List[String](), List[F]())
  }

  def makeEnv(params: List[Parameter]): Env = {
    params.map(GEnvSetter.getP).foldLeft(HashMap[String, Var]()) {
      (hm, p) =>
        {
          if (p.id.isEmpty) hm
          else hm += (p.id.get -> Var(p.loc, true))
        }
    }
  }
  def makeEnvR(params: List[P]): Env = {
    params.foldLeft(HashMap[String, Var]()) {
      (hm, p) =>
        {
          if (p.id.isEmpty) hm
          else hm += (p.id.get -> Var(p.loc, false))
        }
    }
  }
  def getVar(v: Val): Var = v match {
    case v: Var => v.asInstanceOf[Var]
    case _ => Var(Memory(), true)
  }
  def addChain(n: Any): Unit = n match {
    case ModifierDefinition(info, name, params, body) =>
      fchain = fchain :+ (name.name, params.length)
    case FunctionDefinition(info, name, params, mods, result, body, comment) =>
      fchain = fchain :+ (name.fold("")(_.name), params.length)
    case F(id, mod, param, res, stmt, comment) => fchain = fchain :+ (id.get, param.length)
    case _ =>
  }
  def remvChain(n: Any): Unit = n match {
    case ModifierDefinition(info, name, params, body) =>
      fchain = fchain.filterNot { f => f._1 == name.name && f._2 == params.length }
    case FunctionDefinition(info, name, params, mods, result, body, comment) =>
      fchain = fchain.filterNot { f => f._1 == name.fold("")(_.name) && f._2 == params.length }
    case F(id, mod, param, res, stmt, comment) =>
      fchain = fchain.filterNot { f => f._1 == id.get && f._2 == param.length }
    case _ =>
  }
  def walk(node: ContractPart): Env = node match {
    case ModifierDefinition(info, name, params, body) =>
      setCtx("M", name.name); ilflag = false; mflag = true
      addChain(node)
      val ret = body.foldLeft(makeEnv(params)) { (env, b) => walk(env, b) }
      remvChain(node); ret
    case FunctionDefinition(info, name, params, mods, result, body, comment) =>
      setCtx("F", name.fold("")(_.name)); ilflag = false; mflag = false
      addChain(node)
      ctx.cl match {
        case "C" =>
          if (!mods.exists { x => List("External", "Internal", "Public", "Private").contains(walk(x)) }) {
            update(List(8, 9))
            setCtx("NV", "")
            if (!mods.exists { x => x.isInstanceOf[ModifierInvocation] })
              pfsv += (cur -> (pfsv(cur) += (name.fold("")(_.name) -> List[String]())))
          } else {
            update(List(8))
            setCtx("YV", "")
          }
        case "L" =>
          if (mods.exists { x => walk(x) == "Internal" }) {
            ilflag = true
            update(List(2, 3))
          } else if (mods.exists { x => walk(x) == "Public" })
            update(List(2, 4, 37))
          else if (!mods.exists { x => List("External", "Internal", "Public", "Private").contains(walk(x)) }) {
            update(List(2, 4, 37))
            if (!mods.exists { x => x.isInstanceOf[ModifierInvocation] })
              pfsv += (cur -> (pfsv(cur) += (name.fold("")(_.name) -> List[String]())))
          } else
            update(List(2, 4))
      }
      val ret = body.foldLeft(makeEnv(params)) { (env, b) => walk(env, b) }
      remvChain(node); ret
    case _ => HashMap[String, Var]()
  }
  def walk(env: Env, node: Statement): Env = node match {
    case s: SimpleStatement => walk(env, s)
    case If(info, cond, trueB, falseB) =>
      falseB match {
        case Some(e) =>
          walk(walk(walk(env, cond)._1, trueB), e).filter {
            k => env.contains(k._1)
          }
        case None =>
          walk(walk(env, cond)._1, trueB).filter {
            k => env.contains(k._1)
          }
      }
    case While(info, cond, body) =>
      walk(walk(env, cond)._1, body).filter { k => env.contains(k._1) }
    case For(info, init, test, update, body) =>
      val env1 = init.fold(env) { i => walk(env, i) }
      val env2 = test.fold(env1) { t => walk(env1, t)._1 }
      val env3 = walk(env2, body)
      update.fold(env3) { u => walk(env3, u) }.filter { k => env.contains(k._1) }
    case ABlock(info, body) =>
      body.foldLeft(env)(walk(_, _)).filter { k => env.contains(k._1) }
    case InlineAssembly(info, name, body) =>
      ctx match {
        case Ctx("C", "F", _) =>
          clf match {
            case Some(s) =>
              update(List(15, 16, 29, 30)); clf = None
            case None => update(List(16, 30))
          }
        case Ctx("C", "M", _) =>
          clm match {
            case Some(s) =>
              update(List(22, 23, 29, 30)); clm = None
            case None => update(List(23, 30))
          }
        case Ctx("L", "F", _) | Ctx("L", "M", _) =>
          if (ilflag)
            update(List(38)); ilflag = false
          llfm match {
            case Some(s) =>
              update(List(5, 6)); llfm = None
            case None => update(List(5))
          }
        case _ =>
      }
      body.foreach(walk(_))
      env
    case DoWhile(info, body, cond) =>
      walk(walk(env, body), cond)._1.filter { k => env.contains(k._1) }
    case Return(info, result) =>
      result match {
        case Some(e) =>
          val ret = walk(env, e)
          retv = Some(ret._2); ret._1
        case None => env
      }

    case Emit(info, emit) => walk(env, emit)._1
    case _ => env
  }
  def getSL(typ: TypeName): StorageLocation = typ match {
    case ElementaryTypeName(_, _) | FunctionTypeName(_) => Memory()
    case ArrayTypeName(_, _, _) => StoRef()
    case Mapping(_, _, _) => Memory()
    case UserDefinedTypeName(info, names) =>
      names match {
        case List(id) =>
          if (getThis.st.exists { _ == id.name }) StoRef()
          else Memory()
        case Id(_, n) :: List(Id(_, m)) =>
          if (cont.contains(n) && cont(n).st.exists { _ == m }) StoRef()
          else if (lib.contains(n) && lib(n).st.exists { _ == m }) StoRef()
          else Memory()
        case _ => Memory()
      }
    case _ => Memory()
  }
  def walk(node: VariableDeclaration, init: Boolean): Var = node match {
    case VariableDeclaration(info, typ, loc, name) =>
      val rloc = loc match {
        case Some(Storage()) => StoRef()
        case Some(l) => l
        case None => getSL(typ)
      }
      rloc match {
        case Memory() | Storage() => Var(rloc, true)
        case StoRef() => Var(rloc, init)
      }
  }
  def walk(env: Env, node: SimpleStatement): Env = node match {
    case SimpleVarX(info, name, body) =>
      val ret = walk(env, body)
      ret._2 match {
        case VarT(tup) =>
          name.zip(tup).foldLeft(ret._1) { (env, nt) =>
            {
              if (!nt._1.isEmpty)
                env += (nt._1.get.name -> nt._2)
              else
                env
            }
          }
        case _ => env
      }
    case SimpleVar(info, name, body) =>
      body match {
        case Some(e) =>
          walk(env, e) match {
            case (nenv, Var(loc, init)) => nenv += (name.name -> Var(loc, init))
            case _ => env
          }
        case None => env
      }
    case VarDecl(info, dec, body) =>
      (dec, body) match {
        case (List(d), Some(e)) =>
          val ret = walk(env, e)
          if (ret._2.isInstanceOf[Var])
            ret._1 += (d.name.name -> walk(d, ret._2.asInstanceOf[Var].init))
          else
            ret._1
        case (List(d), None) => env += (d.name.name -> walk(d, false))
        case (_, Some(e)) =>
          walk(env, e) match {
            case (nenv, VarT(tup)) =>
              dec.zip(tup).foldLeft(nenv) { (ee, dt) =>
                ee += (dt._1.name.name -> walk(dt._1, dt._2.init))
              }
            case _ => env
          }

        case (_, None) =>
          dec.foldLeft(env) { (ee, d) => ee += (d.name.name -> walk(d, false)) }
        case _ => env
      }
    case ExprStmt(info, body) => walk(env, body)._1
  }
  def svuCheck(v: Val): Unit = v match {
    case Var(Storage(), _) | Var(StoRef(), _) =>
      if (ctx.svu) {
        update(List(10))
        svu = svu :+ ("[" + cur + " -> " + cf + " -> " + csvu + "] ")
      }
      val pfc = pfsv(cur)
      if (pfc.contains(cf))
        pfsv += (cur -> (pfc += (cf -> (pfc(cf) :+ csvu))))
    case VarT(tup) => tup.foreach(svuCheck(_))
    case _ =>
  }
  def howmany(n: String, length: Int): List[F] = {
    val m1 = getThis.func.filter { f => !f.id.isEmpty && f.id.get == n }
    m1.filter { m => m.param.length == length }
  }

  def walk(env: Env, node: Expression): (Env, Val) = node match {
    case VarExp(_, v) => (env, v)
    case AssignOpApp(info, left, op, right) =>
      left match {
        case VarRef(info, Id(_, lhs)) =>
          if (env.contains(lhs)) {
            (env(lhs), walk(env, right)) match {
              case (Var(StoRef(), _), (nenv: Env, Var(StoRef(), init))) =>
                (nenv += (lhs -> Var(StoRef(), init)), Var(StoRef(), init))
              case (Var(StoRef(), _), (nenv: Env, Var(Storage(), _))) =>
                (nenv += (lhs -> Var(StoRef(), true)), Var(StoRef(), true))
              case (Var(StoRef(), _), (nenv: Env, Var(Memory(), _))) =>
                (nenv += (lhs -> Var(StoRef(), true)), Var(StoRef(), true))
              case (_, (nenv: Env, _)) => (nenv, Var(Memory(), true))
            }
          } else if (getThis.env.contains(lhs)) {
            svuCheck(walk(env, left)._2)
            walk(env, right)
          } else {
            walk(env, right)
            (env, Var(Memory(), true))
          }
        case TupleLiteral(_, elems) =>
          val aw = walk(env, right)
          aw._2 match {
            case VarT(tup) =>
              elems.zip(tup).foldLeft((aw._1, Var(Memory(), true))) { (ee, et) =>
                val ret = walk(ee._1, AssignOpApp(info, et._1, op, VarExp(info, et._2)))
                (ret._1, getVar(ret._2))
              }
            case _ => val ret = walk(aw._1, left); svuCheck(ret._2); ret
          }
        case _ =>
          val ret = walk(env, left)
          svuCheck(ret._2)
          walk(ret._1, right)
      }
    case Cond(info, cond, trueE, falseE) =>
      walk(walk(walk(env, cond)._1, trueE)._1, falseE)
    case InfixOpApp(info, left, op, right) =>
      walk(walk(env, left)._1, right)
    case PrefixOpApp(info, op, right) =>
      op.name match {
        case "++" | "--" | "delete" =>
          val ret = walk(env, right)
          svuCheck(ret._2); ret
        case _ => walk(env, right)
      }
    case UnaryAssignOpApp(info, lhs, op) =>
      val ret = walk(env, lhs)
      svuCheck(ret._2); ret
    case FunctionCall(info, ftn, args) =>
      val ar = args.foldLeft((env, List[Var]())) { (elv, arg) =>
        {
          val ret = walk(elv._1, arg)
          (ret._1, elv._2 :+ getVar(ret._2))
        }
      }
      ftn match {
        case VarRef(_, Id(_, n)) =>
          val ftns = howmany(n, args.length)
          ftns.length match {
            case 0 => (env, Var(Memory(), true))
            case 1 =>
              ftns(0) match {
                case F(id, mod, param, res, stmt, comment) =>
                  if (!fchain.exists { _ == (id.get, param.length) }) {
                    val prev = cf; cf = id.get; addChain(ftns(0)); retv = None; val st = (ilflag, mflag)
                    var nenv1 = param.zip(ar._2).foldLeft(HashMap[String, Var]()) { (hm, pa) =>
                      {
                        if (!pa._1.id.isEmpty)
                          hm += (pa._1.id.get -> Var(pa._1.loc, pa._2.init))
                        else
                          hm
                      }
                    }
                    nenv1 ++= makeEnvR(res)
                    val nenv2 = stmt.foldLeft(nenv1) { (env, s) => walk(env, s) }
                    remvChain(ftns(0)); cf = prev; ilflag = st._1; mflag = st._2
                    if (!retv.isEmpty) {
                      val retvg = retv.get; retv = None
                      (ar._1, retvg)
                    } else {
                      res.length match {
                        case 0 => (ar._1, Var(Memory(), true))
                        case 1 =>
                          if (!res(0).id.isEmpty && nenv2.contains(res(0).id.get))
                            (ar._1, Var(res(0).loc, nenv2(res(0).id.get).init))
                          else
                            (ar._1, Var(Memory(), true))
                        case _ =>
                          (ar._1, VarT(res.foldLeft(List[Var]()) { (lv, rr) =>
                            {
                              if (!rr.id.isEmpty && nenv2.contains(rr.id.get))
                                lv :+ Var(rr.loc, nenv2(rr.id.get).init)
                              else
                                lv :+ Var(Memory(), true)
                            }
                          }))
                      }
                    }
                  } else
                    (env, Var(Memory(), true))
                case _ => (env, Var(Memory(), true))
              }
            case _ => (env, Var(Memory(), true))
          }
        case _ =>
          walk(env, ftn)
          (env, Var(Memory(), true))
      }

    case Bracket(info, ftn, args) =>
      args match {
        case Some(e) => walk(walk(env, e)._1, ftn)
        case None => walk(env, ftn)
      }
    case Dot(info, base, name) => walk(env, base)
    case VarRef(info, name) =>
      if (env.contains(name.name)) {
        env(name.name) match {
          case Var(StoRef(), false) =>
            usp = usp :+ ("[" + cur + " -> " + cf + " -> " + name.name + "] ")
            update(List(1)); csvu = name.name
          case Var(StoRef(), true) => csvu = name.name
          case _ => csvu = ""
        }
        (env, env(name.name))
      } else if (getThis.env.contains(name.name)) {
        csvu = name.name
        if (mflag) {
          var modl = modsv(cur)
          modl = modl :+ csvu
          modsv += (cur -> modl)
        }
        (env, getThis.env(name.name))
      } else {
        csvu = ""
        (env, Var(Memory(), true))
      }
    case TupleLiteral(info, elems) =>
      val tup = elems.foldLeft((env, List[Var]())) { (elv, el) =>
        {
          val ret = walk(elv._1, el)
          (ret._1, elv._2 :+ getVar(ret._2))
        }
      }
      (tup._1, VarT(tup._2))
    case TupleOptLiteral(info, elems) =>
      val tup = elems.foldLeft((env, List[Var]())) { (elv, el) =>
        {
          if (!el.isEmpty) {
            val ret = walk(elv._1, el.get)
            (ret._1, elv._2 :+ getVar(ret._2))
          } else
            (elv._1, elv._2 :+ Var(Memory(), true))
        }
      }
      (tup._1, VarT(tup._2))
    case _ => (env, Var(Memory(), true))
  }
  def walk(node: AssemblyItem): Unit = node match {
    case AssemblyBlock(info, body) => body.foreach(walk(_))
    case AssemblyLetX(info, name, body) => walk(body)
    case AssemblyLet(info, name, body) => if (!body.isEmpty) walk(body.get)
    case AssemblyIf(info, cond, body) =>
      walk(cond); body.foreach(walk(_))
    case AssemblySwitch(info, cond, cases, default) =>
      walk(cond)
      cases.foreach(walk(_))
      default.foreach(walk(_))
    case AssemblyFtn(info, name, param, body) => body.foreach(walk(_))
    case AssemblyFor(info, init, change, cond, body) =>
      init.foreach(walk(_))
      walk(change)
      cond.foreach(walk(_))
      body.foreach(walk(_))
    case AssemblyAssignX(info, name, body) => walk(body)
    case AssemblyAssign(info, name, body) => if (!body.isEmpty) walk(body.get)
    case SubAssembly(info, name, body) => body.foreach(walk(_))
    case f: FtnAssembly => walk(f)
    case AssemblyExprStmt(info, body) => walk(body)
    case a: AssemblyExpression => walk(a)
    case _ =>
  }
  def walk(node: FtnAssembly): Unit = node match {
    case FtnAssembly(info, name, body) => body.foreach(walk(_))
  }
  def walk(node: AssemblyExpression): Unit = node match {
    case AssemblyCall(info, name, args) =>
      (ctx, name.name) match {
        case (Ctx("C", "F", _), "delegatecall") => update(List(17, 18, 31, 32))
        case (Ctx("C", "F", _), "callcode") => update(List(19, 20, 33, 34))
        case (Ctx("C", "F", _), "mstore") => update(List(21, 35))
        case (Ctx("C", "M", _), "delegatecall") => update(List(24, 25, 31, 32))
        case (Ctx("C", "M", _), "callcode") => update(List(26, 27, 34, 35))
        case (Ctx("C", "M", _), "mstore") => update(List(28, 35))
        case _ =>
      }
    case _ =>
  }
  def walk(node: AssemblyCase): Unit = node match {
    case AssemblyCase(info, cond, body) =>
      walk(cond); body.foreach(walk(_))
  }
}
