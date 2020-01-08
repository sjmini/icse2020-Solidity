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

package kr.ac.kaist.saf.clone_detector

import kr.ac.kaist.saf.nodes.ast._

case class Node(n: ASTNode, children: List[Node] = List.empty[Node], vector: Vector = Vector.empty)

object Node {
  def toNodes(ns: List[ASTNode]): List[Node] = ns.map(toNode)
  def toNodes(n: Option[ASTNode]): List[Node] = n.toList.map(toNode)
  def toNode(n: ASTNode): Node = {
    n match {
      case SourceUnit(_, body) => Node(n, toNodes(body))
      case ContractDefinition(_, _, _, specifiers, body) => Node(n, toNodes(specifiers) ++ toNodes(body))
      case InheritanceSpecifier(_, name, args) => Node(n, toNode(name) :: toNodes(args))
      case StateVariableDeclaration(_, typ, mods, _, obody) => Node(n, toNode(typ) :: toNodes(mods) ++ toNodes(obody))
      case StructDefinition(_, _, vds) => Node(n, toNodes(vds))
      case ModifierDefinition(_, _, params, body) => Node(n, toNodes(params) ++ toNodes(body))
      case FunctionDefinition(_, _, params, mods, result, body, comment) => Node(n, toNodes(params) ++ toNodes(mods) ++ toNodes(result) ++ toNodes(body))
      case EventDefinition(_, _, params) => Node(n, toNodes(params))
      case ModifierInvocation(_, _, args) => Node(n, toNodes(args))
      case FtnDefStmt(_, _, params, mods, result, body) => Node(n, toNodes(params) ++ toNodes(mods) ++ toNodes(result) :+ toNode(body))
      case If(_, c, t, b) => Node(n, toNode(c) :: toNode(t) :: toNodes(b))
      case While(_, c, b) => Node(n, toNode(c) :: toNode(b) :: Nil)
      case For(_, i, t, u, b) => Node(n, toNodes(i) ++ toNodes(t) ++ toNodes(u) :+ toNode(b))
      case ABlock(_, bs) => Node(n, toNodes(bs))
      case InlineAssembly(_, _, bs) => Node(n, toNodes(bs))
      case DoWhile(_, bs, c) => Node(n, toNode(bs) :: toNode(c) :: Nil)
      case Return(_, e) => Node(n, toNodes(e))
      case Emit(_, e) => Node(n, toNode(e) :: Nil)
      case SimpleVarX(_, _, bs) => Node(n, toNode(bs) :: Nil)
      case SimpleVar(_, _, bs) => Node(n, toNodes(bs))
      case VarDecl(_, ds, bs) => Node(n, toNodes(ds) ++ toNodes(bs))
      case ExprStmt(_, e) => Node(n, toNode(e) :: Nil)
      case AssemblyBlock(_, bs) => Node(n, toNodes(bs))
      case AssemblyLetX(_, _, e) => Node(n, toNode(e) :: Nil)
      case AssemblyLet(_, _, b) => Node(n, toNodes(b))
      case AssemblyIf(_, c, bs) => Node(n, toNode(c) :: toNodes(bs))
      case AssemblySwitch(_, c, cases, default) => Node(n, toNode(c) :: toNodes(cases) ++ toNodes(default))
      case AssemblyFtn(_, _, _, bs) => Node(n, toNodes(bs))
      case AssemblyFor(_, i, change, cs, bs) =>
        val v0: List[Node] = toNodes(i) :+ toNode(change)
        val v1: List[Node] = toNodes(cs) ++ toNodes(bs)
        Node(n, v0 ++ v1)
      case AssemblyAssignX(_, _, b) => Node(n, toNode(b) :: Nil)
      case AssemblyAssign(_, _, b) => Node(n, toNodes(b))
      case SubAssembly(_, _, b) => Node(n, toNodes(b))
      case AssemblyCall(_, _, args) => Node(n, toNodes(args))
      case FtnAssembly(_, _, b) => Node(n, toNodes(b))
      case AssemblyExprStmt(_, b) => Node(n, toNode(b) :: Nil)
      case AssemblyCase(_, c, b) => Node(n, toNode(c) :: toNodes(b))
      case AssignOpApp(_, l, o, r) => Node(n, toNode(l) :: toNode(o) :: toNode(r) :: Nil)
      case Cond(_, c, t, f) => Node(n, toNode(c) :: toNode(t) :: toNode(f) :: Nil)
      case InfixOpApp(_, c, t, f) => Node(n, toNode(c) :: toNode(t) :: toNode(f) :: Nil)
      case PrefixOpApp(_, o, t) => Node(n, toNode(o) :: toNode(t) :: Nil)
      case UnaryAssignOpApp(_, lhs, o) => Node(n, toNode(lhs) :: toNode(o) :: Nil)
      case FunctionCall(_, ftn, args) => Node(n, toNode(ftn) :: toNodes(args))
      case Bracket(_, ftn, args) => Node(n, toNode(ftn) :: toNodes(args))
      case Dot(_, e, _) => Node(n, toNode(e) :: Nil)
      case NewCall(_, c) => Node(n, toNode(c) :: Nil)
      case ArrayLiteral(_, es) => Node(n, toNodes(es))
      case TupleLiteral(_, es) => Node(n, toNodes(es))
      case TupleOptLiteral(_, es) => Node(n, es.flatMap(toNodes))
      case _ => Node(n)
    }
  }
}

