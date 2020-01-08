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
import scala.collection.immutable.{ HashMap, HashSet }

object VectorGenerator {
  val relevants: List[String] = List("StateVariableDeclaration", "FunctionDefinition", "EventDefinition", "EnumDefinition", "VariableDeclaration", "Parameter", "EventParameter", "FtnDefStmt", "If", "While", "For", "ABlock", "InlineAssembly", "DoWhile", "Continue", "Break", "Return", "Throw", "Emit", "Underscore", "SimpleVarX", "SimpleVar", "VarDecl", "ExprStmt", "AssemblyBlock", "AssemblyLetX", "AssemblyLet", "AssemblyIf", "AssemblySwitch", "AssemblyFtn", "AssemblyFor", "AssemblyAssignX", "AssemblyAssign", "AssemblyLabel", "AssemblyNumber", "AssemblyString", "AssemblyBreak", "AssemblyContinue", "SubAssembly", "AssemblyId", "AssemblyCall", "FtnAssembly", "AssemblyExprStmt", "AssemblyCase", "AssignOpApp", "Cond", "InfixOpApp", "PrefixOpApp", "UnaryAssignOpApp", "FunctionCall", "Bracket", "Dot", "NewCall", "New", "TypeRef", "VarRef", "ArrayLiteral", "TupleLiteral", "TupleOptLiteral", "Bool", "IntLiteral", "DoubleLiteral", "StringLiteral")
  val significants: List[String] = List("StateVariableDeclaration", "FunctionDefinition", "EventDefinition", "EnumDefinition", "VariableDeclaration", "Parameter", "EventParameter", "FtnDefStmt", "If", "While", "For", "ABlock", "InlineAssembly", "DoWhile", "Continue", "Break", "Return", "Throw", "Emit", "Underscore", "SimpleVarX", "SimpleVar", "VarDecl", "ExprStmt", "AssemblyBlock", "AssemblyLetX", "AssemblyLet", "AssemblyIf", "AssemblySwitch", "AssemblyFtn", "AssemblyFor", "AssemblyAssignX", "AssemblyAssign", "AssemblyLabel", "AssemblyNumber", "AssemblyString", "AssemblyBreak", "AssemblyContinue", "SubAssembly", "AssemblyId", "AssemblyCall", "FtnAssembly", "AssemblyExprStmt", "AssemblyCase", "AssignOpApp", "Cond", "InfixOpApp", "PrefixOpApp", "UnaryAssignOpApp", "FunctionCall", "Bracket", "Dot", "NewCall", "New", "TypeRef", "VarRef", "ArrayLiteral", "TupleLiteral", "TupleOptLiteral", "Bool", "IntLiteral", "DoubleLiteral", "StringLiteral")
  val size_of_vector: Int = relevants.size
  val minT: Int = 40

  def name(n: ASTNode): String = n.getClass.getSimpleName
  def contains(s: List[String], n: ASTNode): Boolean = s.contains(name(n))

  def generate(n: ASTNode): List[Node] = {
    val nn = Node.toNode(n)
    var m = HashMap.empty[Node, Vector]
    var mergable = HashSet.empty[Node]
    var serializedTree = List.empty[Node]

    def collect(v: Vector, n: Node): Vector = {
      // V_n <- SUM_{n in children(N)} V_n
      val v_n = (v /: n.children)(collect)

      // update current node vector if it is a relevant node.
      val v_u =
        if (contains(relevants, n.n)) {
          val v_u = v_n.update(n.n)
          m += n -> v_u
          v_u
        } else v_n

      if (contains(significants, n.n) && v_u.containsEnoughTokens) {
        mergable += n
      }

      v_u
    }
    def serialize(n: Node): Unit = {
      if (mergable.contains(n))
        serializedTree ::= n
      n.children.foreach(serialize)
    }

    collect(Vector.empty, nn)
    serialize(nn)
    serializedTree = serializedTree.reverse
    // TODO stride

    serializedTree.map(n => n.copy(vector = m.getOrElse(n, Vector.empty)))
  }

  def toString(n: Node): String = {
    val span = n.n.info.span
    val lineno = span.begin.line
    val filename = span.fileName
    val end = span.end.line
    val kind = n.vector.node_kind
    val num_node = n.vector.count
    s"# FILE:$filename, LINE:$lineno, OFFSET:$end, NODE_KIND:$kind, CONTEXT_KIND:0, NEIGHBOR_KIND:0, NUM_NODE:$num_node, NUM_DECL:0, NUM_STMT:0, NUM_EXPR:0,\n${n.vector.toString}"
  }
}
