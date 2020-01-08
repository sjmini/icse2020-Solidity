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

package kr.ac.kaist.saf.nodes.ast

import kr.ac.kaist.saf.util.{ NodeUtil => NU }
import kr.ac.kaist.saf.LINE_SEP
import java.lang.Double
import java.math.BigInteger

case class SourceUnit(
    info: ASTNodeInfo,
    body: List[SourceElement]
) extends ASTNode {
  override def toString(indent: Int): String = {
    NU.initNodesPrint
    val s: StringBuilder = new StringBuilder
    s.append(NU.join(
      indent + 1,
      body,
      LINE_SEP + NU.getIndent(indent + 1),
      new StringBuilder("")
    ))
    s.toString
  }
}
object SourceUnit {
  def apply(info: ASTNodeInfo, body: List[SourceElement]): SourceUnit =
    SourceUnit(info, body)
}

trait SourceElement extends ASTNode

case class PragmaDirective(
    info: ASTNodeInfo,
    name: Id
) extends SourceElement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("pragma ")
      .append(name.toString(indent))
      .append(" ^0.4.11;") // Ignore the version number
    s.toString
  }
}

trait ImportDirective extends SourceElement

case class Import(
    info: ASTNodeInfo,
    alias: Alias
) extends ImportDirective {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("import ")
      .append(alias.toString(indent))
      .append(";")
    s.toString
  }
}

case class ImportAll(
    info: ASTNodeInfo,
    to: Option[Id],
    module: String
) extends ImportDirective {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("import * ")
    to.map(e => s.append(" as ").append(e.toString(indent)))
    s.append(" from ")
      .append(module)
      .append(";")
    s.toString
  }
}

case class ImportFrom(
    info: ASTNodeInfo,
    aliases: List[Alias],
    from: String
) extends ImportDirective {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("import {")
      .append(NU.join(
        indent,
        aliases,
        ", ",
        new StringBuilder("")
      ))
      .append("} from ")
      .append(from)
      .append(";")
    s.toString
  }
}

case class Alias(
    info: ASTNodeInfo,
    from: Id,
    to: Option[Id]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(from.toString(indent))
    to.map(e => s.append(" as ").append(e.toString(indent)))
    s.toString
  }
}

case class ContractDefinition(
    info: ASTNodeInfo,
    kind: DefinitionKind,
    name: Id,
    supers: List[InheritanceSpecifier],
    body: List[ContractPart]
) extends SourceElement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(kind.toString(indent))
      .append(" ")
      .append(name.toString(indent))
    if (!supers.isEmpty) s.append(" is ")
    s.append(NU.join(
      indent,
      supers,
      ", ",
      new StringBuilder("")
    )).append("{\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

trait DefinitionKind extends ASTNode

case class ContractKind(
    info: ASTNodeInfo
) extends DefinitionKind {
  override def toString(indent: Int): String = "contract"
}

case class LibraryKind(
    info: ASTNodeInfo
) extends DefinitionKind {
  override def toString(indent: Int): String = "library"
}

case class InterfaceKind(
    info: ASTNodeInfo
) extends DefinitionKind {
  override def toString(indent: Int): String = "interface"
}

case class InheritanceSpecifier(
    info: ASTNodeInfo,
    name: TypeName,
    args: List[Expression]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
      .append("(")
      .append(NU.join(
        indent,
        args,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

trait ContractPart extends ASTNode

case class StateVariableDeclaration(
    info: ASTNodeInfo,
    typ: TypeName,
    mods: List[Mod],
    name: Id,
    body: Option[Expression]
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(typ.toString(indent))
      .append(" ")
      .append(NU.join(
        indent,
        mods,
        " ",
        new StringBuilder("")
      ))
      .append(" ").append(name.toString(indent))
    body.map(e => s.append(" = ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }
}

case class UsingForDeclaration(
    info: ASTNodeInfo,
    name: Id,
    usingfor: String
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("using ")
      .append(name.toString(indent))
      .append(" for ")
      .append(usingfor)
      .append(";")
    s.toString
  }
}

case class StructDefinition(
    info: ASTNodeInfo,
    name: Id,
    vds: List[VariableDeclaration]
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("struct ")
      .append(name.toString(indent))
      .append(" {\n")
      .append(NU.join(
        indent,
        vds,
        ";" + LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(";}\n")
    s.toString
  }
}

case class ModifierDefinition(
    info: ASTNodeInfo,
    name: Id,
    params: List[Parameter],
    body: List[Statement]
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("modifier ")
      .append(name.toString(indent)).append("(")
      .append(NU.join(
        indent,
        params,
        ", ",
        new StringBuilder("")
      ))
      .append(") {")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
    s.toString
  }
}

case class FunctionDefinition(
    info: ASTNodeInfo,
    name: Option[Id],
    params: List[Parameter],
    mods: List[FtnMod],
    result: List[Parameter],
    body: List[Statement],
    comment: String
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    if (comment != "")
      s.append(comment).append("\n")
    if (name.isEmpty)
      s.append("function ")
    else {
      if (name.get.name == "constructor")
        s.append("constructor ")
      else {
        s.append("function ")
        s.append(name.get.toString(indent))
      }
    }
    s.append("(")
      .append(NU.join(
        indent,
        params,
        ", ",
        new StringBuilder("")
      ))
      .append(") ")
      .append(NU.join(
        indent,
        mods,
        " ",
        new StringBuilder("")
      ))
      .append(" returns (")
      .append(NU.join(
        indent,
        result,
        ", ",
        new StringBuilder("")
      ))
      .append(") {\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

// Ignore (w anonymous)?
case class EventDefinition(
    info: ASTNodeInfo,
    name: Id,
    params: List[EventParameter]
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("event ")
      .append(name.toString(indent))
      .append("(")
      .append(NU.join(
        indent,
        params,
        ", ",
        new StringBuilder("")
      ))
      .append(");")
    s.toString
  }
}

case class EnumDefinition(
    info: ASTNodeInfo,
    name: Id,
    vals: List[Id]
) extends ContractPart {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("enum ")
      .append(name.toString(indent))
      .append("{")
      .append(NU.join(
        indent,
        vals,
        ", ",
        new StringBuilder("")
      ))
      .append("}")
    s.toString
  }
}

case class VariableDeclaration(
    info: ASTNodeInfo,
    typ: TypeName,
    loc: Option[StorageLocation],
    name: Id
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(typ.toString(indent))
    loc.map(e => s.append(" ").append(e.toString(indent)))
    s.append(" ").append(name.toString(indent))
    s.toString
  }
}

case class Parameter(
    info: ASTNodeInfo,
    typ: TypeName,
    loc: Option[StorageLocation],
    name: Option[Id]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(typ.toString(indent))
    loc.map(e => s.append(" ").append(e.toString(indent)))
    name.map(e => s.append(" ").append(e.toString(indent)))
    s.toString
  }
}

// Ignore (w indexed)?
case class EventParameter(
    info: ASTNodeInfo,
    typ: TypeName,
    name: Option[Id]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(typ.toString(indent))
    name.map(e => s.append(" ").append(e.toString(indent)))
    s.toString
  }
}

trait FtnMod extends ASTNode

case class ModifierInvocation(
    info: ASTNodeInfo,
    name: Id,
    args: List[Expression]
) extends FtnMod {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
      .append("(")
      .append(NU.join(
        indent,
        args,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

trait StateMutability extends FtnMod

case class Payable(
    info: ASTNodeInfo
) extends Mod with StateMutability with StorageLocation {
  override def toString(indent: Int): String = "payable"
}

case class Pure(
    info: ASTNodeInfo
) extends StateMutability {
  override def toString(indent: Int): String = "pure"
}

case class View(
    info: ASTNodeInfo
) extends StateMutability {
  override def toString(indent: Int): String = "view"
}

trait Mod extends ASTNode

case class Constant(
    info: ASTNodeInfo
) extends Mod with StateMutability {
  override def toString(indent: Int): String = "constant"
}

case class External(
    info: ASTNodeInfo
) extends Mod with FtnMod {
  override def toString(indent: Int): String = "external"
}

case class Internal(
    info: ASTNodeInfo
) extends Mod with FtnMod {
  override def toString(indent: Int): String = "internal"
}

case class Private(
    info: ASTNodeInfo
) extends Mod with FtnMod {
  override def toString(indent: Int): String = "private"
}

case class Public(
    info: ASTNodeInfo
) extends Mod with FtnMod {
  override def toString(indent: Int): String = "public"
}

trait Statement extends ASTNode

case class FtnDefStmt(
    info: ASTNodeInfo,
    name: Option[Id],
    params: List[Parameter],
    mods: List[FtnMod],
    result: List[Parameter],
    body: Statement
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("function ")
    name.map(e => s.append(e.toString(indent)))
    s.append("(")
      .append(NU.join(
        indent,
        params,
        ", ",
        new StringBuilder("")
      ))
      .append(") ")
      .append(NU.join(
        indent,
        mods,
        " ",
        new StringBuilder("")
      ))
      .append(" returns (")
      .append(NU.join(
        indent,
        result,
        ", ",
        new StringBuilder("")
      ))
      .append(") = ")
      .append(body.toString(indent))
    s.toString
  }
}

case class If(
    info: ASTNodeInfo,
    cond: Expression,
    trueB: Statement,
    falseB: Option[Statement]
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("if (")
      .append(cond.toString(indent))
      .append(") ")
      .append(trueB.toString(indent))
    falseB.map(e => s.append(" else ").append(e.toString(indent)))
    s.toString
  }
}

case class While(
    info: ASTNodeInfo,
    cond: Expression,
    body: Statement
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("while (")
      .append(cond.toString(indent))
      .append(") ")
      .append(body.toString(indent))
    s.toString
  }
}

case class For(
    info: ASTNodeInfo,
    init: Option[SimpleStatement],
    test: Option[Expression],
    update: Option[Statement],
    body: Statement
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("for (")
    init match {
      case Some(e) => s.append(e.toString(indent))
      case _ => s.append(";")
    }
    s.append(" ")
    test.map(e => s.append(e.toString(indent)))
    s.append("; ")
    update.map(e => s.append(e.toString(indent)))
    s.append(") ").append(body.toString(indent))
    s.toString
  }
}

case class ABlock(
    info: ASTNodeInfo,
    body: List[Statement]
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("{\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
    s.append("}\n")
    s.toString
  }
}

case class InlineAssembly(
    info: ASTNodeInfo,
    name: Option[String],
    body: List[AssemblyItem]
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("assembly ")
    name.map(e => s.append(e))
    s.append("{\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

case class DoWhile(
    info: ASTNodeInfo,
    body: Statement,
    cond: Expression
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("do ")
      .append(body.toString(indent))
      .append(" while (")
      .append(cond.toString(indent))
      .append(");")
    s.toString
  }
}

case class Continue(
    info: ASTNodeInfo
) extends Statement {
  override def toString(indent: Int): String = "continue;"
}

case class Break(
    info: ASTNodeInfo
) extends Statement {
  override def toString(indent: Int): String = "break;"
}

case class Return(
    info: ASTNodeInfo,
    result: Option[Expression]
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("return")
    result.map(e => s.append(" ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }
}

case class Throw(
    info: ASTNodeInfo
) extends Statement {
  override def toString(indent: Int): String = "throw;"
}

case class Emit(
    info: ASTNodeInfo,
    emit: Expression
) extends Statement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("emit ").append(emit.toString(indent)).append(";")
    s.toString
  }
}

case class Underscore(
    info: ASTNodeInfo
) extends Statement {
  override def toString(indent: Int): String = "_;"
}

trait SimpleStatement extends Statement

case class SimpleVarX(
    info: ASTNodeInfo,
    name: List[Option[Id]],
    body: Expression
) extends SimpleStatement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("var (")
    name.foreach(e => s.append(e.fold("") {
      _.toString(indent)
    }).append(", "))
    s.append(") = ").append(body.toString(indent)).append(";")
    s.toString
  }
}

case class SimpleVar(
    info: ASTNodeInfo,
    name: Id,
    body: Option[Expression]
) extends SimpleStatement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("var ").append(name.toString(indent))
    body.map(e => s.append(" = ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }
}

case class VarDecl(
    info: ASTNodeInfo,
    dec: List[VariableDeclaration],
    body: Option[Expression]
) extends SimpleStatement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    dec match {
      case List(d) =>
        s.append(d.toString(indent))
      case _ =>
        s.append("(")
          .append(NU.join(
            indent,
            dec,
            ", ",
            new StringBuilder("")
          ))
          .append(")")
    }
    body.map(e => s.append(" = ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }
}

case class ExprStmt(
    info: ASTNodeInfo,
    body: Expression
) extends SimpleStatement {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(body.toString(indent))
      .append(";")
    s.toString
  }
}

trait AssemblyItem extends ASTNode

case class AssemblyBlock(
    info: ASTNodeInfo,
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("{\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

case class AssemblyLetX(
    info: ASTNodeInfo,
    name: List[Id],
    body: AssemblyExpression
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("let (")
      .append(NU.join(
        indent,
        name,
        ", ",
        new StringBuilder("")
      ))
      .append(") := ")
    s.append(body.toString(indent))
    s.toString
  }
}

case class AssemblyLet(
    info: ASTNodeInfo,
    name: List[Id],
    body: Option[FtnAssembly]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("let (")
      .append(NU.join(
        indent,
        name,
        ", ",
        new StringBuilder("")
      ))
      .append(") := ")
    body.map(e => s.append(e.toString(indent)))
    s.toString
  }
}

case class AssemblyIf(
    info: ASTNodeInfo,
    cond: AssemblyExpression,
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("if ")
      .append(cond.toString(indent))
      .append(" {")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
    s.toString
  }
}

case class AssemblySwitch(
    info: ASTNodeInfo,
    cond: AssemblyExpression,
    cases: List[AssemblyCase],
    default: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("switch ")
      .append(cond.toString(indent))
      .append(" ")
      .append(NU.join(
        indent,
        cases,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("default {")
      .append(NU.join(
        indent,
        default,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
    s.toString
  }
}

// Ignore (w "->" w openparen w Id w closeparen)?
case class AssemblyFtn(
    info: ASTNodeInfo,
    name: Id,
    param: Option[Id],
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("function ").append(name.toString(indent))
    s.append("(")
    param.map(e => s.append(e.toString(indent)))
    s.append(") {\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

case class AssemblyFor(
    info: ASTNodeInfo,
    init: List[AssemblyItem],
    change: AssemblyExpression,
    cond: List[AssemblyItem],
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("for {")
      .append(NU.join(
        indent,
        init,
        " ",
        new StringBuilder("")
      ))
      .append("} ")
      .append(change.toString(indent))
      .append(" {")
      .append(NU.join(
        indent,
        cond,
        " ",
        new StringBuilder("")
      ))
      .append("} {")
      .append(NU.join(
        indent,
        body,
        " ",
        new StringBuilder("")
      ))
    s.append("}")
    s.toString
  }
}

case class AssemblyAssignX(
    info: ASTNodeInfo,
    name: Id,
    body: AssemblyExpression
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
      .append(" := ").append(body.toString(indent))
    s.toString
  }
}

case class AssemblyAssign(
    info: ASTNodeInfo,
    name: Id,
    body: Option[FtnAssembly]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    body match {
      case Some(fa) =>
        s.append(name.toString(indent))
          .append(" := ").append(fa.toString(indent))
      case _ =>
        s.append("=:").append(name.toString(indent))
    }
    s.toString
  }
}

case class AssemblyLabel(
    info: ASTNodeInfo,
    label: Id
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(label.toString(indent)).append(":")
    s.toString
  }
}

case class AssemblyNumber(
    info: ASTNodeInfo,
    num: NumberLiteral
) extends AssemblyItem with AssemblyExpression {
  override def toString(indent: Int): String = num.toString(indent)
}

case class AssemblyString(
    info: ASTNodeInfo,
    str: StringLiteral
) extends AssemblyItem with AssemblyExpression {
  override def toString(indent: Int): String = str.toString(indent)
}

case class AssemblyBreak(
    info: ASTNodeInfo
) extends AssemblyItem {
  override def toString(indent: Int): String = "break"
}

case class AssemblyContinue(
    info: ASTNodeInfo
) extends AssemblyItem {
  override def toString(indent: Int): String = "continue"
}

case class SubAssembly(
    info: ASTNodeInfo,
    name: Id,
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("assembly ").append(name.toString(indent))
    s.append(" {\n")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}\n")
    s.toString
  }
}

case class AssemblyId(
    info: ASTNodeInfo,
    name: Id
) extends AssemblyItem with AssemblyExpression {
  override def toString(indent: Int): String = name.toString(indent)
}

case class AssemblyCase(
    info: ASTNodeInfo,
    cond: AssemblyExpression,
    body: List[AssemblyItem]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("case ").append(cond.toString(indent))
    s.append(" {")
      .append(NU.join(
        indent,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append("}")
    s.toString
  }
}

trait AssemblyExpression extends AssemblyItem

case class AssemblyCall(
    info: ASTNodeInfo,
    name: Id,
    args: List[AssemblyExpression]
) extends AssemblyExpression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
    s.append("(")
      .append(NU.join(
        indent,
        args,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

case class FtnAssembly(
    info: ASTNodeInfo,
    name: Id,
    body: List[AssemblyItem]
) extends AssemblyItem {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
    s.append("(")
      .append(NU.join(
        indent,
        body,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

case class AssemblyExprStmt(
    info: ASTNodeInfo,
    body: AssemblyExpression
) extends AssemblyItem {
  override def toString(indent: Int): String = body.toString(indent)
}

trait Expression extends ASTNode

case class AssignOpApp(
    info: ASTNodeInfo,
    left: Expression,
    op: Op,
    right: Expression
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(left.toString(indent))
      .append(" ")
      .append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }
}

case class Cond(
    info: ASTNodeInfo,
    cond: Expression,
    trueE: Expression,
    falseE: Expression
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(cond.toString(indent))
      .append(" ? ")
      .append(trueE.toString(indent))
      .append(" : ")
      .append(falseE.toString(indent))
    s.toString
  }
}

case class InfixOpApp(
    info: ASTNodeInfo,
    left: Expression,
    op: Op,
    right: Expression
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(left.toString(indent))
      .append(" ")
      .append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }
}

case class PrefixOpApp(
    info: ASTNodeInfo,
    op: Op,
    right: Expression
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }
}

case class UnaryAssignOpApp(
    info: ASTNodeInfo,
    lhs: Expression,
    op: Op
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(lhs.toString(indent))
      .append(" ")
      .append(op.toString(indent))
    s.toString
  }
}

// Ignore FunctionCallArguments = '{' NameValueList? '}'
// Ignore NameValuePair ::= Id ':' Expression
case class FunctionCall(
    info: ASTNodeInfo,
    ftn: Expression,
    args: List[Expression]
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(ftn.toString(indent))
      .append("(")
      .append(NU.join(
        indent,
        args,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

case class Bracket(
    info: ASTNodeInfo,
    ftn: Expression,
    args: Option[Expression]
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(ftn.toString(indent))
      .append("[")
    args.map(e => s.append(e.toString(indent)))
    s.append("]")
    s.toString
  }
}

case class Dot(
    info: ASTNodeInfo,
    base: Expression,
    name: Id
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(base.toString(indent))
      .append(".")
      .append(name.toString(indent))
    s.toString
  }
}

case class NewCall(
    info: ASTNodeInfo,
    call: Expression
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("new ")
      .append(call.toString(indent))
    s.toString
  }
}

case class New(
    info: ASTNodeInfo,
    name: TypeName
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("new ")
      .append(name.toString(indent))
    s.toString
  }
}

case class TypeRef(
    info: ASTNodeInfo,
    name: TypeName
) extends Expression {
  override def toString(indent: Int): String = name.toString(indent)
}

case class VarRef(
    info: ASTNodeInfo,
    name: Id
) extends Expression {
  override def toString(indent: Int): String = name.toString(indent)
}

case class ArrayLiteral(
    info: ASTNodeInfo,
    elems: List[Expression]
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("[")
      .append(NU.join(
        indent,
        elems,
        ", ",
        new StringBuilder("")
      ))
      .append("]")
    s.toString
  }
}

case class TupleLiteral(
    info: ASTNodeInfo,
    elems: List[Expression]
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("(")
      .append(NU.join(
        indent,
        elems,
        ", ",
        new StringBuilder("")
      ))
      .append(")")
    s.toString
  }
}

case class TupleOptLiteral(
    info: ASTNodeInfo,
    elems: List[Option[Expression]]
) extends Expression {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("(")
    elems.foreach(e => s.append(e.fold("") {
      _.toString(indent)
    }).append(", "))
    s.append(")")
    s.toString
  }
}

trait Literal extends Expression

case class Bool(
    info: ASTNodeInfo,
    bool: Boolean
) extends Literal {
  override def toString(indent: Int): String = bool.toString
}

trait NumberLiteral extends Literal

case class IntLiteral(
    info: ASTNodeInfo,
    intVal: BigInteger,
    radix: Integer
) extends NumberLiteral {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(radix.toInt match {
      case 8 => "0" + intVal.toString(8)
      case 16 => "0x" + intVal.toString(16)
      case _ => intVal.toString
    })
    s.toString
  }
}

case class DoubleLiteral(
    info: ASTNodeInfo,
    text: String,
    num: Double
) extends NumberLiteral {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(text)
    s.toString
  }
}

case class StringLiteral(
    info: ASTNodeInfo,
    quote: String,
    str: String
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(quote)
    NU.ppAST(s, str)
    s.append(quote)
    s.toString
  }
}

case class Id(
    info: ASTNodeInfo,
    name: String
) extends ASTNode {
  override def toString(indent: Int): String = name
}

case class Op(
    info: ASTNodeInfo,
    name: String
) extends ASTNode {
  override def toString(indent: Int): String = name
}

trait TypeName extends ASTNode

// Ignore (w Args)?
case class ArrayTypeName(
    info: ASTNodeInfo,
    name: TypeName,
    size: Option[Expression]
) extends TypeName {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
      .append("[")
    size.map(e => s.append(e.toString(indent)))
    s.append("]")
    s.toString
  }
}

case class ElementaryTypeName(
    info: ASTNodeInfo,
    typ: String
) extends TypeName {
  override def toString(indent: Int): String = typ
}

case class UserDefinedTypeName(
    info: ASTNodeInfo,
    names: List[Id]
) extends TypeName {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(NU.join(
      indent,
      names,
      ".",
      new StringBuilder("")
    ))
    s.toString
  }
}

case class Mapping(
    info: ASTNodeInfo,
    domain: String,
    range: TypeName
) extends TypeName {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("mapping (")
      .append(domain)
      .append(" => ")
      .append(range.toString(indent))
      .append(")")
    s.toString
  }
}

// Ignore w FunctionTypeParameterList
//        (w internal | w external | w StateMutability)*
//        (w returns w FunctionTypeParameterList)?
case class FunctionTypeName(
    info: ASTNodeInfo
) extends TypeName {
  override def toString(indent: Int): String = "function ()"
}

trait StorageLocation {
  def toString(indent: Int): String
}
case class Calldata() extends StorageLocation {
  def toString(indent: Int): String = "calldata"
}
case class Memory() extends StorageLocation {
  def toString(indent: Int): String = "memory"
}
case class Storage() extends StorageLocation {
  def toString(indent: Int): String = "storage"
}
