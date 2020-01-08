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

trait ASTGeneralWalker[Result] {
  def join(args: Result*): Result

  def walkOpt(opt: Option[ASTNode]): List[Result] =
    opt.fold(List[Result]()) { n: ASTNode =>
      List(n match {
        case s: Statement => walk(s)
        case e: Expression => walk(e)
        case a: FtnAssembly => walk(a)
        case i: Id => walk(i)
      })
    }

  def walk(info: ASTNodeInfo): Result = join()

  def walk(node: ASTNode): Result = node match {
    case n: SourceUnit => walk(n)
    case n: SourceElement => walk(n)
    case n: Alias => walk(n)
    case n: DefinitionKind => walk(n)
    case n: InheritanceSpecifier => walk(n)
    case n: ContractPart => walk(n)
    case n: VariableDeclaration => walk(n)
    case n: Parameter => walk(n)
    case n: EventParameter => walk(n)
    case n: FtnMod => walk(n)
    case n: Mod => walk(n)
    case n: Statement => walk(n)
    case n: AssemblyItem => walk(n)
    case n: AssemblyCase => walk(n)
    case n: Expression => walk(n)
    case n: Id => walk(n)
    case n: Op => walk(n)
    case n: TypeName => walk(n)
  }

  def walk(node: SourceUnit): Result = node match {
    case SourceUnit(info, body) =>
      join(walk(info) :: body.map(walk): _*)
  }

  def walk(node: SourceElement): Result = node match {
    case PragmaDirective(info, name) =>
      join(walk(info), walk(name))
    case ContractDefinition(info, kind, name, supers, body) =>
      join(walk(info) :: walk(kind) :: walk(name) ::
        supers.map(walk) ++ body.map(walk): _*)
  }

  def walk(node: ImportDirective): Result = node match {
    case Import(info, alias) =>
      join(walk(info), walk(alias))
    case ImportAll(info, to, module) =>
      join(walk(info) :: walkOpt(to): _*)
    case ImportFrom(info, aliases, from) =>
      join(walk(info) :: aliases.map(walk): _*)
  }

  def walk(node: FtnMod): Result = node match {
    case ModifierInvocation(info, name, args) =>
      join(walk(info) :: walk(name) :: args.map(walk): _*)
    case m: StateMutability => walk(m)
    case External(info) => walk(info)
    case Internal(info) => walk(info)
    case Private(info) => walk(info)
    case Public(info) => walk(info)
  }

  def walk(node: StateMutability): Result = node match {
    case Payable(info) => walk(info)
    case Pure(info) => walk(info)
    case View(info) => walk(info)
    case Constant(info) => walk(info)
  }

  def walk(node: Mod): Result = node match {
    case Constant(info) => walk(info)
    case External(info) => walk(info)
    case Internal(info) => walk(info)
    case Private(info) => walk(info)
    case Public(info) => walk(info)
  }

  def walk(node: DefinitionKind): Result = node match {
    case ContractKind(info) => walk(info)
    case LibraryKind(info) => walk(info)
    case InterfaceKind(info) => walk(info)
  }

  def walk(node: ContractPart): Result = node match {
    case StateVariableDeclaration(info, typ, mods, name, body) =>
      join(walk(info) :: walk(typ) :: mods.map(walk) ++
        List(walk(name)) ++ walkOpt(body): _*)
    case UsingForDeclaration(info, name, usingfor) =>
      join(walk(info), walk(name))
    case StructDefinition(info, name, vds) =>
      join(walk(info) :: walk(name) :: vds.map(walk): _*)
    case ModifierDefinition(info, name, params, body) =>
      join(walk(info) :: walk(name) :: params.map(walk) ++
        body.map(walk): _*)
    case FunctionDefinition(info, name, params, mods, result, body, comment) =>
      join(walk(info) :: walkOpt(name) ++ params.map(walk) ++
        mods.map(walk) ++ result.map(walk) ++ body.map(walk): _*)
    case EventDefinition(info, name, params) =>
      join(walk(info) :: walk(name) :: params.map(walk): _*)
    case EnumDefinition(info, name, vals) =>
      join(walk(info) :: walk(name) :: vals.map(walk): _*)
  }

  def walk(node: Statement): Result = node match {
    case s: SimpleStatement => walk(s)
    case FtnDefStmt(info, name, params, mods, result, body) =>
      join(walk(info) :: walkOpt(name) ++ params.map(walk) ++
        mods.map(walk) ++ result.map(walk) ++ List(walk(body)): _*)
    case If(info, cond, trueB, falseB) =>
      join(walk(info) :: walk(cond) :: walk(trueB) :: walkOpt(falseB): _*)
    case While(info, cond, body) =>
      join(walk(info), walk(cond), walk(body))
    case For(info, init, test, update, body) =>
      join(walk(info) :: walkOpt(init) ++ walkOpt(test) ++
        walkOpt(update) ++ List(walk(body)): _*)
    case ABlock(info, body) =>
      join(walk(info) :: body.map(walk): _*)
    case InlineAssembly(info, name, body) =>
      join(walk(info) :: body.map(walk): _*)
    case DoWhile(info, body, cond) =>
      join(walk(info), walk(body), walk(cond))
    case Continue(info) => walk(info)
    case Break(info) => walk(info)
    case Return(info, result) => join(walk(info) :: walkOpt(result): _*)
    case Throw(info) => walk(info)
    case Emit(info, emit) => join(walk(info), walk(emit))
    case Underscore(info) => walk(info)
  }

  def walk(node: SimpleStatement): Result = node match {
    case SimpleVarX(info, name, body) =>
      join(walk(info) :: name.flatMap(walkOpt) ++ List(walk(body)): _*)
    case SimpleVar(info, name, body) =>
      join(walk(info) :: walk(name) :: walkOpt(body): _*)
    case VarDecl(info, dec, body) =>
      join(walk(info) :: dec.map(walk) ++ walkOpt(body): _*)
    case ExprStmt(info, body) =>
      join(walk(info), walk(body))
  }

  def walk(node: Expression): Result = node match {
    case AssignOpApp(info, left, op, right) =>
      join(walk(info), walk(left), walk(op), walk(right))
    case Cond(info, cond, trueE, falseE) =>
      join(walk(info), walk(cond), walk(trueE), walk(falseE))
    case InfixOpApp(info, left, op, right) =>
      join(walk(info), walk(left), walk(op), walk(right))
    case PrefixOpApp(info, op, right) =>
      join(walk(info), walk(op), walk(right))
    case UnaryAssignOpApp(info, lhs, op) =>
      join(walk(info), walk(lhs), walk(op))
    case FunctionCall(info, ftn, args) =>
      join(walk(info) :: walk(ftn) :: args.map(walk): _*)
    case Bracket(info, ftn, args) =>
      join(walk(info) :: walk(ftn) :: walkOpt(args): _*)
    case Dot(info, base, name) =>
      join(walk(info), walk(base), walk(name))
    case NewCall(info, call) =>
      join(walk(info), walk(call))
    case New(info, name) =>
      join(walk(info), walk(name))
    case TypeRef(info, name) =>
      join(walk(info), walk(name))
    case VarRef(info, name) =>
      join(walk(info), walk(name))
    case ArrayLiteral(info, elems) =>
      join(walk(info) :: elems.map(walk): _*)
    case TupleLiteral(info, elems) =>
      join(walk(info) :: elems.map(walk): _*)
    case TupleOptLiteral(info, elems) =>
      join(walk(info) :: elems.flatMap(walkOpt): _*)
    case l: Literal => walk(l)
  }

  def walk(node: Literal): Result = node match {
    case Bool(info, bool) => walk(info)
    case StringLiteral(info, quote, str) => walk(info)
    case n: NumberLiteral => walk(n)
  }

  def walk(node: NumberLiteral): Result = node match {
    case IntLiteral(info, intVal, radix) => walk(info)
    case DoubleLiteral(info, text, num) => walk(info)
  }

  def walk(node: AssemblyItem): Result = node match {
    case AssemblyBlock(info, body) =>
      join(walk(info) :: body.map(walk): _*)
    case AssemblyLetX(info, name, body) =>
      join(walk(info) :: name.map(walk) ++ List(walk(body)): _*)
    case AssemblyLet(info, name, body) =>
      join(walk(info) :: name.map(walk) ++ walkOpt(body): _*)
    case AssemblyIf(info, cond, body) =>
      join(walk(info) :: walk(cond) :: body.map(walk): _*)
    case AssemblySwitch(info, cond, cases, default) =>
      join(walk(info) :: walk(cond) :: cases.map(walk) ++
        default.map(walk): _*)
    case AssemblyFtn(info, name, param, body) =>
      join(
        walk(info) :: walk(name) :: walkOpt(param) ++
          body.map(walk): _*
      )
    case AssemblyFor(info, init, change, cond, body) =>
      join(walk(info) :: init.map(walk) ++ List(walk(change)) ++
        cond.map(walk) ++ body.map(walk): _*)
    case AssemblyAssignX(info, name, body) =>
      join(walk(info), walk(name), walk(body))
    case AssemblyAssign(info, name, body) =>
      join(walk(info) :: walk(name) :: walkOpt(body): _*)
    case AssemblyLabel(info, label) =>
      join(walk(info), walk(label))
    case AssemblyNumber(info, num) =>
      join(walk(info), walk(num))
    case AssemblyString(info, str) =>
      join(walk(info), walk(str))
    case AssemblyBreak(info) => walk(info)
    case AssemblyContinue(info) => walk(info)
    case SubAssembly(info, name, body) =>
      join(walk(info) :: walk(name) :: body.map(walk): _*)
    case FtnAssembly(info, name, body) =>
      join(walk(info) :: walk(name) :: body.map(walk): _*)
    case AssemblyExprStmt(info, body) =>
      join(walk(info), walk(body))
    case AssemblyId(info, name) =>
      join(walk(info), walk(name))
    case a: AssemblyExpression => walk(a)
  }

  def walk(node: AssemblyExpression): Result = node match {
    case AssemblyCall(info, name, args) =>
      join(walk(info) :: walk(name) :: args.map(walk): _*)
    case AssemblyNumber(info, num) =>
      join(walk(info), walk(num))
    case AssemblyString(info, str) =>
      join(walk(info), walk(str))
    case AssemblyId(info, name) => join(walk(info), walk(name))
  }

  def walk(node: TypeName): Result = node match {
    case ArrayTypeName(info, name, size) =>
      join(walk(info) :: walk(name) :: walkOpt(size): _*)
    case ElementaryTypeName(info, typ) => walk(info)
    case UserDefinedTypeName(info, names) =>
      join(walk(info) :: names.map(walk): _*)
    case Mapping(info, domain, range) =>
      join(walk(info), walk(range))
    case FunctionTypeName(info) => walk(info)
  }

  def walk(node: Alias): Result = node match {
    case Alias(info, from, to) =>
      join(walk(info) :: walk(from) :: walkOpt(to): _*)
  }

  def walk(node: InheritanceSpecifier): Result = node match {
    case InheritanceSpecifier(info, name, args) =>
      join(walk(info) :: walk(name) :: args.map(walk): _*)
  }

  def walk(node: VariableDeclaration): Result = node match {
    case VariableDeclaration(info, typ, loc, name) =>
      join(walk(info), walk(typ), walk(name))
  }

  def walk(node: Parameter): Result = node match {
    case Parameter(info, typ, loc, name) =>
      join(walk(info) :: walk(typ) :: walkOpt(name): _*)
  }

  def walk(node: EventParameter): Result = node match {
    case EventParameter(info, typ, name) =>
      join(walk(info) :: walk(typ) :: walkOpt(name): _*)
  }

  def walk(node: AssemblyCase): Result = node match {
    case AssemblyCase(info, cond, body) =>
      join(walk(info) :: walk(cond) :: body.map(walk): _*)
  }

  def walk(node: Id): Result = node match {
    case Id(info, name) => walk(info)
  }

  def walk(node: Op): Result = node match {
    case Op(info, name) => walk(info)
  }
}
