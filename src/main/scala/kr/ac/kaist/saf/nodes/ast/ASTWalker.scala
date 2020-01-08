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

trait ASTWalker {
  def walk(info: ASTNodeInfo): ASTNodeInfo = info match {
    case ASTNodeInfo(span) =>
      ASTNodeInfo(span)
  }

  def walk(node: ASTNode): ASTNode = node match {
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

  def walk(node: SourceUnit): SourceUnit = node match {
    case SourceUnit(info, body) =>
      SourceUnit(walk(info), body.map(walk))
  }

  def walk(node: SourceElement): SourceElement = node match {
    case PragmaDirective(info, name) =>
      PragmaDirective(walk(info), walk(name))
    case ContractDefinition(info, kind, name, supers, body) =>
      ContractDefinition(walk(info), walk(kind), walk(name),
        supers.map(walk), body.map(walk))
  }

  def walk(node: ImportDirective): ImportDirective = node match {
    case Import(info, alias) =>
      Import(walk(info), walk(alias))
    case ImportAll(info, to, module) =>
      ImportAll(walk(info), to.map(walk), module)
    case ImportFrom(info, aliases, from) =>
      ImportFrom(walk(info), aliases.map(walk), from)
  }

  def walk(node: FtnMod): FtnMod = node match {
    case ModifierInvocation(info, name, args) =>
      ModifierInvocation(walk(info), walk(name), args.map(walk))
    case m: StateMutability => walk(m)
    case External(info) => External(walk(info))
    case Internal(info) => Internal(walk(info))
    case Private(info) => Private(walk(info))
    case Public(info) => Public(walk(info))
  }

  def walk(node: StateMutability): StateMutability = node match {
    case Payable(info) => Payable(walk(info))
    case Pure(info) => Pure(walk(info))
    case View(info) => View(walk(info))
    case Constant(info) => Constant(walk(info))
  }

  def walk(node: Mod): Mod = node match {
    case Constant(info) => Constant(walk(info))
    case External(info) => External(walk(info))
    case Internal(info) => Internal(walk(info))
    case Private(info) => Private(walk(info))
    case Public(info) => Public(walk(info))
    case Payable(info) => Payable(walk(info))
  }

  def walk(node: DefinitionKind): DefinitionKind = node match {
    case ContractKind(info) => ContractKind(walk(info))
    case LibraryKind(info) => LibraryKind(walk(info))
    case InterfaceKind(info) => InterfaceKind(walk(info))
  }

  def walk(node: ContractPart): ContractPart = node match {
    case StateVariableDeclaration(info, typ, mods, name, body) =>
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
      FunctionDefinition(walk(info), name.map(walk), params.map(walk),
        mods.map(walk), result.map(walk), body.map(walk), comment)
    case EventDefinition(info, name, params) =>
      EventDefinition(walk(info), walk(name), params.map(walk))
    case EnumDefinition(info, name, vals) =>
      EnumDefinition(walk(info), walk(name), vals.map(walk))
  }

  def walk(node: Statement): Statement = node match {
    case s: SimpleStatement => walk(s)
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
    case Throw(info) => Throw(walk(info))
    case Emit(info, emit) => Emit(walk(info), walk(emit))
    case Underscore(info) => Underscore(walk(info))
  }

  def walk(node: SimpleStatement): SimpleStatement = node match {
    case SimpleVarX(info, name, body) =>
      SimpleVarX(walk(info), name.map(_.map(walk)), walk(body))
    case SimpleVar(info, name, body) =>
      SimpleVar(walk(info), walk(name), body.map(walk))
    case VarDecl(info, dec, body) =>
      VarDecl(walk(info), dec.map(walk), body.map(walk))
    case ExprStmt(info, body) =>
      ExprStmt(walk(info), walk(body))
  }

  def walk(node: Expression): Expression = node match {
    case AssignOpApp(info, left, op, right) =>
      AssignOpApp(walk(info), walk(left), walk(op), walk(right))
    case Cond(info, cond, trueE, falseE) =>
      Cond(walk(info), walk(cond), walk(trueE), walk(falseE))
    case InfixOpApp(info, left, op, right) =>
      InfixOpApp(walk(info), walk(left), walk(op), walk(right))
    case PrefixOpApp(info, op, right) =>
      PrefixOpApp(walk(info), walk(op), walk(right))
    case UnaryAssignOpApp(info, lhs, op) =>
      UnaryAssignOpApp(walk(info), walk(lhs), walk(op))
    case FunctionCall(info, ftn, args) =>
      FunctionCall(walk(info), walk(ftn), args.map(walk))
    case Bracket(info, ftn, args) =>
      Bracket(walk(info), walk(ftn), args.map(walk))
    case Dot(info, base, name) =>
      Dot(walk(info), walk(base), walk(name))
    case NewCall(info, call) =>
      NewCall(walk(info), walk(call))
    case New(info, name) =>
      New(walk(info), walk(name))
    case TypeRef(info, name) =>
      TypeRef(walk(info), walk(name))
    case VarRef(info, name) =>
      VarRef(walk(info), walk(name))
    case ArrayLiteral(info, elems) =>
      ArrayLiteral(walk(info), elems.map(walk))
    case TupleLiteral(info, elems) =>
      TupleLiteral(walk(info), elems.map(walk))
    case TupleOptLiteral(info, elems) =>
      TupleOptLiteral(walk(info), elems.map(_.map(walk)))
    case l: Literal => walk(l)
  }

  def walk(node: Literal): Literal = node match {
    case Bool(info, bool) => Bool(walk(info), bool)
    case s: StringLiteral => walk(s)
    case n: NumberLiteral => walk(n)
  }

  def walk(node: StringLiteral): StringLiteral = node match {
    case StringLiteral(info, quote, str) =>
      StringLiteral(walk(info), quote, str)
  }

  def walk(node: NumberLiteral): NumberLiteral = node match {
    case IntLiteral(info, intVal, radix) =>
      IntLiteral(walk(info), intVal, radix)
    case DoubleLiteral(info, text, num) =>
      DoubleLiteral(walk(info), text, num)
  }

  def walk(node: AssemblyItem): AssemblyItem = node match {
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

  def walk(node: FtnAssembly): FtnAssembly = node match {
    case FtnAssembly(info, name, body) =>
      FtnAssembly(walk(info), walk(name), body.map(walk))
  }

  def walk(node: AssemblyExpression): AssemblyExpression = node match {
    case AssemblyCall(info, name, args) =>
      AssemblyCall(walk(info), walk(name), args.map(walk))
    case AssemblyNumber(info, num) =>
      AssemblyNumber(walk(info), walk(num))
    case AssemblyString(info, str) =>
      AssemblyString(walk(info), walk(str))
    case AssemblyId(info, name) => AssemblyId(walk(info), walk(name))
  }

  def walk(node: TypeName): TypeName = node match {
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

  def walk(node: Alias): Alias = node match {
    case Alias(info, from, to) =>
      Alias(walk(info), walk(from), to.map(walk))
  }

  def walk(node: InheritanceSpecifier): InheritanceSpecifier = node match {
    case InheritanceSpecifier(info, name, args) =>
      InheritanceSpecifier(walk(info), walk(name), args.map(walk))
  }

  def walk(node: VariableDeclaration): VariableDeclaration = node match {
    case VariableDeclaration(info, typ, loc, name) =>
      VariableDeclaration(walk(info), walk(typ), loc, walk(name))
  }

  def walk(node: Parameter): Parameter = node match {
    case Parameter(info, typ, loc, name) =>
      Parameter(walk(info), walk(typ), loc, name.map(walk))
  }

  def walk(node: EventParameter): EventParameter = node match {
    case EventParameter(info, typ, name) =>
      EventParameter(walk(info), walk(typ), name.map(walk))
  }

  def walk(node: AssemblyCase): AssemblyCase = node match {
    case AssemblyCase(info, cond, body) =>
      AssemblyCase(walk(info), walk(cond), body.map(walk))
  }

  def walk(node: Id): Id = node match {
    case Id(info, name) => Id(walk(info), name)
  }

  def walk(node: Op): Op = node match {
    case Op(info, name) => Op(walk(info), name)
  }
}
