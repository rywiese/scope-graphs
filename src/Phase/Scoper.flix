mod SG.Phase.Scoper {
    use EnterScope.enterScope // note evidence of non fully qualified use?
    use MarkDecl.markDecl
    use MarkRef.markRef
    use MarkUse.markUse
    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.Module.Module
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Node
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.LocString.isLowerCase
    use ToString.toString

    eff EnterScope {
        def enterScope(enter: Node.Scope -> Node.Scope): Unit
    }

    eff MarkRef {
        def markRef(ref: LocString): Unit
    }

    eff MarkUse {
        def markUse(used: Node.Scope): Unit
    }

    eff MarkDecl {
        def markDecl(decl: Node.Decl): Unit
    }

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph] = {
        Validation.Success(buildScopeGraph(ast))
    }

    def buildScopeGraph(ast: ParsedAst): ScopeGraph =
        region rc {
            let scope = Ref.fresh(rc, Node.Scope.Top);
            let refs = MutMap.empty(rc);
            let edges = MutMap.empty(rc);
            def addEdge(node, edge) = {
                match MutMap.get(node, edges) {
                    case None => {
                        let newList = MutList.empty(rc) !> MutList.push(edge);
                        edges |> MutMap.put(node, newList)
                    }
                    case Some(oldList) => {
                        oldList |> MutList.push(edge)
                    }
                }
            };
            run {
                visitAst(ast)
            } with handler EnterScope {
                def enterScope(enter, resume) = {
                    let parent = Ref.get(scope);
                    let child = enter(parent);
                    Ref.put(child, scope);
                    addEdge(child, Edge.Lex(parent));
                    resume(())
                }
            } with handler MarkRef {
                def markRef(ref, resume) = {
                    refs |> MutMap.put(ref, Ref.get(scope));
                    resume(())
                }
            } with handler MarkUse {
                def markUse(used, resume) = {
                    addEdge(Ref.get(scope), Edge.Use(used));
                    resume(())
                }
            } with handler MarkDecl {
                def markDecl(decl, resume) = {
                    addEdge(Ref.get(scope), Edge.Decl(decl));
                    resume(())
                }
            };
            ScopeGraph.ScopeGraph({
                refs = refs |> MutMap.toMap,
                edges = edges
                    |> MutMap.map(rc, MutList.toList)
                    |> MutMap.toMap
            })
        }

    def visitAst(ast: ParsedAst): Unit \ EnterScope + MarkRef + MarkUse + MarkDecl =
        foreach(scope <- Map.valuesOf(ast#m)) visitScope(scope)

    def visitScope(scope: ParsedAst.Scope): Unit \ EnterScope + MarkRef + MarkUse + MarkDecl =
        let {
            uses,
            modules,
            aliases,
            enums,
            defs,
            loc = _
        } = coerce(scope);
        foreach(uze <- uses) visitUse(uze);
        foreach(module <- modules) visitModule(module);
        foreach(aliass <- aliases) visitAlias(aliass);
        foreach(eenum <- enums) visitEnum(eenum);
        foreach(deff <- defs) visitDef(deff)

    def visitUse(uze: ParsedAst.Use): Unit \ MarkUse =
        let {
            ac = _,
            name,
            renaming = _,
            loc = _
        } = coerce(uze);
        let used = Node.Scope.fromFullyQualified(name);
        markUse(used)

    def visitModule(module: ParsedAst.Module): Unit \ EnterScope + MarkRef + MarkUse + MarkDecl =
        let {
            ac = _,
            name,
            scope,
            loc = _
        } = coerce(module);
        markDecl(Node.Decl.Module(module));
        enterScope(Node.Scope.module(name));
        visitScope(scope)

    def visitAlias(aliass: ParsedAst.TypeAlias): Unit \ MarkRef + MarkDecl =
        let {
            ac = _,
            name = _,
            args,
            tpe,
            loc = _
        } = coerce(aliass);
        markDecl(Node.Decl.TypeAlias(aliass));
        foreach(arg <- args) markDecl(Node.Decl.Tpe(arg));
        visitRefTpe(tpe)

    def visitEnum(eenum: ParsedAst.Enum): Unit \ MarkRef + MarkDecl =
        let {
            ac = _,
            name = _,
            args,
            cases,
            loc = _
        } = coerce(eenum);
        markDecl(Node.Decl.Enum(eenum));
        // we probably need a new scope in which to declare type vars?
        foreach(arg <- args) markDecl(Node.Decl.Tpe(arg));
        foreach(enumCase <- cases) visitEnumCase(enumCase)

    def visitEnumCase(enumCase: ParsedAst.EnumCase): Unit \ MarkRef + MarkDecl =
        let {
            ac = _,
            name = _,
            terms,
            loc = _
        } = coerce(enumCase);
        markDecl(Node.Decl.EnumCase(enumCase));
        foreach(term <- terms) visitRefTpe(term)

    def visitDef(deff: ParsedAst.Def): Unit \ EnterScope + MarkRef + MarkDecl =
        let {
            ac = _,
            name = _,
            args,
            tpe,
            exp,
            loc = _
        } = coerce(deff);
        markDecl(Node.Decl.Def(deff));
        let argTpes = args |> List.map(arg -> match arg { case (_, t) => t });
        // scope for type vars? scope for var names? are they the same?
        foreach(argTpe <- argTpes) visitDeclTpe(argTpe);
        visitDeclTpe(tpe);
        visitExp(exp)

    def visitDeclTpe(tpe: ParsedAst.Tpe): Unit \ MarkRef + MarkDecl =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                let id = coerce(name)#name;
                let isTypeVar = isLowerCase(id);
                if (isTypeVar) {
                    markDecl(Node.Decl.Tpe(id))
                } else {
                    markRef(id)
                }
            }
            case ParsedAst.Tpe.App(applier, applied, _) => {
                visitDeclTpe(applier);
                foreach(t <- applied) visitDeclTpe(t)
            }
        }

    def visitRefTpe(tpe: ParsedAst.Tpe): Unit \ MarkRef =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                markRef(coerce(name)#name)
            }
            case ParsedAst.Tpe.App(applier, applied, _) => {
                visitRefTpe(applier);
                foreach(t <- applied) visitRefTpe(t)
            }
        }

    def visitExp(exp: ParsedAst.Exp): Unit \ EnterScope + MarkRef + MarkDecl =
        match exp {
            case ParsedAst.Exp.Let(id, binding, rest, _) => {
                visitExp(binding);
                enterScope(Node.Scope.letBind(coerce(id)#s));
                markDecl(Node.Decl.Id(id));
                visitExp(rest)
            }
            case ParsedAst.Exp.Stmt(e1, e2, _) => {
                visitExp(e1);
                visitExp(e2)
            }
            case ParsedAst.Exp.App(e1, es, _) => {
                visitExp(e1);
                foreach(e2 <- es) visitExp(e2)
            }
            case ParsedAst.Exp.Name(name, _) => {
                markRef(coerce(name)#name)
            }
            case ParsedAst.Exp.Int(_, _) => ()
            case ParsedAst.Exp.Bool(_, _) => ()
        }

}
