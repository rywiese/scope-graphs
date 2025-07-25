mod SG.Phase.Scoper {
    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.Module.Module
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.LocString.isLowerCase
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Node
    use ToString.toString

    eff AddRef {
        def addRef(scope: Node.Scope, src: Node.Ref): Unit
    }

    eff AddLex {
        def addLex(scope: Node.Scope, parent: Node.Scope): Unit
    }

    eff AddUse {
        def addUse(scope: Node.Scope, used: Node.Scope): Unit
    }

    eff AddDecl {
        def addDecl(scope: Node.Scope, decl: Node.Decl): Unit
    }

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph] = {
        Validation.Success(buildScopeGraph(ast))
    }

    pub def buildScopeGraph(ast: ParsedAst): ScopeGraph =
        region rc {
            let refs = MutMap.empty(rc);
            let edges = MutMap.empty(rc);
            def addEdge(scope, edge) = {
                match MutMap.get(scope, edges) {
                    case None => {
                        let newList = MutList.empty(rc) !> MutList.push(edge);
                        edges |> MutMap.put(scope, newList)
                    }
                    case Some(oldList) => {
                        oldList |> MutList.push(edge)
                    }
                }
            };
            run {
                visitAst(ast)
            } with handler AddRef {
                def addRef(scope, ref, resume) = {
                    refs |> MutMap.put(ref, scope);
                    resume(())
                }
            } with handler AddLex {
                def addLex(scope, parent, resume) = {
                    addEdge(scope, Edge.Lex(parent));
                    resume(())
                }
            } with handler AddUse {
                def addUse(scope, used, resume) = {
                    addEdge(scope, Edge.Use(used));
                    resume(())
                }
            } with handler AddDecl {
                def addDecl(scope, decl, resume) = {
                    addEdge(scope, Edge.Decl(decl));
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

    def visitAst(ast: ParsedAst): Unit \ AddRef + AddLex + AddUse + AddDecl =
        foreach(astScope <- Map.valuesOf(ast#m)) visitScope(Node.Scope.Top, astScope)

    def visitScope(scope: Node.Scope, astScope: ParsedAst.Scope): Unit \ AddRef + AddLex + AddUse + AddDecl =
        let {
            uses,
            modules,
            aliases,
            enums,
            defs,
            loc = _
        } = coerce(astScope);
        foreach(uze <- uses) visitUse(scope, uze);
        foreach(module <- modules) visitModule(scope, module);
        foreach(aliass <- aliases) visitAlias(scope, aliass);
        foreach(eenum <- enums) visitEnum(scope, eenum);
        foreach(deff <- defs) visitDef(scope, deff)

    def visitUse(scope: Node.Scope, uze: ParsedAst.Use): Unit \ AddUse =
        let {
            ac = _,
            name,
            renaming = _,
            loc = _
        } = coerce(uze);
        let used = Node.Scope.fromFullyQualified(name);
        AddUse.addUse(scope, used)

    def visitModule(scope: Node.Scope, module: ParsedAst.Module): Unit \ AddRef + AddLex + AddUse + AddDecl =
        let {
            ac = _,
            name,
            scope = astScope,
            loc = _
        } = coerce(module);
        let subScope = scope |> Node.Scope.enter(name);
        AddDecl.addDecl(scope, Node.Decl.Module(module));
        AddLex.addLex(subScope, scope);
        visitScope(subScope, astScope)

    def visitAlias(scope: Node.Scope, aliass: ParsedAst.TypeAlias): Unit \ AddRef + AddDecl =
        let {
            ac = _,
            name = _,
            args,
            tpe,
            loc = _
        } = coerce(aliass);
        AddDecl.addDecl(scope, Node.Decl.TypeAlias(aliass));
        foreach(arg <- args) AddRef.addRef(scope, Node.Ref.LocString(arg)); // decl?
        visitRefTpe(scope, tpe)

    def visitEnum(scope: Node.Scope, eenum: ParsedAst.Enum): Unit \ AddRef + AddDecl =
        let {
            ac = _,
            name = _,
            args,
            cases,
            loc = _
        } = coerce(eenum);
        AddDecl.addDecl(scope, Node.Decl.Enum(eenum));
        foreach(arg <- args) AddDecl.addDecl(scope, Node.Decl.Tpe(arg));
        foreach(enumCase <- cases) visitEnumCase(scope, enumCase)

    def visitEnumCase(scope: Node.Scope, enumCase: ParsedAst.EnumCase): Unit \ AddRef + AddDecl =
        let {
            ac = _,
            name = _,
            terms,
            loc = _
        } = coerce(enumCase);
        AddDecl.addDecl(scope, Node.Decl.EnumCase(enumCase));
        // we probably need a new scope in which to declare type vars?
        foreach(term <- terms) visitRefTpe(scope, term)

    def visitDef(scope: Node.Scope, deff: ParsedAst.Def): Unit \ AddRef + AddDecl + AddLex =
        let {
            ac = _,
            name = _,
            args,
            tpe,
            exp,
            loc = _
        } = coerce(deff);
        AddDecl.addDecl(scope, Node.Decl.Def(deff));
        let argTpes = args |> List.map(arg -> match arg { case (_, t) => t });
        // scope for type vars? scope for var names? are they the same?
        foreach(argTpe <- argTpes) visitDeclTpe(scope, argTpe);
        visitDeclTpe(scope, tpe);
        visitExp(scope, exp)

    def visitDeclTpe(scope: Node.Scope, tpe: ParsedAst.Tpe): Unit \ AddRef + AddDecl =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                let id = coerce(name)#name;
                if (isLowerCase(id)) {
                    // type var
                    AddDecl.addDecl(scope, Node.Decl.Tpe(id))
                } else {
                    // concrete type
                    AddRef.addRef(scope, Node.Ref.Name(name))
                }
            }
            case ParsedAst.Tpe.App(applier, applied, _) => {
                visitDeclTpe(scope, applier);
                foreach(t <- applied) visitDeclTpe(scope, t)
            }
        }

    def visitRefTpe(scope: Node.Scope, tpe: ParsedAst.Tpe): Unit \ AddRef =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                AddRef.addRef(scope, Node.Ref.Name(name))
            }
            case ParsedAst.Tpe.App(applier, applied, _) => {
                visitRefTpe(scope, applier);
                foreach(t <- applied) visitRefTpe(scope, t)
            }
        }

    def visitExp(scope: Node.Scope, exp: ParsedAst.Exp): Unit \ AddRef + AddDecl + AddLex =
        match exp {
            case ParsedAst.Exp.Let(id, binding, rest, _) => {
                visitExp(scope, binding);
                let childScope = scope |> Node.Scope.letBind(coerce(id)#s);
                AddLex.addLex(childScope, scope);
                AddDecl.addDecl(childScope, Node.Decl.Id(id));
                visitExp(childScope, rest)
            }
            case ParsedAst.Exp.Stmt(e1, e2, _) => {
                visitExp(scope, e1);
                visitExp(scope, e2)
            }
            case ParsedAst.Exp.App(e1, es, _) => {
                visitExp(scope, e1);
                foreach(e2 <- es) visitExp(scope, e2)
            }
            case ParsedAst.Exp.Name(name, _) => {
                AddRef.addRef(scope, Node.Ref.Name(name))
            }
            case ParsedAst.Exp.Int(_, _) => ()
            case ParsedAst.Exp.Bool(_, _) => ()
        }

}
