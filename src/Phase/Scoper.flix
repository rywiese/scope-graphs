mod SG.Phase.Scoper {
    use SG.Ast.ScopeGraph.AstNode.Decls
    use CurrentScope.currentScope
    use CurrentScope.enterFreshScope
    use CurrentScope.exitScope
    use MarkDecl.markDecl
    use MarkUse.markUse
    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.mkName
    use SG.Ast.ParsedAst.Module.Module
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.LocString.isLowerCase
    use SG.Ast.Shared.Location.Source
    use ToString.toString
    use Set.memberOf

    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Decl
    use SG.Ast.ScopeGraph.Decls
    use SG.Ast.ScopeGraph.Def
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Enum
    use SG.Ast.ScopeGraph.EnumCase
    use SG.Ast.ScopeGraph.Exp
    use SG.Ast.ScopeGraph.Handle
    use SG.Ast.ScopeGraph.Kind
    use SG.Ast.ScopeGraph.Module
    use SG.Ast.ScopeGraph.Refr
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.ScopeGraph.Tpe
    use SG.Ast.ScopeGraph.TypeAlias
    use SG.Ast.ScopeGraph.Use

    eff MarkUse {
        def markUse(usage: Refr): Unit
    }

    eff MarkDecl {
        def markDecl(decl: Decl): Unit
    }

    eff CurrentScope {
        def currentScope(): Scope
        def enterFreshScope(): Scope
        def exitScope(): Unit
    }

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph] = {
        Validation.Success(buildScopeGraph(ast))
    }

    def buildScopeGraph(ast: ParsedAst): ScopeGraph =
        region rc {
            let freshScopeCount = Ref.fresh(rc, 0);
            let scopes = MutList.empty(rc);
            let edges = MutMap.empty(rc);
            def currentScope() = {
                scopes
                    |> MutList.last
                    |> Option.getWithDefault(Scope.top())
            };
            def addEdge(scope, edge) = {
                match MutMap.get(scope, edges) {
                    case None => {
                        let newList = MutList.empty(rc) !> MutList.push(edge);
                        edges |> MutMap.put(scope, newList)
                    }
                    case Some(oldList) => {
                        // TODO: pushIfAbsent?
                        oldList |> MutList.push(edge)
                    }
                }
            };
            run {
                ScopeGraph.ScopeGraph({
                    nodes = visitAst(ast),
                    edges = edges
                        |> MutMap.map(rc, MutList.toList)
                        |> MutMap.toMap
                })
            } with handler MarkUse {
                def markUse(usage, resume) = {
                    addEdge(currentScope(), Edge.Use(usage));
                    resume(())
                }
            } with handler MarkDecl {
                def markDecl(decl, resume) = {
                    addEdge(currentScope(), Edge.Decl(decl));
                    resume(())
                }
            } with handler CurrentScope {
                def currentScope(resume) = {
                    resume(currentScope())
                }
                def enterFreshScope(resume) = {
                    let parent = currentScope();
                    let child = freshScopeCount
                        !> Ref.transform(n -> n + 1)
                        |> Ref.get
                        |> Scope.Scope;
                    scopes |> MutList.push(child);
                    addEdge(child, Edge.Lex(parent));
                    resume(child)
                }
                def exitScope(resume) = {
                    scopes |> MutList.pop;
                    resume(())
                }
            }
        }

    def visitAst(ast: ParsedAst): Map[Source, ScopeGraph.Decls] \ CurrentScope + MarkUse + MarkDecl =
        ast#m |> Functor.map(visitDecls)

    def visitDecls(decls: ParsedAst.Scope): ScopeGraph.Decls \ CurrentScope + MarkUse + MarkDecl =
        let {
            uses,
            modules,
            aliases,
            enums,
            defs,
            loc
        } = coerce(decls);
        Decls.Decls({
            uses = uses |> List.map(visitUse),
            modules = modules |> List.map(visitModule),
            aliases = aliases |> List.map(visitAlias),
            enums = enums |> List.map(visitEnum),
            defs = defs |> List.map(visitDef),
            loc = loc
        })

    def visitUse(uze: ParsedAst.Use): ScopeGraph.Use \ CurrentScope + MarkUse + MarkDecl =
        let {
            ac,
            name,
            renaming,
            loc
        } = coerce(uze);
        let usage = visitName(Kind.Any, name);
        markUse(usage);
        Use.Use({
            ac = ac,
            usage = usage,
            renaming = renaming |> Option.map(id -> declare(Kind.Any, Decl.Rename, id, handle -> handle)),
            loc = loc
        })

    def visitModule(module: ParsedAst.Module): ScopeGraph.Module \ CurrentScope + MarkUse + MarkDecl =
        let {
            ac,
            name,
            scope = decls,
            loc
        } = coerce(module);
        match coerce(name)#qualifiers |> List.head {
            case Some(q) => declareInFreshScope(Kind.Module, Decl.Module, q, scope -> handle -> Module.Module({
                ac = ac,
                handle = handle,
                introduces = scope,
                decls = Decls.Decls({
                    uses = List.empty(),
                    modules = visitModule(ParsedAst.Module.Module({
                        ac = ac,
                        name = mkName(coerce(name)#qualifiers |> List.drop(1), coerce(name)#name),
                        scope = decls,
                        loc = loc
                    })) :: Nil,
                    aliases = List.empty(),
                    enums = List.empty(),
                    defs = List.empty(),
                    loc = loc
                }),
                loc = loc
            }))
            case None => declareInFreshScope(Kind.Module, Decl.Module, coerce(name)#name, scope -> handle -> Module.Module({
                ac = ac,
                handle = handle,
                introduces = scope,
                decls = visitDecls(decls),
                loc = loc
            }))
        }

    def visitAlias(aliass: ParsedAst.TypeAlias): ScopeGraph.TypeAlias \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            loc
        } = coerce(aliass);
        declareInFreshScope(Kind.Tpe, Decl.TypeAlias, name, _ -> handle -> TypeAlias.TypeAlias({
            ac = ac,
            handle = handle,
            args = args |> List.map(arg -> declare(Kind.Tpe, Decl.TpeVar, arg, argHandle -> argHandle)),
            tpe = {
                let boundVars = args
                    |> List.map(arg -> coerce(arg)#s)
                    |> List.toSet;
                let (_, sgTpe) = visitTpe(boundVars, tpe);
                sgTpe
            },
            loc = loc
        }))

    def visitEnum(eenum: ParsedAst.Enum): ScopeGraph.Enum \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            args,
            cases,
            loc
        } = coerce(eenum);
        declareInFreshScope(Kind.Tpe, Decl.Enum, name, scope -> handle -> {
            let newArgs = args |> List.map(arg -> declare(Kind.Tpe, Decl.TpeVar, arg, argHandle -> argHandle));
            let boundVars = newArgs |> List.map(arg -> {
                let Handle.Handle(_, l) = arg;
                coerce(l)#s
            }) |> List.toSet;
            let newCases = cases |> List.map(visitEnumCase(boundVars));
            Enum.Enum({
                ac = ac,
                handle = handle,
                introduces = scope,
                args = newArgs,
                cases = newCases,
                loc = loc
            })
        })

    def visitEnumCase(boundVars: Set[String], enumCase: ParsedAst.EnumCase): ScopeGraph.EnumCase \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            terms,
            loc
        } = coerce(enumCase);
        let (_, newTerms) = visitTpes(boundVars, terms); // TODO: Scoping
        declare(Kind.Var, Decl.EnumCase, name, handle -> EnumCase.EnumCase({
            ac = ac,
            handle = handle,
            terms = newTerms,
            loc = loc
        }))

    def visitDef(deff: ParsedAst.Def): ScopeGraph.Def \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            exp,
            loc
        } = coerce(deff);
        declareInFreshScope(Kind.Var, Decl.Def, name, _ -> handle -> {
            let (argVars, argTpes) = args |> List.unzip;
            let newArgVars = argVars |> List.map(argVar -> declare(Kind.Var, Decl.Var, argVar, h -> h));
            let (boundVars, newArgTpes) = visitTpes(Set.empty(), argTpes);
            let newArgs = List.zip(newArgVars, newArgTpes);
            let (_, newTpe) = visitTpe(boundVars, tpe);
            let newExp = visitExp(exp);
            Def.Def({
                ac = ac,
                handle = handle,
                args = newArgs,
                tpe = newTpe,
                exp = newExp,
                loc = loc
            })
        })

    def visitTpe(boundVars: Set[String], tpe: ParsedAst.Tpe): (Set[String], ScopeGraph.Tpe) \ CurrentScope + MarkDecl =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                let locId = coerce(name)#name;
                let isTypeVar = isLowerCase(locId);
                if (isTypeVar) {
                    let id = coerce(locId)#s;
                    if (id `memberOf` boundVars) {
                        (boundVars, ScopeGraph.Tpe.BoundVar(visitName(Kind.Tpe, name)))
                    } else {
                        (boundVars |> Set.insert(id), ScopeGraph.Tpe.FreeVar(declare(Kind.Tpe, Decl.TpeVar, locId, handle -> handle)))
                    }
                } else {
                    (boundVars, ScopeGraph.Tpe.Base(visitName(Kind.Tpe, name)))
                }
            }
            case ParsedAst.Tpe.App(applier, applied, loc) => {
                let (boundVars2, sgApplier) = visitTpe(boundVars, applier);
                let (boundVars3, sgApplied) = visitTpes(boundVars2, applied);
                (boundVars3, ScopeGraph.Tpe.App(sgApplier, sgApplied, loc))
            }
        }

    def visitTpes(boundVars: Set[String], tpes: List[ParsedAst.Tpe]): (Set[String], List[ScopeGraph.Tpe]) \ CurrentScope + MarkDecl =
        tpes |> List.foldLeft(pair -> t -> {
            let (bvs, ts) = pair;
            let (bvs2, t2) = visitTpe(bvs, t);
            (bvs2, t2 :: ts)
        }, (boundVars, Nil))

    def visitExp(exp: ParsedAst.Exp): ScopeGraph.Exp \ CurrentScope + MarkDecl =
        match exp {
            case ParsedAst.Exp.Let(id, binding, rest, loc) => {
                let newBinding = visitExp(binding);
                let _ = enterFreshScope();
                let handle = declare(Kind.Var, Decl.Var, id, h -> h);
                let newRest = visitExp(rest);
                exitScope();
                ScopeGraph.Exp.Let(handle, newBinding, newRest, loc)
            }
            case ParsedAst.Exp.Stmt(e1, e2, loc) => {
                let newE1 = visitExp(e1);
                let newE2 = visitExp(e2);
                ScopeGraph.Exp.Stmt(newE1, newE2, loc)
            }
            case ParsedAst.Exp.App(e, es, loc) => {
                let newE = visitExp(e);
                let newEs = es |> List.map(visitExp);
                ScopeGraph.Exp.App(newE, newEs, loc)
            }
            case ParsedAst.Exp.Name(name, loc) => ScopeGraph.Exp.Refr(visitName(Kind.Var, name), loc)
            case ParsedAst.Exp.Int(i, loc) => ScopeGraph.Exp.Int(i, loc)
            case ParsedAst.Exp.Bool(b, loc) => ScopeGraph.Exp.Bool(b, loc)
        }

    def visitName(kind: Kind, name: ParsedAst.Name): ScopeGraph.Refr \ CurrentScope =
        let { qualifiers, name = id, loc = _ } = coerce(name);
        let scope = currentScope();
        match split1(qualifiers) {
            case Some((unqualified, qualified)) => {
                let qualRef = (Refr.Unqualified(Kind.Any, unqualified, scope), qualified) ||> List.foldLeft(Refr.Qualified(Kind.Any));
                Refr.Qualified(kind, qualRef, id)
            }
            case None => Refr.Unqualified(kind, id, scope)
        }

    def split1(xs: List[t]): Option[(t, List[t])] =
        xs
            |> List.head
            |> Option.map(x -> (x, xs |> List.drop(1)))

    def declareInFreshScope(kind: Kind, cons: t -> Decl, loc: LocString, mkDecl: Scope -> Handle -> t \ ef): t \ ef + CurrentScope + MarkDecl =
        let freshScope = enterFreshScope();
        let decl = mkDecl(freshScope, Handle.Handle(kind, loc));
        exitScope();
        markDecl(cons(decl));
        decl

    def declare(kind: Kind, cons: t -> Decl, loc: LocString, mkDecl: Handle -> t \ ef): t \ ef + MarkDecl =
        let decl = mkDecl(Handle.Handle(kind, loc));
        markDecl(cons(decl));
        decl

}
