mod SG.Phase.Scoper {
    use EnterScope.enterScope // note evidence of non fully qualified use?
    use ExitScope.exitScope
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
    use SG.Ast.ScopeGraph.Def
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Enum
    use SG.Ast.ScopeGraph.EnumCase
    use SG.Ast.ScopeGraph.Exp
    use SG.Ast.ScopeGraph.Kind
    use SG.Ast.ScopeGraph.Module
    use SG.Ast.ScopeGraph.Namespace
    use SG.Ast.ScopeGraph.Refr
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.ScopeGraph.Tpe
    use SG.Ast.ScopeGraph.TypeAlias
    use SG.Ast.ScopeGraph.Use

    eff Ry {
        def currentNamespace(): Namespace
    }

    eff EnterScope {
        def enterScope(enter: Namespace -> Namespace): Unit
    }

    eff ExitScope {
        def exitScope(): Unit
    }

    eff MarkUse {
        def markUse(used: Refr): Unit
    }

    eff MarkDecl {
        def markDecl(decl: Decl): Unit
    }

    def inNewScope(enter: Namespace -> Namespace, f: Unit -> t \ ef): t \ ef + EnterScope + ExitScope =
        enterScope(enter);
        let t = f();
        exitScope();
        t

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph] = {
        Validation.Success(buildScopeGraph(ast))
    }

    def buildScopeGraph(ast: ParsedAst): ScopeGraph =
        region rc {
            let namespaces = MutList.empty(rc);
            // let refs = MutMap.empty(rc);
            let edges = MutMap.empty(rc);
            def currentScope = {
                namespaces
                    |> MutList.last
                    |> Option.getWithDefault(Namespace.Top)
            };
            def addEdge(namespace, edge) = {
                match MutMap.get(namespace, edges) {
                    case None => {
                        let newList = MutList.empty(rc) !> MutList.push(edge);
                        edges |> MutMap.put(namespace, newList)
                    }
                    case Some(oldList) => {
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
            } with handler Ry {
                def currentNamespace(resume) = {
                    let namespace = namespaces
                        |> MutList.last
                        |> Option.getWithDefault(Namespace.Top);
                    resume(namespace)
                }
            } with handler EnterScope {
                def enterScope(enter, resume) = {
                    let parent = currentScope();
                    let child = enter(parent);
                    namespaces |> MutList.push(child);
                    addEdge(child, Edge.Lex(parent));
                    resume(())
                }
            } with handler ExitScope {
                def exitScope(resume) = {
                    namespaces |> MutList.pop;
                    resume(())
                }
            } with handler MarkUse {
                def markUse(used, resume) = {
                    addEdge(currentScope(), Edge.Use(used));
                    resume(())
                }
            } with handler MarkDecl {
                def markDecl(decl, resume) = {
                    addEdge(currentScope(), Edge.Decl(decl));
                    resume(())
                }
            }
        }

    def visitAst(ast: ParsedAst): Map[Source, ScopeGraph.Scope] \ Ry + EnterScope + ExitScope + MarkUse + MarkDecl =
        ast#m |> Functor.map(visitScope) // why does this break the ide?

    def visitScope(scope: ParsedAst.Scope): ScopeGraph.Scope \ Ry + EnterScope + ExitScope + MarkUse + MarkDecl =
        let {
            uses,
            modules,
            aliases,
            enums,
            defs,
            loc
        } = coerce(scope);
        ScopeGraph.Scope.Scope({
            uses = uses |> List.map(visitUse),
            modules = modules |> List.map(visitModule),
            aliases = aliases |> List.map(visitAlias),
            enums = enums |> List.map(visitEnum),
            defs = defs |> List.map(visitDef),
            loc = loc
        })

    def visitUse(uze: ParsedAst.Use): ScopeGraph.Use \ Ry + MarkUse =
        let {
            ac,
            name,
            renaming,
            loc
        } = coerce(uze);
        let usage = visitName(name);
        markUse(usage);
        ScopeGraph.Use.Use({
            ac = ac,
            usage = usage,
            renaming = renaming,
            loc = loc
        })

    def visitModule(module: ParsedAst.Module): ScopeGraph.Module \ Ry + EnterScope + ExitScope + MarkUse + MarkDecl =
        let {
            ac,
            name,
            scope,
            loc
        } = coerce(module);
        let namespace = Ry.currentNamespace();
        let moduleName = coerce(name)#name;
        let refr = Refr.Refr(Kind.Module, None, moduleName, namespace);
        let newModule = ScopeGraph.Module.Module({
            ac = ac,
            qualifier = coerce(name)#qualifiers
                |> List.foldLeft(q -> m -> Refr.Refr(Kind.Module, Some(q), m, namespace), refr),
            name = moduleName,
            scope = inNewScope(Namespace.Module(coerce(moduleName)#s), () -> {
                visitScope(scope)
            }),
            loc = loc
        });
        markDecl(Decl.Module(newModule));
        newModule

    def visitAlias(aliass: ParsedAst.TypeAlias): ScopeGraph.TypeAlias \ Ry + EnterScope + ExitScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            loc
        } = coerce(aliass);
        let newAlias = ScopeGraph.TypeAlias.TypeAlias({
            ac = ac,
            name = name,
            args = args,
            tpe = inNewScope(Namespace.TypeAlias(coerce(name)#s), () -> {
                foreach(arg <- args) {
                    markDecl(Decl.TpeVar(arg))
                };
                let boundVars = args
                    |> List.map(arg -> coerce(arg)#s)
                    |> List.toSet;
                let (_, sgTpe) = visitTpe(boundVars, tpe);
                sgTpe
            }),
            loc = loc
        });
        markDecl(Decl.TypeAlias(newAlias));
        newAlias

    def visitEnum(eenum: ParsedAst.Enum): ScopeGraph.Enum \ Ry + EnterScope + ExitScope + MarkDecl =
        let {
            ac,
            name,
            args,
            cases,
            loc
        } = coerce(eenum);
        // markDecl(Node.Decl.Enum(eenum));
        // foreach(enumCase <- cases) {
        //     markDecl(Node.Decl.EnumCase(enumCase))
        // };
        // inNewScope(Node.Scope.eenum(coerce(name)#s), () -> {
        //     foreach(arg <- args) {
        //         markDecl(Node.Decl.TpeVar(arg))
        //     };
        //     foreach(enumCase <- cases) {
        //         foreach(term <- coerce(enumCase)#terms) {
        //             visitRefTpe(term)
        //         }
        //     }
        // })
        let newCases = inNewScope(Namespace.Enum(coerce(name)#s), () -> {
            foreach(arg <- args) {
                markDecl(Decl.TpeVar(arg))
            };
            let newCases = cases |> List.map(visitEnumCase);
            newCases
        });
        let newEnum = ScopeGraph.Enum.Enum({
            ac = ac,
            name = name,
            args = args,
            cases = newCases,
            loc = loc
        });
        markDecl(Decl.Enum(newEnum));
        foreach(enumCase <- newCases) {
            markDecl(Decl.EnumCase(enumCase))
        };
        newEnum

    def visitEnumCase(enumCase: ParsedAst.EnumCase): ScopeGraph.EnumCase \ Ry + MarkDecl =
        let {
            ac,
            name,
            terms,
            loc
        } = coerce(enumCase);
        let (_, newTerms) = visitTpes(Set.empty(), terms);
        ScopeGraph.EnumCase.EnumCase({
            ac = ac,
            name = name,
            terms = newTerms,
            loc = loc
        })

    def visitDef(deff: ParsedAst.Def): ScopeGraph.Def \ Ry + EnterScope + ExitScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            exp,
            loc
        } = coerce(deff);
        let (newArgs, newTpe, newExp) = inNewScope(Namespace.Def(coerce(name)#s), () -> {
            let (argVars, argTpes) = args |> List.unzip;
            foreach(v <- argVars) {
                markDecl(Decl.Var(v))
            };
            let (boundVars, newArgTpes) = visitTpes(Set.empty(), argTpes);
            let newArgs = List.zip(argVars, newArgTpes);
            let (_, newTpe) = visitTpe(boundVars, tpe);
            let newExp = visitExp(exp);
            (newArgs, newTpe, newExp)
        });
        let newDef = ScopeGraph.Def.Def({
            ac = ac,
            name = name,
            args = newArgs,
            tpe = newTpe,
            exp = newExp,
            loc = loc
        });
        markDecl(Decl.Def(newDef));
        newDef

    def visitTpe(boundVars: Set[String], tpe: ParsedAst.Tpe): (Set[String], ScopeGraph.Tpe) \ Ry + MarkDecl =
        match tpe {
            case ParsedAst.Tpe.Base(name, _) => {
                let locId = coerce(name)#name;
                let isTypeVar = isLowerCase(locId);
                if (isTypeVar) {
                    let id = coerce(locId)#s;
                    if (id `memberOf` boundVars) {
                        (boundVars, ScopeGraph.Tpe.BoundVar(visitName(name)))
                    } else {
                        markDecl(Decl.TpeVar(locId));
                        (boundVars |> Set.insert(id), ScopeGraph.Tpe.FreeVar(locId))
                    }
                } else {
                    (boundVars, ScopeGraph.Tpe.Base(visitName(name)))
                }
            }
            case ParsedAst.Tpe.App(applier, applied, loc) => {
                let (boundVars2, sgApplier) = visitTpe(boundVars, applier);
                let (boundVars3, sgApplied) = visitTpes(boundVars2, applied);
                (boundVars3, ScopeGraph.Tpe.App(sgApplier, sgApplied, loc))
            }
        }

    def visitTpes(boundVars: Set[String], tpes: List[ParsedAst.Tpe]): (Set[String], List[ScopeGraph.Tpe]) \ Ry + MarkDecl =
        tpes |> List.foldLeft(pair -> t -> {
            let (bvs, ts) = pair;
            let (bvs2, t2) = visitTpe(bvs, t);
            (bvs2, t2 :: ts)
        }, (boundVars, Nil))

    def visitExp(exp: ParsedAst.Exp): ScopeGraph.Exp \ Ry + EnterScope + ExitScope + MarkDecl =
        match exp {
            case ParsedAst.Exp.Let(id, binding, rest, loc) => {
                let newBinding = visitExp(binding);
                let newRest = inNewScope(Namespace.Let(coerce(id)#s), () -> {
                    markDecl(Decl.Var(id));
                    visitExp(rest)
                });
                ScopeGraph.Exp.Let(id, newBinding, newRest, loc)
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
            case ParsedAst.Exp.Name(name, loc) => ScopeGraph.Exp.Refr(visitName(name), loc)
            case ParsedAst.Exp.Int(i, loc) => ScopeGraph.Exp.Int(i, loc)
            case ParsedAst.Exp.Bool(b, loc) => ScopeGraph.Exp.Bool(b, loc)
        }

    def visitName(name: ParsedAst.Name): ScopeGraph.Refr \ Ry =
        let { qualifiers, name = id, loc = _ } = coerce(name);
        let namespace = Ry.currentNamespace();
        qualifiers |> List.foldLeft(
            q -> m -> Refr.Refr(Kind.Module, Some(q), m, namespace),
            Refr.Refr(Kind.Module, None, id, namespace)
        )

}
