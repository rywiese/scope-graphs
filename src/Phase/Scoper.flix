mod SG.Phase.Scoper {
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
    use SG.Ast.ScopeGraph.Handle
    use SG.Ast.ScopeGraph.Kind
    use SG.Ast.ScopeGraph.Module
    use SG.Ast.ScopeGraph.Namespace
    use SG.Ast.ScopeGraph.Refr
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.ScopeGraph.Tpe
    use SG.Ast.ScopeGraph.TypeAlias
    use SG.Ast.ScopeGraph.Use

    eff MarkUse {
        def markUse(used: Refr): Unit
    }

    eff MarkDecl {
        def markDecl(decl: Decl): Unit
    }

    eff CurrentScope {
        def get(): Namespace
        def enter(enter: Namespace -> Namespace): Unit
        def exit(): Namespace
    }

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph] = {
        Validation.Success(buildScopeGraph(ast))
    }

    def buildScopeGraph(ast: ParsedAst): ScopeGraph =
        region rc {
            let namespaces = MutList.empty(rc);
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
                        // TODO: pushIfAbset?
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
                def markUse(used, resume) = {
                    foreach(namespace <- used |> Refr.possibleInterpretations) {
                        addEdge(currentScope(), Edge.Use(namespace))
                    };
                    resume(())
                }
            } with handler MarkDecl {
                def markDecl(decl, resume) = {
                    addEdge(currentScope(), Edge.Decl(decl));
                    resume(())
                }
            } with handler CurrentScope {
                def get(resume) = {
                    let namespace = namespaces
                        |> MutList.last
                        |> Option.getWithDefault(Namespace.Top);
                    resume(namespace)
                }
                def enter(enter, resume) = {
                    let parent = currentScope();
                    let child = enter(parent);
                    namespaces |> MutList.push(child);
                    addEdge(child, Edge.Lex(parent));
                    resume(())
                }
                def exit(resume) = {
                    let old = namespaces
                        |> MutList.pop
                        |> Option.getWithDefault(Namespace.Top);
                    resume(old)
                }
            }
        }

    def visitAst(ast: ParsedAst): Map[Source, ScopeGraph.Scope] \ CurrentScope + MarkUse + MarkDecl =
        ast#m |> Functor.map(visitScope) // why does this break the ide?

    def visitScope(scope: ParsedAst.Scope): ScopeGraph.Scope \ CurrentScope + MarkUse + MarkDecl =
        let {
            uses,
            modules,
            aliases,
            enums,
            defs,
            loc
        } = coerce(scope);
        Scope.Scope({
            uses = uses |> List.map(visitUse),
            modules = modules |> List.map(visitModule),
            aliases = aliases |> List.map(visitAlias),
            enums = enums |> List.map(visitEnum),
            defs = defs |> List.map(visitDef),
            loc = loc
        })

    def visitUse(uze: ParsedAst.Use): ScopeGraph.Use \ CurrentScope + MarkUse =
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
            renaming = renaming,
            loc = loc
        })

    def visitModule(module: ParsedAst.Module): ScopeGraph.Module \ CurrentScope + MarkUse + MarkDecl =
        let {
            ac,
            name,
            scope,
            loc
        } = coerce(module);
        let moduleName = coerce(name)#name;
        declare(Kind.Module, Decl.Module, moduleName, handle -> Module.Module({
            ac = ac,
            qualifier = visitQualifiers(coerce(name)#qualifiers),
            handle = handle,
            scope = inSubScope(Namespace.Module(coerce(moduleName)#s), () -> {
                visitScope(scope)
            }),
            loc = loc
        }))

    def visitAlias(aliass: ParsedAst.TypeAlias): ScopeGraph.TypeAlias \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            loc
        } = coerce(aliass);
        declare(Kind.Tpe, Decl.TypeAlias, name, handle -> TypeAlias.TypeAlias({
            ac = ac,
            handle = handle,
            args = args,
            tpe = inSubScope(Namespace.TypeAlias(coerce(name)#s), () -> {
                foreach(arg <- args) {
                    discard(declare(Kind.Var, Decl.TpeVar, arg, argHandle -> argHandle))
                };
                let boundVars = args
                    |> List.map(arg -> coerce(arg)#s)
                    |> List.toSet;
                let (_, sgTpe) = visitTpe(boundVars, tpe);
                sgTpe
            }),
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
        let enumName = coerce(name)#s;
        declare(Kind.Tpe, Decl.Enum, name, handle -> Enum.Enum({
            ac = ac,
            handle = handle,
            args = inSubScope(Namespace.Enum(enumName), () -> {
                // TODO: Should we put Handle in the AST instead of locstring directly???
                let _ = args |> List.forEach(arg -> {
                    discard(declare(Kind.Var, Decl.TpeVar, arg, argHandle -> argHandle))
                });
                args
            }),
            cases = inSubScope(Namespace.Enum(enumName), () -> {
                cases |> List.map(visitEnumCase)
            }),
            loc = loc
        }))

    def visitEnumCase(enumCase: ParsedAst.EnumCase): ScopeGraph.EnumCase \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            terms,
            loc
        } = coerce(enumCase);
        let (_, newTerms) = visitTpes(Set.empty(), terms);
        // TODO: should this be in parent scope? I think maybe not...
        // do we force Enum.Case in the same module, unless there is an import?
        inParentScope(() -> {
            declare(Kind.Var, Decl.EnumCase, name, handle -> EnumCase.EnumCase({
                ac = ac,
                handle = handle,
                terms = newTerms,
                loc = loc
            }))
        })

    def visitDef(deff: ParsedAst.Def): ScopeGraph.Def \ CurrentScope + MarkDecl =
        let {
            ac,
            name,
            args,
            tpe,
            exp,
            loc
        } = coerce(deff);
        let (newArgs, newTpe, newExp) = inSubScope(Namespace.Def(coerce(name)#s), () -> {
            let (argVars, argTpes) = args |> List.unzip;
            foreach(var <- argVars) {
                // TODO: skeptical of markDecls that don't get wrapped in AST
                markDecl(Decl.Var(Handle.Handle(Kind.Var, var, CurrentScope.get())))
            };
            let (boundVars, newArgTpes) = visitTpes(Set.empty(), argTpes);
            let newArgs = List.zip(argVars, newArgTpes);
            let (_, newTpe) = visitTpe(boundVars, tpe);
            let newExp = visitExp(exp);
            (newArgs, newTpe, newExp)
        });
        declare(Kind.Var, Decl.Def, name, handle -> Def.Def({
            ac = ac,
            handle = handle,
            args = newArgs,
            tpe = newTpe,
            exp = newExp,
            loc = loc
        }))

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
                        let handle = Handle.Handle(Kind.Var, locId, CurrentScope.get());
                        // TODO: skeptical of markDecls that don't get wrapped in AST
                        markDecl(Decl.TpeVar(handle));
                        (boundVars |> Set.insert(id), ScopeGraph.Tpe.FreeVar(handle))
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
                let newRest = inSubScope(Namespace.Let(coerce(id)#s), () -> {
                    // TODO: skeptical of markDecls that don't get wrapped in AST
                    markDecl(Decl.Var(Handle.Handle(Kind.Var, id, CurrentScope.get())));
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
            case ParsedAst.Exp.Name(name, loc) => ScopeGraph.Exp.Refr(visitName(Kind.Var, name), loc)
            case ParsedAst.Exp.Int(i, loc) => ScopeGraph.Exp.Int(i, loc)
            case ParsedAst.Exp.Bool(b, loc) => ScopeGraph.Exp.Bool(b, loc)
        }

    // TODO: Bug! if we visit an enum case, shouldn't the previous qualifier be an enum, not a module?
    def visitName(kind: Kind, name: ParsedAst.Name): ScopeGraph.Refr \ CurrentScope =
        let { qualifiers, name = id, loc = _ } = coerce(name);
        let namespace = CurrentScope.get();
        let newQualifiers = visitQualifiers(qualifiers);
        Refr.Refr(kind, newQualifiers, id, namespace)

    def visitQualifiers(qualifiers: List[LocString]): Option[Refr] \ CurrentScope =
        let namespace = CurrentScope.get();
        qualifiers |> List.foldLeft(q -> m -> Some(Refr.Refr(Kind.Module, q, m, namespace)), None)

    def declare(kind: Kind, cons: t -> Decl, loc: LocString, mkDecl: Handle -> t \ ef): t \ ef + CurrentScope + MarkDecl =
        CurrentScope.get()
            |> Handle.Handle(kind, loc)
            |> mkDecl
            !> (cons >> markDecl)

    def inSubScope(enter: Namespace -> Namespace, f: Unit -> t \ ef): t \ ef + CurrentScope =
        CurrentScope.enter(enter);
        let t = f();
        CurrentScope.exit();
        t

    def inParentScope(f: Unit -> t \ ef): t \ ef + CurrentScope =
        let child = CurrentScope.exit();
        let t = f();
        CurrentScope.enter(_ -> child);
        t

}
