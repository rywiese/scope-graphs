mod ScopeGraph.Ast.Util.Finder {
    import java.lang.System

    use ScopeGraph.Ast.ParsedAst.Exp
    use ScopeGraph.Ast.ParsedAst.Tpe
    use ScopeGraph.Ast.ParsedAst.Def
    use ScopeGraph.Ast.ParsedAst.EnumCase
    use ScopeGraph.Ast.ParsedAst.Enum
    use ScopeGraph.Ast.ParsedAst.TypeAlias
    use ScopeGraph.Ast.ParsedAst.Name
    use ScopeGraph.Ast.ParsedAst.Module
    use ScopeGraph.Ast.ParsedAst.Use
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Ast.Shared.Location.Located.getLoc
    use ScopeGraph.Ast.ParsedAst.AstNode
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Shared.Location.Loc

    pub def find(loc: Loc, scope: ParsedAst.Scope): Option[AstNode] = {
        (loc, scope) ||> ifContained(() -> {
            let {
                uses,
                modules,
                aliases,
                enums,
                defs,
                loc = _
            } = coerce(scope);
            uses |> getFirst(findUse(loc)) |> orElse(() ->
                modules |> getFirst(findModule(loc)) |> orElse(() ->
                    aliases |> getFirst(findAlias(loc)) |> orElse(() ->
                        enums |> getFirst(findEnum(loc)) |> orElse(() ->
                            defs |> getFirst(findDef(loc))
            ))))
        })
    }

    def findName(loc: Loc, name: Name): Option[AstNode] = {
        (loc, name) ||> ifContained(() ->
            Some(AstNode.Name(name))
        )
    }

    def findUse(loc: Loc, usage: Use): Option[AstNode] = {
        (loc, usage) ||> ifContained(() -> {
            let {
                ac = _,
                name,
                renaming = _,
                loc = _
            } = coerce(usage);
            name |> findName(loc) |> orElse(() ->
                Some(AstNode.Use(usage))
            )
        })
    }

    def findModule(loc: Loc, module: Module): Option[AstNode] = {
        (loc, module) ||> ifContained(() -> {
            let {
                ac = _,
                name,
                scope,
                loc = _
            } = coerce(module);
            name |> findName(loc) |> orElse(() ->
                scope |> find(loc) |> orElse(() ->
                    Some(AstNode.Module(module))
                )
            )
        })
    }

    def findAlias(loc: Loc, ta: TypeAlias): Option[AstNode] = {
        (loc, ta) ||> ifContained(() -> {
            let {
                ac = _,
                name = _,
                args = _,
                tpe,
                loc = _
            } = coerce(ta);
            tpe |> findTpe(loc) |> orElse(() ->
                Some(AstNode.TypeAlias(ta))
            )
        })
    }

    def findEnum(loc: Loc, e: Enum): Option[AstNode] = {
        (loc, e) ||> ifContained(() -> {
            let {
                ac = _,
                name = _,
                args = _,
                cases,
                loc = _
            } = coerce(e);
            cases |> getFirst(findCase(loc)) |> orElse(() ->
                Some(AstNode.Enum(e))
            )
        })
    }

    def findCase(loc: Loc, c: EnumCase): Option[AstNode] = {
        (loc, c) ||> ifContained(() -> {
            let {
                ac = _,
                name = _,
                terms,
                loc = _
            } = coerce(c);
            terms |> getFirst(findTpe(loc)) |> orElse(() ->
                Some(AstNode.EnumCase(c))
            )
        })
    }

    def findDef(loc: Loc, d: Def): Option[AstNode] = {
        (loc, d) ||> ifContained(() -> {
            let {
                ac = _,
                name = _,
                args,
                tpe,
                exp,
                loc = _
            } = coerce(d);
            args |> getFirst(snd >> findTpe(loc)) |> orElse(() ->
                tpe |> findTpe(loc) |> orElse(() ->
                    exp |> findExp(loc) |> orElse(() ->
                        Some(AstNode.Def(d))
                    )
                )
            )
        })
    }

    def findTpe(loc: Loc, tpe: Tpe): Option[AstNode] = {
        (loc, tpe) ||> ifContained(() -> {
            match tpe {
                case Tpe.Base(_, _) =>
                    Some(AstNode.Tpe(tpe))
                case Tpe.App(t1, t2, _) =>
                    (t1 :: t2) |> getFirst(findTpe(loc)) |> orElse(() ->
                        Some(AstNode.Tpe(tpe))
                    )
            }
        })
    }

    def findExp(loc: Loc, exp: Exp): Option[AstNode] = {
        (loc, exp) ||> ifContained(() -> {
            match exp {
                case Exp.Let(_, e1, e2, _) =>
                    List#{e1, e2} |> getFirst(findExp(loc)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.Stmt(e1, e2, _) =>
                    List#{e1, e2} |> getFirst(findExp(loc)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.App(e1, e2, _) =>
                    (e1 :: e2) |> getFirst(findExp(loc)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.Name(name, _) =>
                    name |> findName(loc) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.Int(_, _) =>
                    Some(AstNode.Exp(exp))
                case Exp.Bool(_, _) =>
                    Some(AstNode.Exp(exp))
            }
        })
    }

    def contains(loc: Loc, thing: thing): Bool with Located[thing] = {
        Loc.containedIn(thing |> getLoc, loc)
    }

    def ifContained(f: Unit -> Option[t] \ ef, loc: Loc, thing: thing): Option[t] \ ef with Located[thing] = {
        if (contains(loc, thing)) checked_ecast(unsafely IO run {
            System.err.println(thing |> getLoc |> ToString.toString);
            f()
        }) else None
    }

    def getFirst(f: a -> Option[b] \ ef, l: List[a]): Option[b] \ ef = {
        match l {
            case x :: xs => let q = f(x); match q {
                case Some(_) => q
                case None => getFirst(f, xs)
            }
            case Nil => None
        }
    }

    def orElse(f: Unit -> Option[t] \ ef, opt: Option[t]): Option[t] \ ef = {
        match opt {
            case Some(_) => opt
            case None => f()
        }
    }

}
