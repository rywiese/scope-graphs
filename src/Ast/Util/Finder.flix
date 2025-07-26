mod SG.Ast.Util.Finder {
    import java.lang.System

    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ParsedAst.Def
    use SG.Ast.ParsedAst.Enum
    use SG.Ast.ParsedAst.EnumCase
    use SG.Ast.ParsedAst.Exp
    use SG.Ast.ParsedAst.Module
    use SG.Ast.ParsedAst.Name
    use SG.Ast.ParsedAst.Tpe
    use SG.Ast.ParsedAst.TypeAlias
    use SG.Ast.ParsedAst.Use
    use SG.Ast.Shared.Location.Loc
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.Located.getLoc

    // worth formalizing what this does
    // finds the smallest/leafiest node that this loc is contained by?
    pub def find(loc: Loc, visitor: AstNode -> Unit \ ef, ast: ParsedAst): Option[AstNode] \ ef = {
        ast#m |> Map.get(coerce(loc)#src) |> Option.flatMap(findScope(loc, visitor))
    }

    def findScope(loc: Loc, visitor: AstNode -> Unit \ ef, scope: ParsedAst.Scope): Option[AstNode] \ ef = {
        (loc, scope) ||> ifContained(() -> {
            visitor(AstNode.Scope(scope));
            let {
                uses,
                modules,
                aliases,
                enums,
                defs,
                loc = _
            } = coerce(scope);
            uses |> getFirst(findUse(loc, visitor)) |> orElse(() ->
                modules |> getFirst(findModule(loc, visitor)) |> orElse(() ->
                    aliases |> getFirst(findAlias(loc, visitor)) |> orElse(() ->
                        enums |> getFirst(findEnum(loc, visitor)) |> orElse(() ->
                            defs |> getFirst(findDef(loc, visitor)) |> orElse(() ->
                                Some(AstNode.Scope(scope))
                            )
            ))))
        })
    }

    def findName(loc: Loc, visitor: AstNode -> Unit \ ef, name: Name): Option[AstNode] \ ef = {
        (loc, name) ||> ifContained(() -> {
            visitor(AstNode.Name(name));
            Some(AstNode.Name(name))
        })
    }

    def findUse(loc: Loc, visitor: AstNode -> Unit \ ef, usage: Use): Option[AstNode] \ ef = {
        (loc, usage) ||> ifContained(() -> {
            visitor(AstNode.Use(usage));
            let {
                ac = _,
                name,
                renaming = _,
                loc = _
            } = coerce(usage);
            name |> findName(loc, visitor) |> orElse(() ->
                Some(AstNode.Use(usage))
            )
        })
    }

    def findModule(loc: Loc, visitor: AstNode -> Unit \ ef, module: Module): Option[AstNode] \ ef = {
        (loc, module) ||> ifContained(() -> {
            visitor(AstNode.Module(module));
            let {
                ac = _,
                name,
                scope,
                loc = _
            } = coerce(module);
            name |> findName(loc, visitor) |> orElse(() ->
                scope |> findScope(loc, visitor) |> orElse(() ->
                    Some(AstNode.Module(module))
                )
            )
        })
    }

    def findAlias(loc: Loc, visitor: AstNode -> Unit \ ef, ta: TypeAlias): Option[AstNode] \ ef = {
        (loc, ta) ||> ifContained(() -> {
            visitor(AstNode.TypeAlias(ta));
            let {
                ac = _,
                name = _,
                args,
                tpe,
                loc = _
            } = coerce(ta);
            tpe |> findTpe(loc, visitor) |> orElse(() ->
                args
                    |> List.filter(contains(loc))
                    |> List.head
                    |> Option.map(str ->
                        AstNode.Name(Name.Name({
                            qualifiers = List.empty(),
                            name = str,
                            loc = coerce(str)#loc
                        }))
                    )
                    |> orElse(() -> Some(AstNode.TypeAlias(ta))
            ))
        })
    }

    def findEnum(loc: Loc, visitor: AstNode -> Unit \ ef, e: Enum): Option[AstNode] \ ef = {
        (loc, e) ||> ifContained(() -> {
            visitor(AstNode.Enum(e));
            let {
                ac = _,
                name = _,
                args = _,
                cases,
                loc = _
            } = coerce(e);
            cases |> getFirst(findCase(loc, visitor)) |> orElse(() ->
                Some(AstNode.Enum(e))
            )
        })
    }

    def findCase(loc: Loc, visitor: AstNode -> Unit \ ef, c: EnumCase): Option[AstNode] \ ef = {
        (loc, c) ||> ifContained(() -> {
            visitor(AstNode.EnumCase(c));
            let {
                ac = _,
                name = _,
                terms,
                loc = _
            } = coerce(c);
            terms |> getFirst(findTpe(loc, visitor)) |> orElse(() ->
                Some(AstNode.EnumCase(c))
            )
        })
    }

    def findDef(loc: Loc, visitor: AstNode -> Unit \ ef, d: Def): Option[AstNode] \ ef = {
        (loc, d) ||> ifContained(() -> {
            visitor(AstNode.Def(d));
            let {
                ac = _,
                name = _,
                args,
                tpe,
                exp,
                loc = _
            } = coerce(d);
            args |> getFirst(snd >> findTpe(loc, visitor)) |> orElse(() ->
                tpe |> findTpe(loc, visitor) |> orElse(() ->
                    exp |> findExp(loc, visitor) |> orElse(() ->
                        Some(AstNode.Def(d))
                    )
                )
            )
        })
    }

    def findTpe(loc: Loc, visitor: AstNode -> Unit \ ef, tpe: Tpe): Option[AstNode] \ ef = {
        (loc, tpe) ||> ifContained(() -> {
            visitor(AstNode.Tpe(tpe));
            match tpe {
                case Tpe.Base(_, _) =>
                    Some(AstNode.Tpe(tpe))
                case Tpe.App(t1, t2, _) =>
                    (t1 :: t2) |> getFirst(findTpe(loc, visitor)) |> orElse(() ->
                        Some(AstNode.Tpe(tpe))
                    )
            }
        })
    }

    def findExp(loc: Loc, visitor: AstNode -> Unit \ ef, exp: Exp): Option[AstNode] \ ef = {
        (loc, exp) ||> ifContained(() -> {
            visitor(AstNode.Exp(exp));
            match exp {
                case Exp.Let(_, e1, e2, _) =>
                    List#{e1, e2} |> getFirst(findExp(loc, visitor)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.Stmt(e1, e2, _) =>
                    List#{e1, e2} |> getFirst(findExp(loc, visitor)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.App(e1, e2, _) =>
                    (e1 :: e2) |> getFirst(findExp(loc, visitor)) |> orElse(() ->
                        Some(AstNode.Exp(exp))
                    )
                case Exp.Name(name, _) =>
                    name |> findName(loc, visitor) |> orElse(() ->
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
