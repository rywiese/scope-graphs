mod ScopeGraph.Phase.Sanitizer {
    use Foldable.forEach
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Location.LocString

    pub eff Error {
        def error(msg: String): Unit
    }

    mod Error {
        pub def runUnsafe(prefix: String, f: Unit -> t \ ef): t \ ef - Error =
            run f() with handler Error {
                def error(msg, _) = bug!(prefix + " " + msg)
            }
    }

    pub def sanitize(p: ParsedAst.Scope): Unit \ Error = {
        let {
            uses,
            modules,
            aliases,
            enums,
            defs
        } = coerce(p);
        uses |> forEach(sanitizeUse);
        modules |> forEach(sanitizeModule);
        aliases |> forEach(sanitizeAlias);
        enums |> forEach(sanitizeEnum);
        defs |> forEach(sanitizeDef)
    }

    def sanitizeUse(u: ParsedAst.Use): Unit \ Error = {
        let {ac = _, name, renaming = renamingOpt} = coerce(u);
        sanitizeName(name);
        match renamingOpt {
            case None => ()
            case Some(renaming) =>
                let lastUsed = coerce(name)#name;
                if (LocString.isUpperCase(lastUsed)) {
                    if (not LocString.isUpperCase(renaming)) {
                        err("Cannot rename upper case to lower case", coerce(renaming)#loc)
                    } else ()
                } else if (LocString.isLowerCase(lastUsed)) {
                    if (not LocString.isLowerCase(renaming)) {
                        err("Cannot rename upper case to lower case", coerce(renaming)#loc)
                    } else ()
                } else ()
        }
    }

    def sanitizeName(n: ParsedAst.Name): Unit \ Error = {
        let {qualifiers, name = _} = coerce(n);
        foreach (word <- qualifiers) assertUpper("Qualifiers must be upper case", word)
    }

    def sanitizeModule(m: ParsedAst.Module): Unit \ Error = {
        let {ac = _, name, scope} = coerce(m);
        sanitizeName(name);
        assertUpperName("Modules must be upper case", name);
        sanitize(scope)
    }

    def sanitizeAlias(ta: ParsedAst.TypeAlias): Unit \ Error = {
        let {ac = _, name, args, tpe} = coerce(ta);
        assertUpper("Type aliases must be upper case", name);
        sanitizeTypeParams(args);
        sanitizeTpe(tpe)
    }

    def sanitizeTypeParams(args: List[LocString]): Unit \ Error =
        foreach (arg <- args) assertLower("Type arguments must be lower case", arg)

    def sanitizeTpe(tpe: ParsedAst.Tpe): Unit \ Error = {
        match tpe {
            case ParsedAst.Tpe.Base(base) =>
                sanitizeName(base);
                if (coerce(base)#qualifiers |> List.isEmpty) ()
                else assertUpperName("Qualified Types must be upper case", base)
            case ParsedAst.Tpe.App(base, ts) =>
                sanitizeTpe(base);
                foreach (t <- ts) sanitizeTpe(t)
        }
    }

    def sanitizeEnum(m: ParsedAst.Enum): Unit \ Error = {
        let {ac = _, name, args, cases} = coerce(m);
        assertUpper("Enums must have upper case names", name);
        sanitizeTypeParams(args);
        cases |> forEach(sanitizeCase)
    }

    def sanitizeCase(c: ParsedAst.EnumCase): Unit \ Error = {
        let {ac = _, name, terms} = coerce(c);
        assertUpper("Enum cases must be upper case", name);
        foreach (term <- terms) sanitizeTpe(term)
    }

    def sanitizeDef(m: ParsedAst.Def): Unit \ Error = {
        let {ac = _, name, args, tpe, exp} = coerce(m);
        assertLower("defs must be lower case", name);
        foreach ((argName, argTpe) <- args) {
            assertLower("Parameters must be lower case", argName);
            sanitizeTpe(argTpe)
        };
        sanitizeTpe(tpe);
        sanitizeExp(exp)
    }

    def sanitizeExp(exp0: ParsedAst.Exp): Unit \ Error = {
        match exp0 {
            case ParsedAst.Exp.Let(name, exp1, exp2) =>
                assertLower("let-binders must be lower case", name);
                sanitizeExp(exp1);
                sanitizeExp(exp2)
            case ParsedAst.Exp.Stmt(exp1, exp2) =>
                sanitizeExp(exp1);
                sanitizeExp(exp2)
            case ParsedAst.Exp.App(exp, exps) =>
                sanitizeExp(exp);
                exps |> forEach(sanitizeExp)
            case ParsedAst.Exp.Name(name) =>
                sanitizeName(name)
            case ParsedAst.Exp.Int(_) =>
                ()
            case ParsedAst.Exp.Bool(_) =>
                ()
        }
    }

    def assertUpperName(msg: String, name: ParsedAst.Name): Unit \ Error =
        assertUpper(msg, coerce(name)#name)

    def assertUpper(msg: String, s: LocString): Unit \ Error = {
        if (not LocString.isUpperCase(s)) err("${msg}: '${s}'", coerce(s)#loc)
        else ()
    }

    def assertLower(msg: String, s: LocString): Unit \ Error = {
        if (not LocString.isLowerCase(s)) err("${msg}: '${s}'", coerce(s)#loc)
        else ()
    }

    def err(msg: String, loc: Location.LocRange): Unit \ Error = {
        Error.error("${msg} (${loc})")
    }

}
