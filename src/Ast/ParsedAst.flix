mod ScopeGraph.Ast.ParsedAst {
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Ast.Shared.Location.LocString
    use ScopeGraph.Ast.Shared.Location.LocRange
    use ScopeGraph.Ast.Shared.Location

    pub enum Scope({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def]
    })

    pub enum Use({
        ac = Access,
        name = Name,
        renaming = Option[LocString],
        loc = LocRange
    })

    pub enum Module({
        ac = Access,
        name = Name,
        scope = Scope,
        loc = LocRange
    })

    pub enum TypeAlias({
        ac = Access,
        name = LocString,
        args = List[LocString],
        tpe = Tpe,
        loc = LocRange
    })

    pub enum Enum({
        ac = Access,
        name = LocString,
        args = List[LocString],
        cases = List[EnumCase]
    })

    pub enum EnumCase({
        ac = Access,
        name = LocString,
        terms = List[Tpe],
        loc = LocRange
    })

    pub enum Def({
        ac = Access,
        name = LocString,
        args = List[(LocString, Tpe)],
        tpe = Tpe,
        exp = Exp,
        loc = LocRange
    })

    pub enum Exp {
        case Let(LocString, Exp, Exp, LocRange)
        case Stmt(Exp, Exp, LocRange)
        case App(Exp, List[Exp], LocRange)
        case Name(Name, LocRange)
        case Int(Int32, LocRange)
        case Bool(Bool, LocRange)
    }

    pub enum Access with Eq, ToString {
        case Public
        case Private
    }

    pub enum Name({
        qualifiers = List[LocString],
        name = LocString,
        loc = LocRange
    })

    pub enum Tpe {
        case Base(Name, LocRange)
        case App(Tpe, List[Tpe], LocRange)
    }



    //
    // Better Constructors.
    //


    pub def mkScope(
        uses: List[Use],
        modules: List[Module],
        aliases: List[TypeAlias],
        enums: List[Enum],
        defs: List[Def]
    ): Scope =
        Scope.Scope({
            uses = uses,
            modules = modules,
            aliases = aliases,
            enums = enums,
            defs = defs
        })

    pub def mkUse(ac: Access, name: Name, loc: LocRange): Use =
        Use.Use({ac = ac, name = name, renaming = None, loc = loc})

    pub def mkRenamedUse(ac: Access, name: Name, renaming: LocString, loc: LocRange): Use =
        Use.Use({ac = ac, name = name, renaming = Some(renaming), loc = loc})

    pub def mkModule(ac: Access, name: Name, scope: Scope, loc: LocRange): Module =
        Module.Module({ac = ac, name = name, scope = scope, loc = loc})

    pub def mkTypeAlias(
        ac: Access,
        name: LocString,
        args: List[LocString],
        tpe: Tpe,
        loc: LocRange
    ): TypeAlias =
        TypeAlias.TypeAlias({ac = ac, name = name, args = args, tpe = tpe, loc = loc})

    pub def mkEnum(
        ac: Access,
        name: LocString,
        args: List[LocString],
        cases: List[EnumCase]
    ): Enum =
        Enum.Enum({ac = ac, name = name, args = args, cases = cases})

    pub def mkEnumCase(ac: Access, name: LocString, terms: List[Tpe], loc: LocRange): EnumCase =
        EnumCase.EnumCase({ac = ac, name = name, terms = terms, loc = loc})

    pub def mkDef(
        ac: Access,
        name: LocString,
        args: List[(LocString, Tpe)],
        tpe: Tpe,
        exp: Exp,
        loc: LocRange
    ): Def =
        Def.Def({ac = ac, name = name, args = args, tpe = tpe, exp = exp, loc = loc})

    pub def mkName(qualifiers: List[LocString], name: LocString): Name = {
        let first = qualifiers |> List.head |> Option.getWithDefault(name);
        let loc = Location.union(first |> Located.getLoc, name |> Located.getLoc);
        Name.Name({qualifiers = qualifiers, name = name, loc = loc})
    }






    //
    // Instances etc.
    //




    instance Coerce[Scope] {
        type Out = {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def]
        }

        pub def coerce(x: Scope): {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def]
        } = {
            let Scope.Scope(r) = x;
            r
        }
    }

    instance Coerce[Module] {
        type Out = {ac = Access, name = Name, scope = Scope, loc = LocRange}

        pub def coerce(x: Module): {ac = Access, name = Name, scope = Scope, loc = LocRange} = {
            let Module.Module(r) = x;
            r
        }
    }

    instance Coerce[Name] {
        type Out = {qualifiers = List[LocString], name = LocString, loc = LocRange}

        pub def coerce(x: Name): {qualifiers = List[LocString], name = LocString, loc = LocRange} = {
            let Name.Name(r) = x;
            r
        }
    }

    instance Eq[Name] {
        pub def eq(x: Name, y: Name): Bool = {
            let {qualifiers = qualifiersX, name = nameX, loc = _} = coerce(x);
            let {qualifiers = qualifiersY, name = nameY, loc = _} = coerce(y);
            qualifiersX == qualifiersY and nameX == nameY
        }
    }

    instance Located[Name] {
        pub def getLoc(x: Name): LocRange =
            coerce(x)#loc
    }


    instance ToString[Name] {
        pub def toString(x: Name): String =
            Name.toString(x)
    }

    mod Name {
        pub def toString(n: Name): String = {
            let {qualifiers, name, loc = _} = coerce(n);
            List.join(".", qualifiers `List.append` List#{name})
        }
    }

    instance Coerce[Use] {
        type Out = {ac = Access, name = Name, renaming = Option[LocString], loc = LocRange}

        pub def coerce(x: Use): {ac = Access, name = Name, renaming = Option[LocString], loc = LocRange} = {
            let Use.Use(r) = x;
            r
        }
    }

    instance Located[Use] {
        pub def getLoc(x: Use): LocRange =
            coerce(x)#loc
    }

    instance Coerce[TypeAlias] {
        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange
        }

        pub def coerce(x: TypeAlias): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange
        } = {
            let TypeAlias.TypeAlias(r) = x;
            r
        }
    }

    instance Coerce[Enum] {
        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase]
        }

        pub def coerce(x: Enum): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase]
        } = {
            let Enum.Enum(r) = x;
            r
        }
    }

    instance Coerce[EnumCase] {
        type Out = {
            ac = Access,
            name = LocString,
            terms = List[Tpe],
            loc = LocRange
        }

        pub def coerce(x: EnumCase): {
            ac = Access,
            name = LocString,
            terms = List[Tpe],
            loc = LocRange
        } = {
            let EnumCase.EnumCase(r) = x;
            r
        }
    }

    instance Coerce[Def] {
        type Out = {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        }

        pub def coerce(x: Def): {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        } = {
            let Def.Def(r) = x;
            r
        }
    }

    instance Located[Def] {
        pub def getLoc(x: Def): LocRange =
            coerce(x)#loc
    }


    mod Exp {
        use ScopeGraph.Ast.Shared.Location.LocRange

        pub def getLoc(exp: Exp): LocRange = match exp {
            case Let(_, _, _, loc) => loc
            case Stmt(_, _, loc) => loc
            case App(_, _, loc) => loc
            case Name(_, loc) => loc
            case Int(_, loc) => loc
            case Bool(_, loc) => loc
        }
    }

    instance Located[Exp] {
        pub def getLoc(x: Exp): LocRange =
            Exp.getLoc(x)
    }

    mod Tpe {
        use ScopeGraph.Ast.Shared.Location.LocRange

        pub def getLoc(t: Tpe): LocRange = match t {
            case Base(_, loc) => loc
            case App(_, _, loc) => loc
        }
    }

    instance Located[Tpe] {
        pub def getLoc(x: Tpe): LocRange =
            Tpe.getLoc(x)
    }

}
