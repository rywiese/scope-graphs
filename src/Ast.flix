mod ScopeGraph.Ast {
    use ScopeGraph.Ast.Loc.LocString

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

    pub enum Scope({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def]
    })

    pub def mkUse(ac: Access, name: Name): Use =
        Use.Use({ac = ac, name = name, renaming = None})

    pub def mkRenamedUse(ac: Access, name: Name, renaming: LocString): Use =
        Use.Use({ac = ac, name = name, renaming = Some(renaming)})

    pub enum Use({
        ac = Access,
        name = Name,
        renaming = Option[LocString]
    })

    pub def mkModule(ac: Access, name: Name, scope: Scope): Module =
        Module.Module({ac = ac, name = name, scope = scope})

    pub enum Module({
        ac = Access,
        name = Name,
        scope = Scope
    })

    pub def mkTypeAlias(
        ac: Access,
        name: LocString,
        args: List[LocString],
        tpe: Tpe
    ): TypeAlias =
        TypeAlias.TypeAlias({ac = ac, name = name, args = args, tpe = tpe})

    pub enum TypeAlias({
        ac = Access,
        name = LocString,
        args = List[LocString],
        tpe = Tpe
    })

    pub def mkEnum(
        ac: Access,
        name: LocString,
        args: List[LocString],
        cases: List[EnumCase]
    ): Enum =
        Enum.Enum({ac = ac, name = name, args = args, cases = cases})

    pub enum Enum({
        ac = Access,
        name = LocString,
        args = List[LocString],
        cases = List[EnumCase]
    })

    pub def mkEnumCase(ac: Access, name: LocString, terms: List[Tpe]): EnumCase =
        EnumCase.EnumCase({ac = ac, name = name, terms = terms})

    pub enum EnumCase({
        ac = Access,
        name = LocString,
        terms = List[Tpe]
    })

    pub def mkDef(
        ac: Access,
        name: LocString,
        args: List[(LocString, Tpe)],
        tpe: Tpe,
        exp: Exp
    ): Def =
        Def.Def({ac = ac, name = name, args = args, tpe = tpe, exp = exp})

    pub enum Def({
        ac = Access,
        name = LocString,
        args = List[(LocString, Tpe)],
        tpe = Tpe,
        exp = Exp
    })

    pub enum Exp {
        case Let(LocString, Exp, Exp)
        case Stmt(Exp, Exp)
        case App(Exp, List[Exp])
        case Name(Name)
        case Int(Int32)
    }

    pub enum Access with Eq, ToString {
        case Public
        case Private
    }

    pub def mkName(qualifiers: List[LocString], name: LocString): Name =
        Name.Name({qualifiers = qualifiers, name = name})

    pub enum Name({
        qualifiers = List[LocString],
        name = LocString
    })

    pub enum Tpe {
        case Base(Name)
        case App(Tpe, List[Tpe])
    }





    //
    // Instances etc.
    //





    instance Coerce[Name] {
        type Out = {qualifiers = List[LocString], name = LocString}

        pub def coerce(x: Name): {qualifiers = List[LocString], name = LocString} = {
            let Name.Name(r) = x;
            r
        }
    }

    instance Eq[Name] {
        pub def eq(x: Name, y: Name): Bool = {
            let {qualifiers = qualifiersX, name = nameX} = coerce(x);
            let {qualifiers = qualifiersY, name = nameY} = coerce(y);
            qualifiersX == qualifiersY and nameX == nameY
        }
    }

    instance ToString[Name] {
        pub def toString(x: Name): String =
            Name.toString(x)
    }

    mod Name {
        pub def toString(n: Name): String = {
            let {qualifiers, name} = coerce(n);
            List.join(".", qualifiers `List.append` List#{name})
        }
    }

    instance Coerce[Use] {
        type Out = {ac = Access, name = Name, renaming = Option[LocString]}

        pub def coerce(x: Use): {ac = Access, name = Name, renaming = Option[LocString]} = {
            let Use.Use(r) = x;
            r
        }
    }

    instance Coerce[TypeAlias] {
        type Out = {ac = Access, name = LocString, args = List[LocString], tpe = Tpe}

        pub def coerce(x: TypeAlias): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe
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
        type Out = {ac = Access, name = LocString, terms = List[Tpe]}

        pub def coerce(x: EnumCase): {ac = Access, name = LocString, terms = List[Tpe]} = {
            let EnumCase.EnumCase(r) = x;
            r
        }
    }

    instance Coerce[Def] {
        type Out = {ac = Access, name = LocString, args = List[(LocString, Tpe)], tpe = Tpe, exp = Exp}

        pub def coerce(x: Def): {ac = Access, name = LocString, args = List[(LocString, Tpe)], tpe = Tpe, exp = Exp} = {
            let Def.Def(r) = x;
            r
        }
    }

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
        type Out = {ac = Access, name = Name, scope = Scope}

        pub def coerce(x: Module): {ac = Access, name = Name, scope = Scope} = {
            let Module.Module(r) = x;
            r
        }
    }

}
