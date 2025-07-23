mod SG.Ast {
    use SG.Ast.ParsedAst.Scope
    use SG.Ast.Shared.Location.Source

    pub type alias ParsedAst = {
        m = Map[Source, Scope]
    }
}

mod SG.Ast.ParsedAst {
    use SG.Ast.Shared.Access
    use SG.Ast.Shared.EnumCase
    use SG.Ast.Shared.Exp
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.Source
    use SG.Ast.Shared.Name
    use SG.Ast.Shared.Tpe

    pub enum Scope({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def],
        loc = LocRange
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
        cases = List[EnumCase],
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

    pub enum AstNode {
        case Scope(Scope)
        case Use(Use)
        case Module(Module)
        case TypeAlias(TypeAlias)
        case Enum(Enum)
        case EnumCase(EnumCase)
        case Def(Def)
        case Exp(Exp)
        case Name(Name)
        case Tpe(Tpe)
    }



    //
    // Better Constructors.
    //


    pub def mkScope(
        uses: List[Use],
        modules: List[Module],
        aliases: List[TypeAlias],
        enums: List[Enum],
        defs: List[Def],
        loc: LocRange
    ): Scope =
        Scope.Scope({
            uses = uses,
            modules = modules,
            aliases = aliases,
            enums = enums,
            defs = defs,
            loc = loc
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
        cases: List[EnumCase],
        loc: LocRange
    ): Enum =
        Enum.Enum({ac = ac, name = name, args = args, cases = cases, loc = loc})

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
            defs = List[Def],
            loc = LocRange
        }

        pub def coerce(x: Scope): {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            loc = LocRange
        } = {
            let Scope.Scope(r) = x;
            r
        }
    }

    instance Located[Scope] {
        pub def getLoc(x: Scope): LocRange =
            coerce(x)#loc
    }

    instance Coerce[Module] {
        type Out = {ac = Access, name = Name, scope = Scope, loc = LocRange}

        pub def coerce(x: Module): {ac = Access, name = Name, scope = Scope, loc = LocRange} = {
            let Module.Module(r) = x;
            r
        }
    }

    instance Located[Module] {
        pub def getLoc(x: Module): LocRange =
            coerce(x)#loc
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

    instance Located[TypeAlias] {
        pub def getLoc(x: TypeAlias): LocRange =
            coerce(x)#loc
    }

    instance Coerce[Enum] {
        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange
        }

        pub def coerce(x: Enum): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange
        } = {
            let Enum.Enum(r) = x;
            r
        }
    }

    instance Located[Enum] {
        pub def getLoc(x: Enum): LocRange =
            coerce(x)#loc
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

    instance Located[AstNode] {
        pub def getLoc(x: AstNode): LocRange = match x {
            case AstNode.Scope(y) => y |> Located.getLoc
            case AstNode.Use(y) => y |> Located.getLoc
            case AstNode.Module(y) => y |> Located.getLoc
            case AstNode.TypeAlias(y) => y |> Located.getLoc
            case AstNode.Enum(y) => y |> Located.getLoc
            case AstNode.EnumCase(y) => y |> Located.getLoc
            case AstNode.Def(y) => y |> Located.getLoc
            case AstNode.Exp(y) => y |> Located.getLoc
            case AstNode.Name(y) => y |> Located.getLoc
            case AstNode.Tpe(y) => y |> Located.getLoc
        }
    }

}
