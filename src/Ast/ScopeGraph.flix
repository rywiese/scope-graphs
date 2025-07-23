mod SG.Ast.ScopeGraph {
    use Hash.combine
    use Hash.hash
    use Order.compare
    use SG.Ast.ScopeGraph.Enum
    use SG.Ast.Shared.Access
    use SG.Ast.Shared.EnumCase
    use SG.Ast.Shared.Exp
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Name
    use SG.Ast.Shared.Tpe
    use ToString.toString

    pub type alias ScopedAst = {
        top = Scope, // maybe this should be a list as well
        edges = MultiMap[Node, Edge]
    }

    pub enum Scope({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def],
        node = Node
    })

    pub enum Use({
        ac = Access,
        name = Name,
        renaming = Option[LocString],
        loc = LocRange,
        parent = Node
    })

    pub enum Module({
        ac = Access,
        loc = LocRange,
        scope = Scope,
        parent = Node
    })

    pub enum TypeAlias({
        ac = Access,
        name = LocString,
        args = List[LocString],
        tpe = Tpe,
        loc = LocRange,
        parent = Node
    })

    pub enum Enum({
        ac = Access,
        name = LocString,
        args = List[LocString],
        cases = List[EnumCase],
        loc = LocRange,
        parent = Node
    })

    pub enum Def({
        ac = Access,
        name = LocString,
        args = List[(LocString, Tpe)],
        tpe = Tpe,
        exp = Exp,
        loc = LocRange,
        parent = Node
    })

    pub enum Node {
        case Top
        case Named({ qualifiers = List[String], name = String })
    }

    pub enum Edge {
        case Parent({ parent = Node })
        case Use({ id = Option[String], node = Node })
        case Module({ module = Module })
        case TypeAlias({ typeAlias = TypeAlias })
        case Enum({ eenum = Enum })
        case Def({ deff = Def })
    }

    instance SemiGroup[Scope] {
        // TODO: maybe rethink this?
        pub def combine(x: Scope, y: Scope): Scope =
            let scope1 = coerce(x);
            let scope2 = coerce(y);
            Scope.Scope({
                uses = scope1#uses ++ scope2#uses,
                modules = scope1#modules ++ scope2#modules,
                aliases = scope1#aliases ++ scope2#aliases,
                enums = scope1#enums ++ scope2#enums,
                defs = scope1#defs ++ scope2#defs,
                node = if (scope1#node == scope2#node)
                    scope1#node
                else
                    bug!("Cannot combine nodes of different scopes")
            })
    }

    instance Monoid[Scope] {    
        pub def empty(_unit: Unit): Scope = Scope.Scope({
            uses = Nil,
            modules = Nil,
            aliases = Nil,
            enums = Nil,
            defs = Nil,
            node = Node.Top
        })
    }

    instance Coerce[Scope] {

        type Out = {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            node = Node
        }

        pub def coerce(x: Scope): {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            node = Node
        } =
            let Scope.Scope(r) = x;
            r

    }

    instance Hash[Scope] {
        pub def hash(x: Scope): Int32 =
            let { uses, modules, aliases, enums, defs, node } = coerce(x);
            hash("Scope")
                |> combine(hash(uses))
                |> combine(hash(modules))
                |> combine(hash(aliases))
                |> combine(hash(enums))
                |> combine(hash(defs))
                |> combine(hash(node))
    }

    instance Coerce[Use] {

        type Out = {
            ac = Access,
            name = Name,
            renaming = Option[LocString],
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: Use): {
            ac = Access,
            name = Name,
            renaming = Option[LocString],
            loc = LocRange,
            parent = Node
        } =
            let Use.Use(r) = x;
            r

    }

    instance Hash[Use] {
        pub def hash(x: Use): Int32 =
            let { ac, name, renaming, loc, parent } = coerce(x);
            hash("Scope")
                |> combine(hash(ac))
                |> combine(hash(name))
                |> combine(hash(renaming))
                |> combine(hash(loc))
                |> combine(hash(parent))
    }

    instance Coerce[Module] {

        type Out = {
            ac = Access,
            loc = LocRange,
            scope = Scope,
            parent = Node
        }

        pub def coerce(x: Module): {
            ac = Access,
            loc = LocRange,
            scope = Scope,
            parent = Node
        } =
            let Module.Module(r) = x;
            r

    }

    instance Hash[Module] {
        pub def hash(x: Module): Int32 =
            let { ac, loc, scope, parent } = coerce(x);
            hash("Module")
                |> combine(hash(ac))
                |> combine(hash(loc))
                |> combine(hash(scope))
                |> combine(hash(parent))
    }

    instance Coerce[TypeAlias] {

        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: TypeAlias): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange,
            parent = Node
        } =
            let TypeAlias.TypeAlias(r) = x;
            r

    }

    instance Hash[TypeAlias] {
        pub def hash(x: TypeAlias): Int32 =
            let { ac, name, args, tpe, loc, parent } = coerce(x);
            hash("TypeAlias")
                |> combine(hash(ac))
                |> combine(hash(name))
                |> combine(hash(args))
                |> combine(hash(tpe))
                |> combine(hash(loc))
                |> combine(hash(parent))
    }

    instance Coerce[Enum] {

        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: Enum): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange,
            parent = Node
        } =
            let Enum.Enum(r) = x;
            r

    }

    instance Hash[Enum] {
        pub def hash(x: Enum): Int32 =
            let { ac, name, args, cases, loc, parent } = coerce(x);
            hash("Enum")
                |> combine(hash(ac))
                |> combine(hash(name))
                |> combine(hash(args))
                |> combine(hash(cases))
                |> combine(hash(loc))
                |> combine(hash(parent))
    }

    instance Coerce[Def] {

        type Out = {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: Def): {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange,
            parent = Node
        } =
            let Def.Def(r) = x;
            r

    }

    instance Hash[Def] {
        pub def hash(x: Def): Int32 =
            let { ac, name, args, tpe, exp, loc, parent } = coerce(x);
            hash("Def")
                |> combine(hash(ac))
                |> combine(hash(name))
                |> combine(hash(args))
                |> combine(hash(tpe))
                |> combine(hash(exp))
                |> combine(hash(loc))
                |> combine(hash(parent))
    }

    mod Node {
        pub def fullyQualifiedName(node: Node): String =
            match node {
                case Top => ""
                case Named({ qualifiers, name }) =>
                    (qualifiers ++ (name :: Nil)) |> String.intercalateChar('.')
            }
    }

    instance Hash[Node] {
        pub def hash(x: Node): Int32 =
            match x {
                case Node.Top => hash("Top")
                case Node.Named({ qualifiers, name }) => hash(qualifiers) |> combine(hash(name))
            }
    }

    instance Eq[Node] {
        pub def eq(x: Node, y: Node): Bool =
            Node.fullyQualifiedName(x) == Node.fullyQualifiedName(y)
    }

    instance Order[Node] {
        pub def compare(x: Node, y: Node): Comparison =
            compare(Node.fullyQualifiedName(x), Node.fullyQualifiedName(y))
    }

    instance Hash[Edge] {
        pub def hash(x: Edge): Int32 =
            match x {
                case Edge.Parent({ parent }) => hash("Parent") |> combine(hash(parent))
                case Edge.Use({ id, node }) => hash("Use") |> combine(hash(id)) |> combine(hash(node))
                case Edge.Module({ module }) => hash("Module") |> combine(hash(module))
                case Edge.TypeAlias({ typeAlias }) => hash("TypeAlias") |> combine(hash(typeAlias))
                case Edge.Enum({ eenum  }) => hash("Enum") |> combine(hash(eenum))
                case Edge.Def({ deff  }) => hash("Def") |> combine(hash(deff))
            }
    }

    instance Eq[Edge] {
        pub def eq(x: Edge, y: Edge): Bool = hash(x) == hash(y)
    }

    instance Order[Edge] {
        pub def compare(x: Edge, y: Edge): Comparison = compare(hash(x), hash(y))
    }

}
