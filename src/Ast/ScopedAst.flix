mod SG.Ast {
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Namespace
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.Shared.Location.Source

    pub enum ScopeGraph({
        nodes = Map[Source, Scope],
        edges = Map[Namespace, List[Edge]]
    })
}

mod SG.Ast.ScopeGraph {
    use Comparison.EqualTo
    use Order.compare
    use SG.Ast.Shared.Access
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.Source

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
        usage = Refr,
        renaming = Option[LocString], // handle???
        loc = LocRange
    })

    pub enum Module({
        ac = Access,
        qualifier = Option[Refr],
        handle = Handle,
        scope = Scope,
        loc = LocRange
    })

    pub enum TypeAlias({
        ac = Access,
        handle = Handle,
        args = List[LocString],
        tpe = Tpe,
        loc = LocRange
    })

    pub enum Enum({
        ac = Access,
        handle = Handle,
        args = List[LocString],
        cases = List[EnumCase],
        loc = LocRange
    })

    pub enum EnumCase({
        ac = Access,
        handle = Handle,
        terms = List[Tpe],
        loc = LocRange
    })

    pub enum Def({
        ac = Access,
        handle = Handle,
        args = List[(LocString, Tpe)],
        tpe = Tpe,
        exp = Exp,
        loc = LocRange
    })

    pub enum Exp {
        case Let(LocString, Exp, Exp, LocRange)
        case Stmt(Exp, Exp, LocRange)
        case App(Exp, List[Exp], LocRange)
        case Refr(Refr, LocRange)
        case Int(Int32, LocRange)
        case Bool(Bool, LocRange)
    }

    pub enum Tpe {
        case Base(Refr)
        case BoundVar(Refr)
        case FreeVar(Handle)
        case App(Tpe, List[Tpe], LocRange)
    }

    pub enum Handle(Kind, LocString, Namespace)

    mod Handle {
        use PartialOrder.lessEqual
        use SG.Ast.ScopeGraph.Kind
        use SG.Ast.ScopeGraph.Decl
        use SG.Ast.ScopeGraph.Handle
        use SG.Ast.ScopeGraph.Namespace
        use SG.Ast.ScopeGraph.Refr
        use SG.Ast.Shared.Location.LocString

        pub def handle(decl: Decl): Handle =
            match decl {
                case Decl.Module(module) => coerce(module)#handle
                case Decl.TypeAlias(typeAlias) => coerce(typeAlias)#handle
                case Decl.Enum(eenum) => coerce(eenum)#handle
                case Decl.EnumCase(enumCase) => coerce(enumCase)#handle
                case Decl.Def(deff) => coerce(deff)#handle
                case Decl.TpeVar(handle) => handle
                case Decl.Var(handle) => handle
            }

        pub def matches(refr: Refr, handle: Handle): Bool =
            let Handle.Handle(handleKind, handleName, _) = handle;
            let refrKind = coerce(refr)#kind;
            let refrName = coerce(refr)#name;
            handleKind `lessEqual` refrKind and coerce(handleName)#s == coerce(refrName)#s
            
    }

    pub enum Refr with Eq, Order, ToString {
        case Refr(
            Kind,
            Option[Refr], // qualifier
            LocString, // name
            Namespace // referencedIn (not qualifiers)
        )
    }

    mod Refr {
        use SG.Ast.ScopeGraph.Kind
        use SG.Ast.ScopeGraph.Namespace

        // May be interesting to narrow this down with a regex of sorts
        // Part of the configuration/spec could be a regex of allowed import types
        // Maybe each Refr has a regex??
        pub def possibleInterpretations(refr: Refr): List[Namespace] =
            let { kind, qualifier, name, referencedIn = _ } = coerce(refr);
            let id = coerce(name)#s;
            qualifier
                |> Option.map(possibleInterpretations)
                |> Option.getWithDefault(Namespace.Top :: Nil)
                |> List.flatMap(parent -> match kind {
                    // TODO: Helper functions for both directions of Kind <-> Namespace mapping?
                    case Kind.Module => Namespace.Module(id, parent) :: Nil
                    case Kind.Tpe => Namespace.TypeAlias(id, parent)
                        :: Namespace.Enum(id, parent)
                        :: Nil
                    case Kind.Var => Namespace.Def(id, parent)
                        :: Namespace.Let(id, parent)
                        :: Nil
                    case Kind.Any => Namespace.Module(id, parent)
                        :: Namespace.TypeAlias(id, parent)
                        :: Namespace.Enum(id, parent)
                        :: Namespace.Def(id, parent)
                        :: Namespace.Let(id, parent)
                        :: Nil
                })
    }

    pub enum AstNode {
        case Scope(Scope)
        case Use(Use)
        case Module(Module)
        case TypeAlias(TypeAlias)
        case Enum(Enum)
        case EnumCase(EnumCase)
        case Def(Def)
        case Exp(Exp)
        case Tpe(Tpe)
        case Refr(Refr)
    }

    // Figure out what to call this... type/kind/category?
    // basically a scoping "primitive"
    pub enum Kind with Eq, Order, ToString {
        case Any
        case Module
        case Tpe
        case Var
    }

    pub enum Namespace with Eq, Order, ToString {
        case Top
        case Module(String, Namespace)
        case TypeAlias(String, Namespace)
        case Enum(String, Namespace)
        case Def(String, Namespace)
        case Let(String, Namespace)
    }

    pub enum Decl {
        case Module(Module)
        case TypeAlias(TypeAlias)
        case Enum(Enum)
        case EnumCase(EnumCase)
        case Def(Def)
        case TpeVar(Handle)
        case Var(Handle)
    }

    mod Decl {
        use SG.Ast.ScopeGraph.Decl
        use SG.Ast.ScopeGraph.Handle

        pub def handle(decl: Decl): Handle =
            match decl {
                case Decl.Module(module) => coerce(module)#handle
                case Decl.TypeAlias(typeAlias) => coerce(typeAlias)#handle
                case Decl.Enum(eenum) => coerce(eenum)#handle
                case Decl.EnumCase(enumCase) => coerce(enumCase)#handle
                case Decl.Def(deff) => coerce(deff)#handle
                case Decl.TpeVar(handle) => handle
                case Decl.Var(handle) => handle
            }
            
    }

    pub enum Edge {
        case Lex(Namespace)
        case Use(Namespace)
        case Decl(Decl)
    }

    mod Edge {
        use SG.Ast.ScopeGraph.EdgeType

        pub def edgeType(edge: Edge): EdgeType =
            match edge {
                case Lex(_) => EdgeType.Lex
                case Use(_) => EdgeType.Use
                case Decl(_) => EdgeType.Decl
            }
    }

    // Instances

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
        } =
            let Scope.Scope(r) = x;
            r
    }

    instance Located[Scope] {
        pub def getLoc(x: Scope): LocRange = coerce(x)#loc
    }

    instance Coerce[Use] {
        type Out = {
            ac = Access,
            usage = Refr,
            renaming = Option[LocString],
            loc = LocRange
        }
    
        pub def coerce(x: Use): {
            ac = Access,
            usage = Refr,
            renaming = Option[LocString],
            loc = LocRange
        } =
            let Use.Use(r) = x;
            r
    }

    instance Located[Use] {
        pub def getLoc(x: Use): LocRange = coerce(x)#loc
    }

    instance Coerce[Module] {
        type Out = {
            ac = Access,
            qualifier = Option[Refr],
            handle = Handle,
            scope = Scope,
            loc = LocRange
        }
    
        pub def coerce(x: Module): {
            ac = Access,
            qualifier = Option[Refr],
            handle = Handle,
            scope = Scope,
            loc = LocRange
        } =
            let Module.Module(r) = x;
            r
    }

    instance Located[Module] {
        pub def getLoc(x: Module): LocRange = coerce(x)#loc
    }

    instance Coerce[TypeAlias] {
        type Out = {
            ac = Access,
            handle = Handle,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange
        }
    
        pub def coerce(x: TypeAlias): {
            ac = Access,
            handle = Handle,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange
        } =
            let TypeAlias.TypeAlias(r) = x;
            r
    }

    instance Located[TypeAlias] {
        pub def getLoc(x: TypeAlias): LocRange = coerce(x)#loc
    }

    instance Coerce[Enum] {
        type Out = {
            ac = Access,
            handle = Handle,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange
        }
    
        pub def coerce(x: Enum): {
            ac = Access,
            handle = Handle,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange
        } =
            let Enum.Enum(r) = x;
            r
    }

    instance Located[Enum] {
        pub def getLoc(x: Enum): LocRange = coerce(x)#loc
    }

    instance Coerce[EnumCase] {
        type Out = {
            ac = Access,
            handle = Handle,
            terms = List[Tpe],
            loc = LocRange
        }
    
        pub def coerce(x: EnumCase): {
            ac = Access,
            handle = Handle,
            terms = List[Tpe],
            loc = LocRange
        } =
            let EnumCase.EnumCase(r) = x;
            r
    }

    instance Located[EnumCase] {
        pub def getLoc(x: EnumCase): LocRange = coerce(x)#loc
    }

    instance Coerce[Def] {
        type Out = {
            ac = Access,
            handle = Handle,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        }
    
        pub def coerce(x: Def): {
            ac = Access,
            handle = Handle,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        } =
            let Def.Def(r) = x;
            r
    }

    instance Located[Def] {
        pub def getLoc(x: Def): LocRange = coerce(x)#loc
    }

    instance Located[Tpe] {
        pub def getLoc(x: Tpe): LocRange =
            match x {
                case Tpe.Base(refr) => refr |> Located.getLoc
                case Tpe.BoundVar(refr) => refr |> Located.getLoc
                case Tpe.FreeVar(handle) => handle |> Located.getLoc
                case Tpe.App(_, _, loc) => loc
            }
    }

    instance Located[Exp] {
        pub def getLoc(x: Exp): LocRange =
            match x {
                case Exp.Let(_, _, _, loc) => loc
                case Exp.Stmt(_, _, loc) => loc
                case Exp.App(_, _, loc) => loc
                case Exp.Refr(_, loc) => loc
                case Exp.Int(_, loc) => loc
                case Exp.Bool(_, loc) => loc
            }
    }

    instance Located[Handle] {
        pub def getLoc(x: Handle): LocRange =
            let Handle.Handle(_, name, _) = x;
            Located.getLoc(name)
    }

    instance Coerce[Refr] {
        type Out = {
            kind = Kind,
            qualifier = Option[Refr],
            name = LocString,
            referencedIn = Namespace
        }

        pub def coerce(x: Refr): {
            kind = Kind,
            qualifier = Option[Refr],
            name = LocString,
            referencedIn = Namespace
        } =
            let Refr.Refr(kind, qualifier, name, referencedIn) = x; {
                kind = kind,
                qualifier = qualifier,
                name = name,
                referencedIn = referencedIn
            }
    }

    instance Located[Refr] {
        pub def getLoc(x: Refr): LocRange =
            coerce(x)#name |> Located.getLoc
    }

    instance PartialOrder[Kind] {
        pub def lessEqual(x: Kind, y: Kind): Bool =
            match y {
                case Kind.Any => true
                case _ => x == y
            }
    }

    instance Located[AstNode] {
        pub def getLoc(x: AstNode): LocRange =
            match x {
                case AstNode.Scope(scope) => scope |> Located.getLoc
                case AstNode.Use(usage) => usage |> Located.getLoc
                case AstNode.Module(module) => module |> Located.getLoc
                case AstNode.TypeAlias(typeAlias) => typeAlias |> Located.getLoc
                case AstNode.Enum(eenum) => eenum |> Located.getLoc
                case AstNode.EnumCase(enumCase) => enumCase |> Located.getLoc
                case AstNode.Def(deff) => deff |> Located.getLoc
                case AstNode.Exp(exp) => exp |> Located.getLoc
                case AstNode.Tpe(tpe) => tpe |> Located.getLoc
                case AstNode.Refr(refr) => refr |> Located.getLoc
            }
    }

}
