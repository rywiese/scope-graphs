mod SG.Ast.Shared {
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString

    pub enum Exp with Hash {
        case Let(LocString, Exp, Exp, LocRange)
        case Stmt(Exp, Exp, LocRange)
        case App(Exp, List[Exp], LocRange)
        case Name(Name, LocRange)
        case Int(Int32, LocRange)
        case Bool(Bool, LocRange)
    }

    instance Located[Exp] {
        pub def getLoc(x: Exp): LocRange =
            Exp.getLoc(x)
    }

    mod Exp {
        use SG.Ast.Shared.Location.LocRange

        pub def getLoc(exp: Exp): LocRange = match exp {
            case Let(_, _, _, loc) => loc
            case Stmt(_, _, loc) => loc
            case App(_, _, loc) => loc
            case Name(_, loc) => loc
            case Int(_, loc) => loc
            case Bool(_, loc) => loc
        }
    }

}
