mod SG.Ast.Shared {
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange

    pub enum Tpe with Hash {
        case Base(Name, LocRange)
        case App(Tpe, List[Tpe], LocRange)
    }

    instance Located[Tpe] {
        pub def getLoc(x: Tpe): LocRange =
            Tpe.getLoc(x)
    }

    mod Tpe {
        use SG.Ast.Shared.Location.LocRange

        pub def getLoc(t: Tpe): LocRange = match t {
            case Base(_, loc) => loc
            case App(_, _, loc) => loc
        }
    }
}
