pub mod SG.Ast.ParsedAst.Tpe {
    use SG.Ast.Shared.Location.LocRange

    pub def getLoc(t: Tpe): LocRange = match t {
        case Base(_, loc) => loc
        case App(_, _, loc) => loc
    }
}
