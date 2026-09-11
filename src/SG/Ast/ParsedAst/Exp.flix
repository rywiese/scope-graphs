pub mod SG.Ast.ParsedAst.Exp {
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
