mod SG.Ast {
    use SG.Ast.Shared.Location.Source
    use SG.Ast.Lexed.Token
    use SG.Ast.ParsedAst.Scope

    pub type alias Lexed = {
        m = Map[Source, Vector[Token]]
    }

    pub type alias ParsedAst = {
        m = Map[Source, Scope]
    }
}
