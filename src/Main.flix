def main(): Unit \ IO + Sys = run {
    match Environment.getArgs() {
        case "lsp" :: Nil =>
            ScopeGraph.Lsp.runServer()
        case "example" :: Nil =>
            discard ScopeGraph.compile("./examples/misc.txt")
        case "run" :: path :: Nil =>
           discard ScopeGraph.compile(path)
        case _ =>
            let options = List#{
                ("lsp", "starts LSP server"),
                ("example", "analyses ./examples/misc.txt"),
                ("run <path>", "compiles the file located at <path>")
            };
            let lines = options |> List.map(match (name, descr) -> "  [${name}] ${descr}") |> List.join("\n");
            println("usage:\n${lines}")
    }
} with Environment.runWithIO
  with ScopeGraph.Ast.Shared.Crash.runWith(println)

mod ScopeGraph {
    use ScopeGraph.Ast
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Phase.File
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Phase.Sanitizer
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    pub def compile(input: String): ParsedAst.Scope \ Crash = {
        input |>
            Lexer.lex |>
            Parser.parse !>
            Sanitizer.sanitize
    }

    pub def compileUnsafe(input: String): ParsedAst.Scope = {
        run compile(input) with Crash.runUnsafe
    }

    pub def compileFile(path: String): ParsedAst.Scope \ IO + Crash = {
        path |> File.read |> compile
    }

    pub def compileFileUnsafe(path: String): ParsedAst.Scope \ IO = {
        run compileFile(path) with Crash.runWith(Error.toStringWithFile(path) >> bug!)
    }

}
