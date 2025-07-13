def main(): Unit \ IO + Sys = run {
    match Environment.getArgs() {
        case "lsp" :: Nil => ScopeGraph.Lsp.runServer()
        case "example" :: Nil => ScopeGraph.compileUnsafe("./examples/misc.txt")
        case "run" :: path :: Nil => ScopeGraph.compileUnsafe(path)
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

mod ScopeGraph {
    use ScopeGraph.Ast
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Phase.Sanitizer
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Phase.File
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    pub def compile(input: String): Unit \ IO + Crash = {
        input |>
            Lexer.lex |>
            Parser.parse |>
            Sanitizer.sanitize;
        println("Compilation successful!")
    }

    pub def compileUnsafe(input: String): Unit \ IO = {
        run compile(input) with Crash.runUnsafe
    }

    pub def compileFile(path: String): Unit \ IO + Crash = {
        path |> File.read |> compile
    }

    pub def compileFileUnsafe(path: String): Unit \ IO = {
        run compileFile(path) with Crash.runWith(Error.toStringWithFile(path) >> bug!)
    }

}
