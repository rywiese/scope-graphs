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
    use ScopeGraph.Ast.CheckResult
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Phase.File
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Phase.Sanitizer
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    pub def compile(input: String): CheckResult \ Crash = {
        let ast = input |>
            Lexer.lex |>
            Parser.parse !>
            Sanitizer.sanitize;
        CheckResult.mk(ast)
    }

    pub def compileRes(input: String): Result[Error, CheckResult] = {
        run input |> compile |> Result.Ok with Crash.runWith(Result.Err)
    }

    pub def compileUnsafe(input: String): CheckResult = {
        run compile(input) with Crash.runUnsafe
    }

    pub def compileFile(path: String): CheckResult \ IO + Crash = {
        path |> File.read |> compile
    }

    pub def compileFileRes(path: String): Result[Error, CheckResult] \ IO = {
        run path |> compileFile |> Result.Ok with Crash.runWith(Result.Err)
    }

    pub def compileFileUnsafe(path: String): CheckResult \ IO = {
        run compileFile(path) with Crash.runWith(Error.toStringWithFile(path) >> bug!)
    }

}
