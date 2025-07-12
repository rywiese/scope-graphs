def main(): Unit \ IO = {
    ScopeGraph.compileUnsafe("./examples/misc.txt")
}

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
