def main(): Unit \ IO = {
    ScopeGraph.compileUnsafe("./examples/misc.txt")
}

mod ScopeGraph {
    use ScopeGraph.Ast
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Phase.Sanitizer
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Phase.File
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    pub def compile(input: String): Unit \ IO + Lexer.Error + Parser.Error + Sanitizer.Error = {
        input |>
            Lexer.lex |>
            Parser.parse |>
            Sanitizer.sanitize;
        println("Compilation successful!")
    }

    pub def compileUnsafe(input: String): Unit \ IO = {
        run compile(input) with runAllUnsafe
    }

    pub def compileFile(path: String): Unit \ IO + File.Error + Lexer.Error + Parser.Error + Sanitizer.Error = {
        path |> File.read |> compile
    }

    pub def compileFileUnsafe(path: String): Unit \ IO = {
        run compileFile(path) with runAllUnsafe
    }

    pub def runAllUnsafe(f: Unit -> t \ ef): t \ ef - File.Error - Lexer.Error - Parser.Error - Sanitizer.Error = {
        run f()
        with File.Error.runUnsafe("File error")
        with Lexer.Error.runUnsafe("Lex error")
        with Parser.Error.runUnsafe("Parse error")
        with Sanitizer.Error.runUnsafe("Sanitize error")
    }

}
