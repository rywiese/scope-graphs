use ScopeGraph.Manager

def main(): Unit \ IO + Sys = {
    let res = run mainAux() with Environment.runWithIO with FileRead.runWithIO;
    match res {
        case Result.Ok(()) => ()
        case Result.Err(err) => bug!(err |> ToString.toString)
    }
}

def mainAux(): Unit \ IO + FileRead + Environment = region rc {
    match Environment.getArgs() {
        case "lsp" :: Nil =>
            ScopeGraph.Lsp.runServer()
        case "run" :: path :: Nil =>
           let man = Manager.empty(rc);
            man |> Manager.addFileIfNotPresent(path);
            match man |> Manager.check {
                case Validation.Success(_) => println("success")
                case Validation.Failure(errs) =>
                    foreach (err <- errs) println(err)
            }
        case _ =>
            let options = List#{
                ("lsp", "starts LSP server"),
                ("example", "analyses ./examples/misc.txt"),
                ("run <path>", "compiles the file located at <path>")
            };
            let lines = options |> List.map(match (name, descr) -> "  [${name}] ${descr}") |> List.join("\n");
            println("usage:\n${lines}")
    }
}

mod ScopeGraph {
    use Functor.map
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Ast
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.CheckResult
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Phase.Sanitizer
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    pub def compile(inputs: Map[Source, String]): Validation[Error, CheckResult] = {
        let res = forM (
            lexed <- Lexer.lex(inputs) |> Validation.toResult;
            parsed <- Parser.parse(lexed) |> Validation.toResult;
            _ <- Sanitizer.sanitize(parsed) |> Validation.toResult
        ) yield CheckResult.mk(parsed);
        match res {
            case Result.Ok(ok) => Validation.Success(ok)
            case Result.Err(errs) => Validation.Failure(errs)
        }
    }

    // pub def compileUnsafe(inputs: Map[Source, String]): CheckResult \ IO = {
    //     match compile(inputs) {
    //         case Validation.Success(v) => v
    //         case Validation.Failure(errs) =>
    //             foreach (err <- errs) println(err);
    //             bug!("Compilation Failed")
    //     }
    // }

}
