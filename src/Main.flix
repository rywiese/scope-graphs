use SG.Manager

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
            SG.Lsp.runServer()
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

mod SG {
    use Functor.map
    use SG.Ast.Shared.Location.Source
    use SG.Ast
    use SG.Ast.ParsedAst
    use SG.Ast.CheckResult
    use SG.Ast.Shared.Crash
    use SG.Ast.Shared.Error
    use SG.Phase.Lexer
    use SG.Phase.Parser
    use SG.Phase.Scoper
    use SG.Phase.Sanitizer
    use SG.Util.StringCursor
    use SG.Util.VectorCursor

}
