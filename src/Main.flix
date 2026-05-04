use SG.Manager
use Sys.Env

def main(): Unit \ IO = {
    run mainAux() with Env.runWithIO with Fs.FileRead.runWithIO
}

def mainAux(): Unit \ IO + Fs.FileRead + Env = region rc {
    match Env.getArgs() {
        case "lsp" :: Nil =>
            SG.Lsp.runServer()
        case "run" :: path :: Nil =>
            let man = Manager.empty(rc);
            match man |> Manager.addFileIfNotPresent(path) {
                case Result.Err(err) => println(err)
                case Result.Ok(()) =>
                    match man |> Manager.check {
                        case Validation.Success(_) => println("success")
                        case Validation.Failure(errs) =>
                            foreach (err <- errs) println(err)
                    }
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

