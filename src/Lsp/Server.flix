mod ScopeGraph.Lsp {
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer

    pub struct Server[r] {
        server: LanguageServer,
        connect: LanguageClient -> Unit \ r
    }
}

mod ScopeGraph.Lsp.Server {
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Location.Loc
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Ast.Shared.Location.LocRange
    use ScopeGraph.Ast.Util.Finder

    use Functor.map

    import java.lang.{Object => JObject}
    import java.lang.System
    import java.lang.Boolean
    import java.lang.Character
    import java.lang.Byte
    import java.lang.Short
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Float
    import java.lang.Double
    import java.net.URI
    import java.nio.file.Paths
    import java.util.ArrayList
    import java.util.concurrent.CompletableFuture
    import java.util.function.Supplier
    import org.eclipse.lsp4j.{Location => JLocation}
    import org.eclipse.lsp4j.{Position => JPosition}
    import org.eclipse.lsp4j.{Range => JRange}
    import org.eclipse.lsp4j.DefinitionParams
    import org.eclipse.lsp4j.DidChangeConfigurationParams
    import org.eclipse.lsp4j.DidChangeTextDocumentParams
    import org.eclipse.lsp4j.DidChangeWatchedFilesParams
    import org.eclipse.lsp4j.DidCloseTextDocumentParams
    import org.eclipse.lsp4j.DidOpenTextDocumentParams
    import org.eclipse.lsp4j.DidSaveTextDocumentParams
    import org.eclipse.lsp4j.DocumentHighlightParams
    import org.eclipse.lsp4j.DocumentLinkParams
    import org.eclipse.lsp4j.InitializeParams
    import org.eclipse.lsp4j.InitializeResult
    import org.eclipse.lsp4j.MessageParams
    import org.eclipse.lsp4j.MessageType
    import org.eclipse.lsp4j.Position
    import org.eclipse.lsp4j.DocumentHighlight
    import org.eclipse.lsp4j.DocumentHighlightKind
    import org.eclipse.lsp4j.ServerCapabilities
    import org.eclipse.lsp4j.ServerInfo
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer
    import org.eclipse.lsp4j.services.TextDocumentService
    import org.eclipse.lsp4j.services.WorkspaceService

    pub def getJServer(s: Server[r]): LanguageServer =
        s->server

    pub def connect(c: LanguageClient, s: Server[r]): Unit \ r =
        s->connect(c)

    pub def mkServer(rc: Region[r]): Server[r] \ r + IO = {
        let client = Ref.fresh(rc, None);
        let textDocumentService = mkTextDocumentService(client);
        let workspaceService = mkWorkspaceService();
        let server = new LanguageServer {
            def initialize(_this: LanguageServer, _params: InitializeParams): CompletableFuture \ IO = {
                CompletableFuture.supplyAsync(new Supplier {
                    def get(_this: Supplier): JObject \ IO =
                        let sc = new ServerCapabilities();
                        let bTrue = Boolean.valueOf(true);
                        sc.setDefinitionProvider(bTrue);
                        sc.setDocumentHighlightProvider(bTrue);
                        checked_cast(new InitializeResult(
                            sc,
                            new ServerInfo("flixsg", "0.0.1")
                        ))
                })
            }
            def exit(_this: LanguageServer): Unit = {
                ()
            }
            def getTextDocumentService(_this: LanguageServer): TextDocumentService = {
                textDocumentService
            }
            def getWorkspaceService(_this: LanguageServer): WorkspaceService = {
                workspaceService
            }
            def shutdown(_this: LanguageServer): CompletableFuture \ IO = {
                CompletableFuture.completedFuture(null)
            }
        };
        new Server @ rc {
            server = server,
            connect = c -> Ref.put(Some(c), client)
        }
    }

    def mkTextDocumentService(client0: Ref[Option[LanguageClient], r]): TextDocumentService \ IO = {
        new TextDocumentService {
            def didOpen(_this: TextDocumentService, _params: DidOpenTextDocumentParams): Unit \ IO = {
                System.err.println("didOpen")
            }
            def didClose(_this: TextDocumentService, _params: DidCloseTextDocumentParams): Unit \ IO = {
                System.err.println("didClose")
            }
            def didChange(_this: TextDocumentService, _params: DidChangeTextDocumentParams): Unit \ IO = {
                System.err.println("didChange")
            }
            def didSave(_this: TextDocumentService, _params: DidSaveTextDocumentParams): Unit \ IO = {
                System.err.println("didSave")
            }
            def documentHighlight(_this: TextDocumentService, params: DocumentHighlightParams): CompletableFuture \ IO + r = {
                System.err.println("documentHighlight");
                let uri = params.getTextDocument().getUri();
                let path = Paths.get(new URI(uri)).toString();
                let locOpt = match ScopeGraph.compileFileRes(path) {
                    case Result.Err(err) =>
                        Result.Err("${err}")
                    case Result.Ok(ast) =>
                        let cursor = params.getPosition() |> posToLoc;
                        match Finder.find(cursor, ast) |> Option.map(Located.getLoc) {
                            case Some(loc) => Result.Ok(loc)
                            case None => Result.Err("failed to find")
                        }
                };
                match locOpt {
                    case Result.Ok(_) => ()
                    case Result.Err(err) => client0 |> showError(err)
                };

                let list = locOpt |>
                    Result.map(loc -> new DocumentHighlight(loc |> locToJLoc, DocumentHighlightKind.Text)) |>
                    Result.toList |>
                    toArrayList;
                CompletableFuture.completedFuture(list)
            }
            def definition(_this: TextDocumentService, params: DefinitionParams): CompletableFuture \ IO + r = {
                System.err.println("definition");
                let uri = params.getTextDocument().getUri();
                let path = Paths.get(new URI(uri)).toString();
                let locOpt = match ScopeGraph.compileFileRes(path) {
                    case Result.Err(err) =>
                        Result.Err("${err}")
                    case Result.Ok(ast) =>
                        let cursor = params.getPosition() |> posToLoc;
                        match Finder.find(cursor, ast) |> Option.map(Located.getLoc) {
                            case Some(loc) => Result.Ok(loc)
                            case None => Result.Err("failed to find")
                        }
                };
                match locOpt {
                    case Result.Ok(_) => ()
                    case Result.Err(err) => client0 |> showError(err)
                };

                let list = locOpt |>
                    Result.map(loc -> new JLocation(uri, loc |> locToJLoc)) |>
                    Result.toList |>
                    toArrayList;
                CompletableFuture.completedFuture(list)
            }

        }
    }

    def showError(msg: String, client0: Ref[Option[LanguageClient], r]): Unit \ IO + r = {
        client0 |> Ref.get |> Option.forEach(c -> c.showMessage(new MessageParams(MessageType.Error, msg)))
    }

    def _logDev(msg: String): Unit \ IO = {
        System.err.println(msg)
    }

    def posToLoc(p: Position): Loc \ IO = {
        // TODO need a (line,col) -> offset map to find the index here.
        Location.mkLoc(line = p.getLine() + 1, column = p.getCharacter() + 1, -1)
    }

    def toObject(x: t): JObject = {
        typematch x {
            case y: Bool => checked_cast(Boolean.valueOf(y))
            case y: Char => checked_cast(Character.valueOf(y))
            case y: Int8 => checked_cast(Byte.valueOf(y))
            case y: Int16 => checked_cast(Short.valueOf(y))
            case y: Int32 => checked_cast(Integer.valueOf(y))
            case y: Int64 => checked_cast(Long.valueOf(y))
            case y: Float32 => checked_cast(Float.valueOf(y))
            case y: Float64 => checked_cast(Double.valueOf(y))
            case _: _ => unchecked_cast(x as JObject)
        }
    }

    /// Boxes primitives
    def toArrayList(l: List[t]): ArrayList \ IO = {
        let list = new ArrayList();
        foreach(x <- l) discard list.add(toObject(x));
        list
    }

    def locToJLoc(loc: LocRange): JRange \ IO = {
        let {start, end} = coerce(loc);
        // zero-based
        let jStart = new JPosition(coerce(start)#line - 1, coerce(start)#column - 1);
        let jEnd = new JPosition(coerce(end)#line - 1, coerce(end)#column - 1);
        // exclusive?
        new JRange(jStart, jEnd)
    }

    def mkWorkspaceService(): WorkspaceService \ IO = {
        new WorkspaceService {
            def didChangeConfiguration(_this: WorkspaceService, _params: DidChangeConfigurationParams): Unit \ IO = {
                System.err.println("didChangeConfiguration")
            }
            def didChangeWatchedFiles(_this: WorkspaceService, _params: DidChangeWatchedFilesParams): Unit \ IO = {
                System.err.println("didChangeWatchedFiles")
            }
        }
    }

}
