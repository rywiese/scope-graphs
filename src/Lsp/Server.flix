mod ScopeGraph.Lsp {
    use ScopeGraph.Manager
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer

    struct Server[r] {
        mut client: Option[LanguageClient],
        manager: Manager[r]
    }
}

mod ScopeGraph.Lsp.Server {
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Ast.CheckResult
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Location.Loc
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Ast.Shared.Location.LocRange
    use ScopeGraph.Manager

    use Functor.map

    import java.lang.{Object => JObject}
    import java.lang.Boolean
    import java.lang.Byte
    import java.lang.Character
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Short
    import java.lang.System
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
    import org.eclipse.lsp4j.DocumentHighlight
    import org.eclipse.lsp4j.DocumentHighlightKind
    import org.eclipse.lsp4j.DocumentHighlightParams
    import org.eclipse.lsp4j.DocumentLinkParams
    import org.eclipse.lsp4j.InitializeParams
    import org.eclipse.lsp4j.InitializeResult
    import org.eclipse.lsp4j.MessageParams
    import org.eclipse.lsp4j.MessageType
    import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
    import org.eclipse.lsp4j.Position
    import org.eclipse.lsp4j.ServerCapabilities
    import org.eclipse.lsp4j.ServerInfo
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer
    import org.eclipse.lsp4j.services.TextDocumentService
    import org.eclipse.lsp4j.services.WorkspaceService

    pub def mkServer(rc: Region[r]): {server = LanguageServer, connect = LanguageClient -> Unit \ r} \ r + IO = {
        let server = new Server @ rc {
            manager = Manager.empty(rc),
            client = None
        };
        let textDocumentService = mkTextDocumentService(server);
        let workspaceService = mkWorkspaceService();
        let jServer = new LanguageServer {
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
        {
            server = jServer,
            connect = c -> server->client = Some(c)
        }
    }

    def mkTextDocumentService(s: Server[r]): TextDocumentService \ IO = {
        new TextDocumentService {
            def didOpen(_this: TextDocumentService, params: DidOpenTextDocumentParams): Unit \ IO + r = {
                System.err.println("didOpen");
                s |> addUriIfNotPresent(params.getTextDocument().getUri())
            }
            def didClose(_this: TextDocumentService, _params: DidCloseTextDocumentParams): Unit \ IO = {
                System.err.println("didClose")
            }
            def didChange(_this: TextDocumentService, params: DidChangeTextDocumentParams): Unit \ IO + r = {
                System.err.println("didChange");
                s |> updateUri(params.getTextDocument().getUri())
            }
            def didSave(_this: TextDocumentService, params: DidSaveTextDocumentParams): Unit \ IO + r = {
                System.err.println("didSave");
                s |> updateUri(params.getTextDocument().getUri())
            }
            def documentHighlight(_this: TextDocumentService, params: DocumentHighlightParams): CompletableFuture \ IO + r = {
                System.err.println("documentHighlight");
                s |> addUriIfNotPresent(params.getTextDocument().getUri());
                let highlights = match s->manager |> Manager.check {
                    case Validation.Failure(_) => Nil
                    case Validation.Success(cr) =>
                        let uri = params.getTextDocument().getUri();
                        let path = pathFromUri(uri);
                        let src = Source.File(path);
                        let cursor = params.getPosition() |> posToLoc(src);
                        match CheckResult.enclosingRange(cursor, cr) {
                            case None =>
                                logDev("finder", "miss");
                                Nil
                            case Some(range) =>
                                List#{range}
                        }
                };
                let list = highlights |>
                    map(loc -> new DocumentHighlight(loc |> locToJLoc, DocumentHighlightKind.Text)) |>
                    toArrayList;
                CompletableFuture.completedFuture(list)
            }
            def definition(_this: TextDocumentService, params: DefinitionParams): CompletableFuture \ IO + r = {
                System.err.println("definition");
                let uri = params.getTextDocument().getUri();
                s |> addUriIfNotPresent(params.getTextDocument().getUri());
                let highlights = match s->manager |> Manager.check {
                    case Validation.Failure(_) => Nil
                    case Validation.Success(cr) =>
                        let path = pathFromUri(uri);
                        let src = Source.File(path);
                        let cursor = params.getPosition() |> posToLoc(src);
                        match CheckResult.definition(cursor, cr) {
                            case None =>
                                logDev("finder", "miss");
                                Nil
                            case Some(range) =>
                                List#{range}
                        }
                };
                let list = highlights |>
                    map(loc -> {
                        JEither.forLeft(new JLocation(uri, loc |> locToJLoc))
                    }) |>
                    toArrayList;
                CompletableFuture.completedFuture(list)
            }
        }
    }

    def addUriIfNotPresent(uri: String, s: Server[r]): Unit \ r + IO = {
        addFileIfNotPresent(pathFromUri(uri), s)
    }

    def updateUri(uri: String, s: Server[r]): Unit \ r + IO = {
        updateFile(pathFromUri(uri), s)
    }

    def addFileIfNotPresent(path: String, s: Server[r]): Unit \ r + IO = {
        let res = run s->manager |> Manager.addFileIfNotPresent(path) with FileRead.runWithIO;
        match res {
            case Result.Ok(()) => ()
            case Result.Err(err) => s |> showError(ToString.toString(err))
        }
    }

    def updateFile(path: String, s: Server[r]): Unit \ r + IO = {
        let res = run s->manager |> Manager.updateFile(path) with FileRead.runWithIO;
        match res {
            case Result.Ok(()) => ()
            case Result.Err(err) => s |> showError(ToString.toString(err))
        }
    }

    def pathFromUri(uri: String): String \ IO = {
        Paths.get(new URI(uri)).toString()
    }

    def showError(msg: String, s: Server[r]): Unit \ IO + r = {
        s->client |> Option.forEach(c -> c.showMessage(new MessageParams(MessageType.Error, msg)))
    }

    def logDev(prefix: String, msg: String): Unit \ IO = {
        System.err.println(prefix + ": " + msg)
    }

    def posToLoc(src: Source, p: Position): Loc \ IO = {
        // TODO need a (line,col) -> offset map to find the index here.
        Location.mkLoc(line = p.getLine() + 1, column = p.getCharacter() + 1, -1, src)
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
        let {start, end, src = _} = coerce(loc);
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
