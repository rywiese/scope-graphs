mod ScopeGraph.Lsp {
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer

    pub struct Server[r] {
        server: LanguageServer,
        connect: LanguageClient -> Unit \ r
    }
}

mod ScopeGraph.Lsp.Server {
    import org.eclipse.lsp4j.MessageType
    import org.eclipse.lsp4j.MessageParams
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.DidChangeWatchedFilesParams
    import org.eclipse.lsp4j.DidChangeConfigurationParams
    import org.eclipse.lsp4j.DidSaveTextDocumentParams
    import org.eclipse.lsp4j.DidChangeTextDocumentParams
    import org.eclipse.lsp4j.DidCloseTextDocumentParams
    import org.eclipse.lsp4j.DidOpenTextDocumentParams
    import java.util.function.Supplier
    import java.lang.{Object => JObject}
    import java.util.concurrent.CompletableFuture
    import org.eclipse.lsp4j.InitializeParams
    import org.eclipse.lsp4j.InitializeResult
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
                        checked_cast(new InitializeResult())
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
            def didOpen(_this: TextDocumentService, _params: DidOpenTextDocumentParams): Unit \ r + IO = {
                client0 |> Ref.get |> Option.forEach(client -> {
                    client.showMessage(new MessageParams(MessageType.Info, "Hello"))
                })
            }
            def didClose(_this: TextDocumentService, _params: DidCloseTextDocumentParams): Unit = ()
            def didChange(_this: TextDocumentService, _params: DidChangeTextDocumentParams): Unit = ()
            def didSave(_this: TextDocumentService, _params: DidSaveTextDocumentParams): Unit = ()
        }
    }

    def mkWorkspaceService(): WorkspaceService \ IO = {
        new WorkspaceService {
            def didChangeConfiguration(_this: WorkspaceService, _params: DidChangeConfigurationParams): Unit = ()
            def didChangeWatchedFiles(_this: WorkspaceService, _params: DidChangeWatchedFilesParams): Unit = ()
        }
    }

}
