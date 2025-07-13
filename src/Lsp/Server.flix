mod ScopeGraph.Lsp {
    import org.eclipse.lsp4j.services.LanguageClient
    import org.eclipse.lsp4j.services.LanguageServer

    pub struct Server[r] {
        server: LanguageServer,
        connect: LanguageClient -> Unit \ r
    }
}

mod ScopeGraph.Lsp.Server {
    import org.eclipse.lsp4j.DefinitionParams
    import java.lang.Boolean
    import org.eclipse.lsp4j.DocumentLinkParams
    import java.util.ArrayList
    import org.eclipse.lsp4j.DocumentHighlightParams
    import java.lang.{Object => JObject}
    import java.util.concurrent.CompletableFuture
    import java.util.function.Supplier
    import java.lang.System
    import org.eclipse.lsp4j.DidChangeConfigurationParams
    import org.eclipse.lsp4j.DidChangeTextDocumentParams
    import org.eclipse.lsp4j.DidChangeWatchedFilesParams
    import org.eclipse.lsp4j.DidCloseTextDocumentParams
    import org.eclipse.lsp4j.DidOpenTextDocumentParams
    import org.eclipse.lsp4j.DidSaveTextDocumentParams
    import org.eclipse.lsp4j.InitializeParams
    import org.eclipse.lsp4j.InitializeResult
    import org.eclipse.lsp4j.MessageParams
    import org.eclipse.lsp4j.MessageType
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
                        sc.setDefinitionProvider(Boolean.valueOf(true));
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

    def mkTextDocumentService(_client0: Ref[Option[LanguageClient], r]): TextDocumentService \ IO = {
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
            def documentHighlight(_this: TextDocumentService, _params: DocumentHighlightParams): CompletableFuture \ IO = {
                System.err.println("documentHighlight");
                CompletableFuture.completedFuture(new ArrayList())
            }
            def definition(_this: TextDocumentService, _params: DefinitionParams): CompletableFuture \ IO = {
                System.err.println("definition");
                CompletableFuture.completedFuture(new ArrayList())
            }

        }
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
