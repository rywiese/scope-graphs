mod SG.Lsp {
    import java.lang.System
    import org.eclipse.lsp4j.jsonrpc.Launcher
    import org.eclipse.lsp4j.launch.LSPLauncher
    import org.eclipse.lsp4j.services.LanguageClient

    pub def runServer(): Unit \ IO = region rc {
        System.err.println("Starting SG LSP server");
        let server = Server.mkServer(rc);
        let launcher = LSPLauncher.createServerLauncher(
            server#server,
            System.in,
            System.out
        );
        let client = unchecked_cast(launcher.getRemoteProxy() as LanguageClient);
        server#connect(client);
        launcher.startListening().get();
        System.err.println("Stopped SG LSP server")
    }

}
