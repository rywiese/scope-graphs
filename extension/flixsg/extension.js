const vscode = require('vscode');
const cp = require('child_process');
const path = require('path');
const fs = require('fs');
const {
  LanguageClient
} = require('vscode-languageclient/node');

let client;

function activate(context) {
  const workspaceFolders = vscode.workspace.workspaceFolders;

  if (!workspaceFolders) {
    vscode.window.showErrorMessage('Flixsg: No workspace folder open.');
    return;
  }

  const workspacePath = workspaceFolders[0].uri.fsPath;

  // Construct full path to the JAR inside the user's project
  const jarPath = path.join(workspacePath, 'artifact', 'scope-graphs.jar');

  if (!fs.existsSync(jarPath)) {
    vscode.window.showErrorMessage(`Flixsg: Language server not found at ${jarPath}`);
    return;
  }

  const serverCommand = 'java';
  const serverArgs = ['-jar', jarPath, 'lsp'];

  const serverProcess = cp.spawn(serverCommand, serverArgs, {
    cwd: workspacePath
  });

  serverProcess.stderr.on('data', (data) => {
    console.error(`[LSP STDERR] ${data}`);
  });

  const streamInfo = () => {
    return Promise.resolve({
    writer: serverProcess.stdin,
    reader: serverProcess.stdout
    })
  };

  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'flixsg' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.flixsg')
    }
  };

  client = new LanguageClient(
    'flixsgLanguageServer',
    'Flixsg Language Server',
    streamInfo,
    clientOptions
  );

  client.start();
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = {
  activate,
  deactivate
};
