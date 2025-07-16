const vscode = require('vscode');
const cp = require('child_process');
const path = require('path');
const fs = require('fs');
const {
  LanguageClient
} = require('vscode-languageclient/node');

let client;

let clientOptions;
let workspacePath;
let jarPath;

function activate(context) {
  const workspaceFolders = vscode.workspace.workspaceFolders;

  if (!workspaceFolders) {
    vscode.window.showErrorMessage('Flixsg: No workspace folder open.');
    return;
  }

  workspacePath = workspaceFolders[0].uri.fsPath;

  // Construct full path to the JAR inside the user's project
  jarPath = path.join(workspacePath, 'artifact', 'scope-graphs.jar');

  startServer();

  const stopCommand = vscode.commands.registerCommand('flixsg:stop', () => stopServer())
  context.subscriptions.push(stopCommand);

  const restartCommand = vscode.commands.registerCommand('flixsg:restart', () => restartServer())
  context.subscriptions.push(restartCommand);
}

function startServer() {
  if (!fs.existsSync(jarPath)) {
    vscode.window.showErrorMessage(`Flixsg: Language server not found at ${jarPath}`);
    return;
  }

  const serverProcess = cp.spawn('java', ['-jar', jarPath, 'lsp'], {
    cwd: workspacePath
  });

  serverProcess.stderr.on('data', (data) => {
    console.info(`[SG LSP] ${data}`);
  });

  const streamInfo = () => {
    return Promise.resolve({
      writer: serverProcess.stdin,
      reader: serverProcess.stdout
    })
  };

  clientOptions = {
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

  return client.start();
}

function stopServer() {
  client.stop()
}

async function restartServer() {
  stopServer()
  startServer()
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = {
  activate,
  deactivate
};
