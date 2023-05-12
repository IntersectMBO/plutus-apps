{ inputs, cell }:

cell.library.make-plutus-apps-project {
  deferPluginErrors = true;
  enableHaskellProfiling = false;
}
