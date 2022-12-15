{ inputs, cell }:

cell.library.make-plutus-apps-project {
  deferPluginErrors = false;
  enableHaskellProfiling = false;
}
