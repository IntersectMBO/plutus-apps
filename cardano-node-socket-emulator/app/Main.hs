module Main (main) where

import Cardano.BM.Trace (Trace, stdoutTrace)
import Cardano.Node.Socket.Emulator qualified as NodeServer
import Cardano.Node.Socket.Emulator.Types (CNSEServerLogMsg)
import Data.Default (def)
import Plutus.Monitoring.Util qualified as LM
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

prettyTrace :: Trace IO CNSEServerLogMsg
prettyTrace = LM.convertLog (renderStrict . layoutPretty defaultLayoutOptions . pretty) stdoutTrace

main :: IO ()
main = NodeServer.main prettyTrace def (pure ())
