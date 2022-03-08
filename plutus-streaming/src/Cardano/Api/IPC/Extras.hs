module Cardano.Api.IPC.Extras where

-- import Cardano.Api
-- import Control.Tracer (nullTracer)
-- import Ouroboros.Network.NodeToClient qualified as N2C
-- import Ouroboros.Network.NodeToNode qualified as N2N
-- import Ouroboros.Network.Snocket (socketSnocket)
-- import Ouroboros.Network.Socket (nullNetworkConnectTracers)


-- connectToLocalNode' :: LocalNodeConnectInfo mode
--                    -> LocalNodeClientProtocolsInMode mode
--                    -> IO ()
-- connectToLocalNode' localNodeConnectInfo handlers
--   = connectToLocalNodeWithVersion' localNodeConnectInfo (const handlers)

-- -- | Establish a connection to a local node and execute the given set of
-- -- protocol handlers parameterized on the negotiated node-to-client protocol
-- -- version.
-- --
-- connectToLocalNodeWithVersion' :: LocalNodeConnectInfo mode
--                               -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode mode)
--                               -> IO ()
-- connectToLocalNodeWithVersion' LocalNodeConnectInfo {
--                      localNodeSocketPath,
--                      localNodeNetworkId,
--                      localConsensusModeParams
--                    } clients =
--     N2C.withIOManager $ \iomgr ->
--       N2C.connectTo
--         (N2C.localSnocket iomgr)
--         N2C.NetworkConnectTracers {
--           N2C.nctMuxTracer       = nullTracer,
--           N2C.nctHandshakeTracer = nullTracer
--         }
--         versionedProtocls
--         localNodeSocketPath
--   where
--     versionedProtocls =
--       -- First convert from the mode-parametrised view of things to the
--       -- block-parametrised view and then do the final setup for the versioned
--       -- bundles of mini-protocols.
--       undefined
--       -- case mkLocalNodeClientParams localConsensusModeParams clients of
--       --   LocalNodeClientParams ptcl clients' ->
--       --     mkVersionedProtocols localNodeNetworkId ptcl clients'


-- connect ioManager =
--   N2N.connectTo
--     (socketSnocket ioManager)
--     N2N.nullNetworkConnectTracers
--     _versions
--     _maybeSockAddr
--     _sockAddr
