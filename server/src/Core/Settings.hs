module Core.Settings
  ( getMapPath
  , getServerUdpPort
  , getServerTcpPort
  ) where

import System.FilePath ((</>))
import Network.Socket (PortNumber)
-- TODO: Đọc các giá trị này từ server.yaml bằng cách dùng Core.Config.loadConfig
-- Hiện tại, chúng ta hardcode để dễ phát triển.

-- | Lấy đường dẫn đến map PvP
getMapPath :: IO FilePath
getMapPath = pure $ "server" </> "assets" </> "maps" </> "pvp.json"

-- | Lấy cổng UDP của server
getServerUdpPort :: PortNumber
getServerUdpPort = 8888

-- | Lấy cổng TCP của server
getServerTcpPort :: PortNumber
getServerTcpPort = 4000