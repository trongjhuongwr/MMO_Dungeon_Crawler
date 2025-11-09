module Settings
  ( -- Paths
    mapPVP
  , mapDungeon
    -- Helpers
  , texture
  , tank
  , projectile
  , ui
  , mapPath
  ) where

import System.FilePath ((</>))
import Network.Socket (HostName, PortNumber)

-- ===================================
-- Asset Paths
-- ===================================
assetsRoot :: FilePath
assetsRoot = "client" </> "assets"

mapsRoot :: FilePath
mapsRoot = assetsRoot </> "maps"

texturesRoot :: FilePath
texturesRoot = assetsRoot </> "textures"

-- Hàm trợ giúp chung
texture :: FilePath -> FilePath
texture p = texturesRoot </> p

mapPath :: FilePath -> FilePath
mapPath p = mapsRoot </> p

-- Hàm trợ giúp cụ thể
tank :: FilePath -> FilePath -> FilePath
tank tankName file = texture $ "tanks" </> tankName </> file

projectile :: FilePath -> FilePath
projectile file = texture $ "projectiles" </> file

ui :: FilePath -> FilePath
ui file = texture $ "ui" </> file

-- Đường dẫn map cụ thể
mapPVP :: FilePath
mapPVP = mapPath "pvp.json"

mapDungeon :: FilePath
mapDungeon = mapPath "pvp.json"