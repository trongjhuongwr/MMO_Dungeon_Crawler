import Codec.Picture
import Codec.Picture.Types (Image, PixelRGBA8, imageWidth, imageHeight)
import Control.Monad (forM_)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

-- | Kích thước của mỗi tile
tileSize :: Int
tileSize = 16

-- | Hàm chính để cắt ảnh
dumpTiles :: FilePath -> IO ()
dumpTiles imagePath = dumpTilesWithOut imagePath "tile_dump"

-- | Hàm main để chạy test
main :: IO ()
main = do
    -- Ensure we can print UTF-8 characters to stdout (avoids Windows encoding errors)
    hSetEncoding stdout utf8
    args <- getArgs
    case args of
      (imgPath:_) -> do
          -- Optional: allow user to pass output directory as second arg
          let outDir = case args of
                         (_:dir:_) -> dir
                         _         -> "tile_dump"
          dumpTilesWithOut imgPath outDir
      _ -> putStrLn "Usage: TileDumper <image-path> [output-dir]"

-- | Variant that allows specifying output directory
dumpTilesWithOut :: FilePath -> FilePath -> IO ()
dumpTilesWithOut imagePath outputDir = do
    putStrLn $ "Đang đọc ảnh từ: " ++ imagePath
    eImg <- readImage imagePath

    case eImg of
        Left err -> putStrLn $ "Lỗi đọc ảnh: " ++ err
        Right dynamicImg -> do
            let img = convertRGBA8 dynamicImg
            let (w, h) = (imageWidth img, imageHeight img)

            putStrLn $ "Kích thước ảnh: " ++ show w ++ "x" ++ show h

            let numTilesX = w `div` tileSize
            let numTilesY = h `div` tileSize

            if numTilesX <= 0 || numTilesY <= 0
              then putStrLn "Ảnh quá nhỏ để chia thành các tile 16x16."
              else do
                putStrLn $ "Phát hiện " ++ show (numTilesX * numTilesY) ++ 
                           " tiles (" ++ show numTilesX ++ " hàng x " ++ show numTilesY ++ " cột)"

                createDirectoryIfMissing True outputDir
                putStrLn $ "Đang lưu các tiles vào thư mục: " ++ outputDir

                forM_ [0 .. numTilesY - 1] $ \ty -> do
                    forM_ [0 .. numTilesX - 1] $ \tx -> do
                        let px = tx * tileSize
                            py = ty * tileSize
                            tileImg = cropImage px py tileSize tileSize img
                            filename = outputDir </> ("tile_" ++ show ty ++ "_" ++ show tx ++ ".png")
                        -- Lưu tile đã cắt (ImageRGBA8 -> DynamicImage)
                        savePngImage filename (ImageRGBA8 tileImg)
                        putStrLn $ "Saved: " ++ filename

                putStrLn $ "Hoàn tất! Đã lưu " ++ show (numTilesX * numTilesY) ++ 
                           " files vào thư mục '" ++ outputDir ++ "'."


-- | Cắt một vùng của ảnh (x, y là góc trên trái), trả về ảnh mới kích thước w x h
cropImage :: Int -> Int -> Int -> Int -> Image PixelRGBA8 -> Image PixelRGBA8
cropImage x y w h img = generateImage gen w h
    where
        gen i j = pixelAt img (x + i) (y + j)