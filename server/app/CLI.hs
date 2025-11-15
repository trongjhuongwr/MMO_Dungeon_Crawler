module CLI (runCLI) where

-- Lấy danh sách các tham số và xử lý chúng
runCLI :: [String] -> IO ()
runCLI args =
  case head args of
    "migrate" -> do
      putStrLn "Running database migrations..."
      putStrLn "Migrations complete."
    "reset-db" -> do
      putStrLn "Resetting database..."
      putStrLn "Database reset."
    _ ->
      putStrLn $ "Unknown command: " ++ head args