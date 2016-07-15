import Cagnotte.Parser
import Cagnotte.Debts
import Cagnotte.Types
import Text.Megaparsec
import System.Environment
import System.Exit

processFile :: String -> IO ()
processFile filename = do
  transactions <- runParser parseInput filename <$> readFile filename
  case transactions of
    Left err -> print err >> exitFailure
    Right tr -> do
--      print tr
      let debts = computeDebts tr
      printDebts debts

main :: IO ()
main = do
  args <- getArgs

  if length args /= 1
    then do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " <input filename>"
      exitFailure
    else processFile (args !! 0)
