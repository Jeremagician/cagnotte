module Cagnotte.Debts
  ( printDebts
  , computeDebts
  )
where

import Cagnotte.Types
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord
import Data.Function (on)

printDebts :: Debts -> IO ()
printDebts d = do
  let xs = M.toList d
  print $ groupBy ((==) `on` (fst . fst)) xs

  -- let xs = M.toList d in
  --   groupBy (fst . fst) xs
  --   mapM_ personDebts xs



  -- where
  --   personDebts (n,l) = do
  --     putStrLn $ hdr n
  --     let filtered = filter ((/= 0) . snd) l in
  --       if length filtered == 0
  --       then putStrLn "No debt"
  --       else mapM_ debt filtered
  --     putStrLn $ replicate (length $ hdr n) '#'
  --     putStrLn ""

  --   hdr n = "#### " ++ n ++ " ####"

  --   debt (n, a) = putStrLn $ n ++ " : " ++ (show a)

addDebt :: Debts -> String -> String -> Amount -> Debts

-- A transaction from and to the same person is effectless
addDebt debts from to amt | from == to = debts

addDebt debts from to amt =
  -- We start by checking if `to` owe money to `from`
  -- if so, we substract the debt and get the new debt "if exists"
  let owed = M.lookup (to, from) debts in
  case owed of
    -- On this case, `to` owe nothing
    Nothing -> M.adjust (+ amt) (from, to) debts
    Just money ->
      if money >= amt
      then M.adjust (subtract amt) (to, from) debts
      else
        let new = M.adjust (subtract money) (to, from) debts
        in M.adjust (+ (amt - money)) (from, to) new

handleTransaction :: [String] -> Debts -> Transaction -> Debts
handleTransaction people debts tr =
  case (from tr, to tr) of
    (Owner x, Everybody)   -> foldl (\a b -> addDebt a b x divd) debts people
    (Owner x, Owner y)     -> addDebt debts y x amnt
    (Everybody, Owner x)   -> foldl (\a b -> addDebt a x b divd) debts people
    (Everybody, Everybody) -> debts

  where amnt = amount tr
        divd = amnt / (fromIntegral $ length people)

people :: [Transaction] -> [String]
people xs = nub $ go xs
  where go [] = []
        go (x:xs) = (wallet $ from x) ++ (wallet $ to x) ++ (go xs)

        wallet Everybody = []
        wallet (Owner x) = [x]

computeDebts :: [Transaction] -> Debts
computeDebts xs = foldl (handleTransaction ppl) init xs
  where init = M.fromList [ ((x, y), 0) | x <- ppl, y <- ppl]
        ppl = people xs
