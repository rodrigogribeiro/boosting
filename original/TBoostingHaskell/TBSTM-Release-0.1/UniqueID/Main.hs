module Main where


import qualified UniqueIDCAS as CAS
import qualified UniqueIDSTM as STM
import qualified UniqueIDTB as TB

main :: IO ()
main = 
 do args <- getArgs
    case args of
     [mode, t, l] -> 
       do let len = read l :: Int
          let threads = read t :: Int
          let run nl = mainPar nl threads len
          case mode of
           "CAS"   -> do idgen <- ListHandle Int <- createList [0..len]
                         run nl
           "CASusingSTM"   -> do nl :: CASusingSTM.ListHandle Int <- createList [0..len]
                                 run nl
