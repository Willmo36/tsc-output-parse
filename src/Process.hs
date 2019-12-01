module Process where

import System.Process

rrp = "c:/skip/code/react_restaurant_portal"

runTscCommand cwd = do
  let cmd = (shell "tsc") {cwd = Just cwd}
  (_, errors, _) <- readCreateProcessWithExitCode cmd ""
  
  return $ lines errors
