
module System.SGE.ShellCmd where

import HSH
import System.Exit

newtype ShellCmd = Cmd { fromCmd :: String } deriving Show

class Shell a where
    shellCmd :: a -> ShellCmd


runShell :: Shell a => a -> IO ExitCode
runShell = run . fromCmd . shellCmd
