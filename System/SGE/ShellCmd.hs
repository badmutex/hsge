
module System.SGE.ShellCmd where


newtype ShellCmd = Cmd { fromCmd :: String } deriving Show

class Shell a where
    shellCmd :: a -> ShellCmd


