{-# LANGUAGE
  Rank2Types
  , FlexibleInstances
  , TypeSynonymInstances
  #-}

module System.SGE.QSub ( CmdString (..)
                       , EmailTrigger (..)
                       , QSubFlag (..)
                       , QSub (..)
                       ) where

import System.SGE.ShellCmd
import System.SGE.Classes

import Text.Printf
import Data.List (intercalate)
import Data.Monoid



instance CmdString String where
    cmdString = id

data EmailTrigger = JobStarts
                  | JobEnds
                  | JobAborted
                  | JobRescheduled
                  | JobSuspended
                    deriving Eq

instance CmdString EmailTrigger where
    cmdString JobStarts      = "b"
    cmdString JobEnds        = "e"
    cmdString JobAborted     = "a"
    cmdString JobRescheduled = "a"
    cmdString JobSuspended   = "s"

instance CmdString [EmailTrigger] where
    cmdString = concatMap cmdString


data QSubFlag = JobName
              | EmailAddress
              | EmailTrigger
              | WorkDir
                deriving Eq

instance CmdString QSubFlag where
    cmdString JobName      = "N"
    cmdString EmailAddress = "M"
    cmdString EmailTrigger = "m"
    cmdString WorkDir      = "wd"


data QSub = QSub {
      job          :: ShellCmd
    , jobName      :: String
    , emailAddress :: String
    , emailOn      :: [EmailTrigger]
    , workingDir   :: FilePath
    }


fromQsub :: (Eq m, CmdString m, Monoid m, CmdString flag) => flag -> m -> String
fromQsub flag option
    | option == mempty = ""
    | otherwise        = printf "-%s %s" (cmdString flag) (cmdString option)

instance Shell QSub where
    shellCmd qsub = Cmd cmd
        where
          cmd = let jn = fromQsub JobName (jobName qsub)
                    ea = fromQsub EmailAddress (emailAddress qsub)
                    et = fromQsub EmailTrigger (emailOn qsub)
                    d  = fromQsub WorkDir (workingDir qsub)
                in printf "echo '%s' | qsub %s" (fromCmd . job $ qsub) (intercalate " " [jn, ea, et, d])

test = fromCmd $ shellCmd config
    where config = QSub {
                     job          = Cmd "foo"
                   , jobName      = "name"
                   , emailAddress = "foo@bar.baz"
                   , emailOn      = [JobStarts, JobEnds]
                   , workingDir   = "."
                   }
