module FDNS.ExitCode where

data EXIT_CODE =  OK          -- Status code 0
                | ARGUMENTS   -- Status code 1
                deriving (Show, Enum)