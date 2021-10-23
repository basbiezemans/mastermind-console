module Strict where

import System.IO

readFile :: String -> IO String
readFile filePath = do
    fh <- openFile filePath ReadMode
    xs <- hGetContents' fh -- strict
    hClose fh
    return xs

-- The hGetContents' operation reads all input on the given handle
-- before returning it as a String.
hGetContents' :: Handle -> IO String
hGetContents' fh = do
    eof <- hIsEOF fh
    if eof then
        return []
    else do
        c <- hGetChar fh
        cs <- hGetContents' fh
        return (c:cs)
