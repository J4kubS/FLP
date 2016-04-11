{-|
Module     : Main
Copyright  : (c) Jakub Šoustar, 2016
License    : MIT
Maintainer : Jakub Šoustar <jakub.soustar@gmail.com>, xsoust02 <xsoust02@stud.fit.vutbr.cz>

Part of the rv-2-rka (re-2-efa) project.
This is the application's main module.
-}

module Main where

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Environment
import System.IO

import Automaton
import Options
import Parser

{-|
If the input is a valid regular expression (RE) in postfix notation,
prints representation of the RE and/or equivalent extended
finite automaton.
-}
run :: String -> [Option] -> IO ()
run input flags = do
	let tree = Parser.parseTree . Parser.tokenize $ input

	when (Options.Representation `elem` flags) $ print tree
	when (Options.Automaton `elem` flags) $ print $ Automaton.fromTree tree

{-|
Program's entry point. Run with -h option to see help.
Reads a single line from supplied file (defaults to stdin).
-}
main :: IO ()
main = do
	(flags, file) <- getArgs >>= parseOptions

	when (Options.Representation `elem` flags || Options.Automaton `elem` flags) $
		if isJust file
			then do
				fileHandle <- openFile (fromJust file) ReadMode
				result <- try $ hGetLine fileHandle :: IO (Either IOError String)

				case result of
					Right input -> run input flags
					Left _ -> run "" flags

				hClose fileHandle

			else do
				result <- try getLine :: IO (Either IOError String)

				case result of
					Right input -> run input flags
					Left _ -> run "" flags
