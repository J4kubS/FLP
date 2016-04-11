{-|
Module     : Main
Copyright  : (c) Jakub Šoustar, 2016
License    : MIT
Maintainer : Jakub Šoustar <jakub.soustar@gmail.com>, xsoust02 <xsoust02@stud.fit.vutbr.cz>

Part of the rv-2-rka (re-2-efa) project.
This module handles command line options.
-}

module Options
( Option (Representation, Automaton)
, parseOptions
) where

import System.Console.GetOpt
import System.Exit
import System.IO

{-|
Represents options without arguments.
-}
data Option
	= Representation -- ^ Print representation of the regular expression.
	| Automaton      -- ^ Print representation of the extended finite automaton.
	| Help           -- ^ Print help and exit.
	deriving Eq

{-|
Recognized options.
-}
options :: [OptDescr Option]
options =
	[ Option ['r'] ["representation"] (NoArg Representation) "Print representation of the regular expression."
	, Option ['t'] ["automaton"]      (NoArg Automaton)      "Print representation of the extended finite automaton."
	, Option ['h'] ["help"]           (NoArg Help)           "Print this help and exit."
	]

{-|
Parse command line options.
-}
parseOptions :: [String] -> IO ([Option], Maybe String)
parseOptions cliOpts = case getOpt RequireOrder options cliOpts of
	(opts, nonOpts, [])
		| Help `elem` opts -> do
			hPutStrLn stderr usage
			exitSuccess

		| not $ null opts ->
			if not $ null nonOpts
				then return (opts, Just (head nonOpts))
				else return (opts, Nothing)

		| otherwise -> error usage

	(_, _, errors) -> error $ "\n" ++ concat errors ++ "\n" ++ usage

	where
		header = "Usage: rv-2-rka [options] [file]"
		usage = usageInfo header options
