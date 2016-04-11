{-|
Module     : Parser
Copyright  : (c) Jakub Šoustar, 2016
License    : MIT
Maintainer : Jakub Šoustar <jakub.soustar@gmail.com>, xsoust02 <xsoust02@stud.fit.vutbr.cz>

Part of the rv-2-rka (re-2-efa) project.
This module provides lexical and syntactic parsing capabilities.
The scanner converts an input string into list of tokens.
The parser converts a list of tokens into a parse tree.
-}

module Parser
( Operator (..)
, Symbol (..)
, Token (..)
, ParseTree (..)
, parseTree
, tokenize
)
where

import Data.Char

{-|
Represents a recognized operator.
-}
data Operator
	= Concatenation -- ^ Concatenation of two regular expressions.
	| Alternation   -- ^ Alternation of two regular expressions.
	| KleeneStar    -- ^ Kleene star of regular expression.
	deriving Eq

{-|
Represents a recognized symbol.
-}
data Symbol
	= Character Char -- ^ Single a-z (lowercase) character.
	| Epsilon        -- ^ Empty string.
	| Empty          -- ^ Empty input. Used to handle special case when no input is provided.
	deriving (Eq, Ord)

{-|
Represents a lexical token. Token may either be an operator or a symbol.
-}
data Token
	= Operator Operator
	| Symbol Symbol
	deriving Eq

{-|
Represents a parse tree of tokens.
-}
data ParseTree
	= Branch Token [ParseTree] -- ^ Branch node with 0-n children.
	| Leaf Token               -- ^ Leaf node.

{-|
Represents a forest of parse trees. Used for regular expression validation.
-}
type ParseForest = [ParseTree]

instance Show Operator where
	show Concatenation = "."
	show Alternation = "+"
	show KleeneStar = "*"

instance Show Symbol where
	show (Character char) = [char]
	show Epsilon = ""
	show Empty = ""

instance Show Token where
	show (Operator operator) = show operator
	show (Symbol symbol) = show symbol

{-|
Textual representation of parse tree in postfix notation.
-}
instance Show ParseTree where
	show tree = case tree of
		(Branch token branches) -> concatMap show branches ++ show token
		(Leaf token) -> show token

{-|
Parse a list of tokens into parse tree forest.
-}
parseForest :: [Token] -> ParseForest
parseForest = foldl foldToken []
	where
		-- Concatenation. Pop two nodes from the stack, combine them into new one, and push it onto the stack.
		foldToken (top : top' : stack) (Operator Concatenation)   = Branch (Operator Concatenation) [top', top] : stack
		-- Alternation. Pop two nodes from the stack, combine them into new one, and push it onto the stack.
		foldToken (top : top' : stack) (Operator Alternation)     = Branch (Operator Alternation) [top', top] : stack
		-- Kleene star. Pop a node from the stack, wrap it with new one, and push it onto the stack.
		foldToken (top : stack)        (Operator KleeneStar)      = Branch (Operator KleeneStar) [top] : stack
		-- Character. Create new leaf node and push it onto the stack.
		foldToken stack                (Symbol (Character token)) = Leaf (Symbol (Character token)) : stack
		-- Special case. No input was provided.
		foldToken _                    (Symbol Empty)             = [Leaf (Symbol Empty)]
		foldToken _ _ = error "Parser: ParseForest: Invalid Regular Expression."

{-|
Parse a list of tokens into parse tree.
-}
parseTree :: [Token] -> ParseTree
parseTree tokens
	-- RE is valid only if tokens can be reduced into a single parse tree.
	| length forest == 1 = head forest
	-- Trees, trees everywhere!
	| otherwise = error "Parser: ParseTree: Incomplete Regular Expression."
	where
		forest = parseForest tokens

{-|
Convert a string into a list of tokens.
Recognized tokens are:

    - Empty string

    - Lowercase characters from English alphabet ([a-z])

    - Plus sign (+)

    - Asterisk (*)

    - Dot (.)
-}
tokenize :: String -> [Token]
tokenize string
	| not $ null string = map convert string
	| otherwise         = [Symbol Empty]
	where
		convert char
			| isAsciiLower char = Symbol (Character char)
			| char == '.'       = Operator Concatenation
			| char == '+'       = Operator Alternation
			| char == '*'       = Operator KleeneStar
			| otherwise	= error $ "Parser: Tokenize: Invalid Symbol " ++ show char ++ "."
