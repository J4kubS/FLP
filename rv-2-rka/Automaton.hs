{-|
Module     : Main
Copyright  : (c) Jakub Šoustar, 2016
License    : MIT
Maintainer : Jakub Šoustar <jakub.soustar@gmail.com>, xsoust02 <xsoust02@stud.fit.vutbr.cz>

Part of the rv-2-rka (re-2-efa) project.
This module provides functionality to create extended finite automaton from a parse tree.
The automaton is generated using <https://en.wikipedia.org/wiki/Thompson%27s_construction Thompson's construction>.
-}

module Automaton
( Automaton (..)
, fromTree
)
where

import qualified Data.List as List
import qualified Data.Set as Set

import Parser

{-|
Represents an extended finite automaton.
-}
data Automaton = Automaton
	{ transitions :: Set.Set Transition -- ^ Partial transition function.
	, states :: Set.Set State           -- ^ States are continuous sequence of integers, starting at 0.
	, startState :: State               -- ^ Minimum from states.
	, finalState :: State               -- ^ Maximum from states. All constructed automatons have exactly one final state.
	}

{-|
Represents a transition function. (p, s, q) <=> delta(p, s) -> {q}.
-}
type Transition = (State, Symbol, State)

{-|
Represents a state of automaton.
-}
type State = Int

{-|
Textual representation of the automaton in the following format:

	(1) List of comma separated states on a single line

	(2) Start state on a single line

	(3) Final state on a single line

	(4) List of transitions, each on a single line, in the following format:
		/p/ \<comma\> /s/ \<comma\> /q/. This represents transition of &#948;(/p/, /s/) &#8594; {/q/}.
-}
instance Show Automaton where
	show automaton =
		states' ++ "\n" ++ startState' ++ "\n" ++ finalState' ++ "\n" ++ transitions'
		where
			transitions' = List.intercalate "\n" . List.map showTransition . Set.toList $ transitions automaton
			showTransition (s, c, f) = show s ++ "," ++ show c ++ "," ++ show f

			states' = List.intercalate "," . List.map show . Set.toAscList $ states automaton
			startState' = show $ startState automaton
			finalState' = show $ finalState automaton

{-|
Construct an extended finite automaton from a parse tree.
Construction follows <https://en.wikipedia.org/wiki/Thompson%27s_construction Thompson's construction>.
-}
fromTree :: ParseTree -> Automaton
fromTree (Leaf (Symbol symbol)) =
	Automaton
	{ transitions = transitions'
	, startState = startState'
	, finalState = finalState'
	, states = states'
	}
	where
		transitions'
			-- Automaton will accept a single symbol.
			| symbol /= Empty = Set.singleton (startState', symbol, finalState')
			-- Automaton will not accept any symbols.
			| otherwise = Set.empty
		states' = Set.fromList [startState', finalState']
		startState' = 0
		finalState' = 1

fromTree (Branch (Operator Concatenation) [branch1, branch2]) =
	concatenate (fromTree branch1) (fromTree branch2)

fromTree (Branch (Operator Alternation) [branch1, branch2]) =
	alternate (fromTree branch1) (fromTree branch2)

fromTree (Branch (Operator KleeneStar) [branch]) =
	kleeneStar . fromTree $ branch

{-|
Construct new automaton by applying concatenation operator on the two provided automatons.
-}
concatenate :: Automaton -> Automaton -> Automaton
concatenate automata1 automata2 =
	Automaton
	{ transitions = transitions'
	, startState = startState'
	, finalState = finalState'
	, states = states'
	}
	where
		-- Shift states to start at final state of automata1.
		-- Final state of automata1 will overlap with start state of automata2'.
		automata2' = shiftStates (Set.findMax (states automata1)) automata2
		-- Merge both, now shifted, transition functions.
		transitions' = Set.union (transitions automata1) (transitions automata2')
		-- Merge states of both automatons.
		states' = Set.union (states automata1) (states automata2')
		-- New final state.
		finalState' = finalState automata2'
		-- New start state.
		startState' = startState automata1

{-|
Construct new automaton by applying alternation operator on the two provided automatons.
-}
alternate :: Automaton -> Automaton -> Automaton
alternate automata1 automata2 =
	Automaton
	{ transitions = transitions'
	, startState = startState'
	, finalState = finalState'
	, states = states'
	}
	where
		-- Add new start and final states.
		automata1'' = newFinalState finalState' . newStartState startState' $ automata1'
		-- Make room for new start state.
		automata1' = shiftStates 1 automata1
		-- Add new start and final states.
		automata2'' = newFinalState finalState' . newStartState startState' $ automata2'
		-- Shift states to start after the states of automata1'.
		automata2' = shiftStates (Set.findMax (states automata1') + 1) automata2

		-- Merge both, now shifted, transition functions.
		transitions' = Set.union (transitions automata1'') (transitions automata2'')
		-- Merge states of both automatons and add new start and final states.
		states' = Set.insert finalState' . Set.insert finalState' . Set.union (states automata1'') $ states automata2''
		-- New final state.
		finalState' = Set.findMax (states automata2') + 1
		-- New start state.
		startState' = 0

{-|
Construct new automaton by applying Kleene star operator on the provided automaton.
-}
kleeneStar :: Automaton -> Automaton
kleeneStar automaton =
	Automaton
	{ transitions = transitions'
	, startState = startState'
	, finalState = finalState'
	, states = states'
	}
	where
		-- Add new start and final states.
		automaton'' = newFinalState finalState' . newStartState startState' $ automaton'
		-- Make room for new start state.
		automaton' = shiftStates 1 automaton

		-- Add transitions for Kleene star.
		transitions' = Set.insert (finalState'', Epsilon, startState'') . Set.insert (startState', Epsilon, finalState') $ transitions automaton''
		-- New, extended, states
		states' = states automaton''

		-- Old start state.
		startState'' = startState automaton'
		-- New start state.
		startState' = 0

		-- Old final state.
		finalState'' = finalState automaton'
		-- New final state.
		finalState' = Set.findMax (states automaton') + 1

{-|
Shift all automaton's states by offset.
-}
shiftStates :: Int -> Automaton -> Automaton
shiftStates offset automaton =
	automaton
	{ transitions = transitions'
	, startState = startState'
	, finalState = finalState'
	, states = states'
	}
	where
		transitions' = Set.map (\ (s, c, f) -> (s + offset, c, f + offset)) $ transitions automaton
		states' = Set.map (+ offset) $ states automaton
		startState' = startState automaton + offset
		finalState' = finalState automaton + offset

{-|
Inset new start state into the automaton.
The old start state is linked with the new one with epsilon transition.
-}
newStartState :: State -> Automaton -> Automaton
newStartState startState' automaton =
	automaton
	{ transitions = transitions'
	, startState = startState'
	, states = states'
	}
	where
		transitions' = Set.insert (startState', Epsilon, startState automaton) $ transitions automaton
		states' = Set.insert startState' $ states automaton

{-|
Inset new final state into the automaton.
The old final state is linked with the new one with epsilon transition.
-}
newFinalState :: State -> Automaton -> Automaton
newFinalState finalState' automaton =
	automaton
	{ transitions = transitions'
	, finalState = finalState'
	, states = states'
	}
	where
		transitions' = Set.insert (finalState automaton, Epsilon, finalState') $ transitions automaton
		states' = Set.insert finalState' $ states automaton
