%%  File        : fillin.pl
%%  Author      : Jianyu Zhu
%%  Student ID  : 734057
%%  Purpose     : A solution to the fillin puzzles

%% | This project is implemented to find solutions to the fillin puzzles.
%%   In the code, the project is divided into several parts, including
%%   the process of initial puzzles, generation of all fill-able slots,
%%   matches between the slots and the list of words, and the calculation
%%   of the final solution.

%% | The following part of code is provided, including the main predicate
%%   as well as other necessary predicates for file reading, writing, and
%%   test.

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(clpfd)).

%% | The initial process of fillin puzzles

%% predicate name : process_puzzle_in_row/3
%% this predicate changes all '_' in a list into a varable with empty
%% value.

process_puzzle_in_row([],Acc,Acc).
process_puzzle_in_row([Head|Tail],Acc,Row):-
	(   Head = '_' ->
	        append(Acc,[Empty],NewAcc);
	        append(Acc,[Head],NewAcc)
	),
	process_puzzle_in_row(Tail,NewAcc,Row).

%% predicate name : process_puzzle/3
%% this predicate changes a puzzle into a new puzzle which has its '_'s
%% substituted to variables without value

process_puzzle([],Acc,Acc).
process_puzzle([FstRow|RestRows],Acc,Puzzle):-
	process_puzzle_in_row(FstRow,[],Row),
	append(Acc,[Row],NewAcc),
	process_puzzle(RestRows,NewAcc,Puzzle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% | Find all slots in the puzzle

%% predicate name : find_slots_in_row/3
%% this predicate returns the slots in a list. Slot is defined as less
%% two continious variables, and all slots found will be stored in a
%% list, so the accumulator should be a list of list.

find_slots_in_row([],[],Acc,Acc).
find_slots_in_row([],Slot_tmp,Acc,Slots):-
    %% Once the end of list is reached, check the slot_tmp. If it is
    %% long enough to be a real slot, add it to the accumulator.
	length(Slot_tmp,L),
	(   L =< 1 ->
			find_slots_in_row([],[],Acc,Slots);
			append(Acc,[Slot_tmp],NewAcc),
	        find_slots_in_row([],[],NewAcc,Slots)
	).
find_slots_in_row([Head|Tail],Slot_tmp,Acc,Slots):-
    %% If the head of list is not '#', add the head element to the end
    %% of the slot_tmp.
	(   nonvar(Head) ->
	    (   Head \= '#' ->
	           append(Slot_tmp,[Head],NewSlot_tmp),
	           find_slots_in_row(Tail,NewSlot_tmp,Acc,Slots);
	           %% Once the '#' in the list is reached, check the slot_tmp.
               %% If itis long enough to be a real slot, add it to the
               %% accumulator, and the slot_tmp should be reset to [].
		       length(Slot_tmp,L),
	           (   L >= 2 ->
	                   append(Acc,[Slot_tmp],NewAcc),
	                   find_slots_in_row(Tail,[],NewAcc,Slots);
	                   find_slots_in_row(Tail,[],Acc,Slots)
	           )
	    );
	    append(Slot_tmp,[Head],NewSlot_tmp),
	    find_slots_in_row(Tail,NewSlot_tmp,Acc,Slots)
	).

%% predicate name : find_slots_in_horizon/3
%% call the predicate 'find_slots_in_row' for each row in the horizontal
%% direction. Find all slots and store them in a list.

find_slots_in_horizon([],Acc,Acc).
find_slots_in_horizon([FstRow|RestRow],Acc,Slots):-
	find_slots_in_row(FstRow,[],[],Slots_in_row),
	append(Slots_in_row,Acc,NewAcc),
	find_slots_in_horizon(RestRow,NewAcc,Slots).

%% predicate name : find_slots_in_puzzle/3
%% call predicate 'find_slots_in_horizon' in the transpose of itself as
%% well as itself. Find all Slots in a puzzle and store them in a list.

find_slots_in_puzzle([],Acc,Acc).
find_slots_in_puzzle(Puzzle,Acc,Slots):-
	find_slots_in_horizon(Puzzle,[],Slots_in_horizon),
	append(Slots_in_horizon, Acc, NewAcc),
	transpose(Puzzle,Vertical_puzzle),
	find_slots_in_horizon(Vertical_puzzle,[],Slots_in_vertical),
	append(Slots_in_vertical,NewAcc,Fin_Acc),
	find_slots_in_puzzle([],Fin_Acc,Slots).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% | The next step is to match the slots and the word in the list

%% predicate name : find_match_word_of_slots/3
%% this predicate is to find the list of words that match each slots,
%% and it calls the predicate 'find_match_word' for each slot.
%% The first argument of this predicate is a list of slots, the second
%% is the list of words, and the last one the the matched words for
%% each slot.

find_match_word_of_slots([],_,[]).
find_match_word_of_slots([Head|Tail],Wordlist,Matchedlist_for_words):-
	find_match_word(Head,Wordlist,Matched_list),
	append([Matched_list],Matchedlist_for_words_tmp,Matchedlist_for_words),
	find_match_word_of_slots(Tail,Wordlist,Matchedlist_for_words_tmp).

%% predicate name : find_match_word/3
%% this predicate is to find the list of words that match a slot,
%% and it calls the predicate 'match' for comparison between a slot and a
%% word.

find_match_word(_,[],[]).
find_match_word(Slot,[Head|Tail],Matched_words):-
	%% check the lenghts of the slot and the word first.
    length(Slot,L1),
    length(Head,L2),
    (   L1 = L2 ->
            match(Slot,Head,Result),
            (   Result = true ->
                append([Head],Matched,Matched_words),
                find_match_word(Slot,Tail,Matched);
                find_match_word(Slot,Tail,Matched_words)
            );
	    find_match_word(Slot,Tail,Matched_words)
    ).

%% predicate name : match/3
%% this predicate is to pare up the slot and a specific word. If success
%% the argument 'Result' should be true.

match([],[],true).
match([Head1|Tail1],[Head2|Tail2],Result):-
	(   var(Head1) ->
		%%Head1 is empty value, so it matches with every letters.
	        match(Tail1,Tail2,Result);
		nonvar(Head1),nonvar(Head2),Head1 = Head2 ->
	        match(Tail1,Tail2,Result);
	    Result = false
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% | The final step is to find the solution of a fillin puzzle

%% predicate name : index_of_slot_with_least_matches/2
%% this predicate is to find the index of the slot that has least matched
%% words. It calls 'lengths_of_each_element' for the calculation of
%% number of matched words for each slot.

index_of_slot_with_least_matches(Matchedlist_for_words,Index):-
	lengths_of_each_element(Matchedlist_for_words,Lengths),
	min_list(Lengths,Least),
	nth0(Index,Lengths,Least).

%% predicate name : lengths_of_each_element/2
%% this predicate is to calculate the length of each element in the list,
%% so it is a list of list.

lengths_of_each_element([],[]).
lengths_of_each_element([Head|Tail],Lengths):-
	length(Head,L),
	append([L],Lengths_of_Rest,Lengths),
	lengths_of_each_element(Tail,Lengths_of_Rest).

%% predicate name : fill/2
%% this predicate chooses the word to fill a specific slot.

fill_slot(Word,[Word]).
fill_slot(Word,[Head|Tail]):-
	Word = Head;
	fill_slot(Word,Tail).

%% predicate name : shrunk_list/3
%% this predicate is to remove specific elements from the list.

shrunk_list([],_,[]).
shrunk_list([Head|Tail],Elements_to_remove,Remain):-
	check(Head,Elements_to_remove),
	shrunk_list(Tail,Elements_to_remove,Remain).
shrunk_list([Head|Tail],Elements_to_remove,[Head|Remain]):-
	shrunk_list(Tail,Elements_to_remove,Remain).

%% predicate name : check/2
%% this predicate finds whether a value is inthe list.

check(Elem, [Head|_]) :-
	Elem == Head.
check(Elem, [_|Tail]) :-
	check(Elem, Tail).

%% predicate name : find_solution/2
%% this predicate is to generate the solution of the fillin puzzle by
%% filling all words in list to all the slots.

find_solution([],[]).
find_solution(Slots,Wordlist):-
	%% generate the lists of matched words for all slots.
	find_match_word_of_slots(Slots,Wordlist,Matchedlist_for_slots),
	%% find the index of slot that has least matched words.
	index_of_slot_with_least_matches(Matchedlist_for_slots,Index),
	%% find the slot with least matched words.
	nth0(Index,Slots,Slot_in_position),
	nth0(Index,Matchedlist_for_slots,Matchedlist_in_position),
	%% fill the slot.
	fill_slot(Slot_in_position,Matchedlist_in_position),
	%% remove the filled slot and the word that is used from the list.
	shrunk_list(Slots,[Slot_in_position],Rest_slots),
	shrunk_list(Wordlist,[Slot_in_position],Rest_wordlist),
	%% find next slot to fill
	find_solution(Rest_slots,Rest_wordlist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% | the predicate to generate the solution puzzle.

solve_puzzle(Puzzle, Wordlist, Solution):-
	process_puzzle(Puzzle,[],Solution),
	find_slots_in_puzzle(Solution,[],Slots),
	find_solution(Slots,Wordlist).
