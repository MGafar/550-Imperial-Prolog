%%%%% Natural Language Program

sentence(S) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP, VP, S).

noun_phrase(NP) :-
	article(A),
	noun(N),
	append(A, N, NP).

verb_phrase(V) :-
	verb(V).
verb_phrase(VP) :-
	verb(V),
	noun_phrase(NP),
	append(V, NP, VsicP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 	a.  conj(Text) */

%Base case to verify the input text is a sentence.
conj(Text) :-
	sentence(Text).

%Recursive function to verify the text is composed from a set of sentences "T" with connectives "and".
conj(Text) :-
	append(H, [and|T], Text),
	sentence(T),
	conj(H).

/* 	b.  encode(Text, EncodedText) */

%Main predicate which calls a tail recursive predicate with an accumulator.
encode(T, ET) :-
	encode_acc(T, [], ET).

%Base case for the recursive predicate where the final list is the same as the accumulator if the input list is empty.
encode_acc([], Acc, Acc).

%Tail recursive predicate which encrypts the header then stores it in an accumulator list.
encode_acc([H|T], Acc, ET) :-
	(noun([H]) ->  encode_word(H, EnWord); H = EnWord),
	append(Acc, [EnWord], L),
	encode_acc(T, L, ET).

%Auxilary predicate, used to encrypt a word that is a noun. 
encode_word(Word, EnWord) :-
	is_animate(Word, Char1),
	length_of_word(Word, Char2),
	atom_chars(Word, [Char3|_]),
	atom_chars(EnWord, [Char1, Char2, Char3]).

%Auxilary predicate used to verify if an input word is a noun. Char1 used to represent the type of noun the input word is (animate/non-animate).
is_animate(Word, Char1) :-
	animate(AnimateList),
	(member(Word, AnimateList) -> Char1 = a; Char1 = d).

%Auxilary predicate to find the number of characters in a word then Char2 is used to characterize the word as (short/long).
length_of_word(Word, Char2) :-
	atom_chars(Word, CharList),
	length(CharList, Length),
	(Length > 3 -> Char2 = l; Char2 = s).

/* 	c.  same_actor(Text) */

%Main predicate used to find a list of all actors in an input text then verifying there is only one actor in the list.
same_actor(Text) :-
	setof(Actor, is_actor_in(Actor, Text), ActorsList),
	length(ActorsList, 1).
	
%Auxilary predicate to find an actor within an input text.
is_actor_in(Actor, Text) :-
	append(_, [Actor,Action|_], Text),
	noun([Actor]),
	verb([Action]).
