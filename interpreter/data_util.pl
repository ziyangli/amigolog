
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% conversion

%% -- term_string(?String, ?Term)
term_string(S, T) :-
        ground(S),
        string_to_atom(S, A),
        term_to_atom(T, A), !.
term_string(S, T) :-
        ground(T),
        term_to_atom(T, A),
        string_to_atom(S, A), !.

%% -- number_string(?Number, ?String)
number_string(N, S) :-
        ground(N),
        number_chars(N, LChar),
        string_to_list(S, LChar), !.
number_string(N, S) :-
        ground(S),
        string_to_atom(S, A),
        atom_codes(A, CA),
        number_codes(N, CA), !.
%% ?- number_string(N, '1.23').
%@ N = 1.23.

%% --
%%    Convert anything into a number
any_to_number(N, N) :- number(N).
any_to_number(A, N) :- atom(A), atom_number(A, N).
any_to_number(S, N) :- string(S), string_to_number(S, N).

%% -- 
%%    Convert anything into a string
any_to_string(S, S) :- string(S), !. %% ?- any_to_string(`abc`, X).
any_to_string(A, S) :- atom(A), string_to_atom(S, A), !.
any_to_string(A, S) :- number(A), string_to_number(S, A), !.
any_to_string(A, S) :- is_list(A), 
        ( (member(X,A), \+ number(X)) -> 
              build_string(A, S)   % Manually build string S
        ;
              string_to_list(S, A) % A is list of char codes!
        ), !.
%% ?- any_to_string([abc, def], S).

any_to_string(A, S) :- \+ is_list(A), compound(A), string_to_term(S, A), !.
any_to_string(A, '_Var') :- var(A), !.

%% --
%%    Convert a list of anything to into a list of strings
lany_to_string([], []).
lany_to_string([A|R], [SA|SR]) :- any_to_string(A, SA),
                                  lany_to_string(R, SR).

%% -- string_to_term(?String, ?Term)
string_to_term(S, T):- var(T), !,    % S ---> T
                       string_to_atom(S, A), term_to_atom(T, A). 
string_to_term(S, T):- \+ var(T),    % T ---> S
                       term_to_atom(T, A), string_to_atom(S, A).

%% -- string_to_number(?String, ?Term)
%% string_to_number('1111', N)
string_to_number(S, N):- ground(N),
                         number_chars(N, L), string_to_list(S, L), !.
string_to_number(S, N):- ground(S),
                         string_to_atom(S, A), 
                         atom_codes(A, CA), 
                         number_codes(N, CA), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% string

%% -- concat_strings(+String1, +String2, ?String3)
%%    succeed if String3 is the concatenation of String1 and String2
concat_strings(String1, String2, String3) :- 
        string_concat(String1, String2, String3).

%% -- concat_string(+List, ?String)
%%    succeed if String is the concatenation of the atomic terms contained in List
concat_string([], EmptyString):- string_to_list(EmptyString,[]).
concat_string([String1|RS], String3):-
        concat_string(RS, RSString2),
        concat_strings(String1, RSString2, String3).

%% -- substring(+String, +SubString, ?Position)
%%    succeed if SubString is a substring of String beginning at Position.
substring(String, SubString, Pos) :- substring(String, Pos, _, SubString).
%% -- substring(+String1, ?Position, ?Length, ?String2)
%%    succeed if String2 is the substring of String1 starting at Position and of Length.
substring(String, Start, Length, SubString):- 
	var(SubString), !,
        sub_string(String, Start, Length, _, SubString).
substring(String, Start, Length, Sub):- 
	string_length(String, LString), 
	LString\=0,
        string_to_atom(Sub, SubAtom),
        sub_string(String, Start, Length, _, SubAtom).
% -- substring(+String, ?Before, ?Length, ?After, ?String2)
%        Succeeds if String2 is a substring of String, with Length, 
%        preceded by Before, and followed by After characters
substring(String, Before, Length, After, SubString) :-
	sub_string(String, Before, Length, After, SubString).

%% -- divide_string(+String, +ListStarts, -LStrings)
%%    Decompose String into SubStrings according to separator locations in ListStarts, index start from 0.
divide_string(_, [_], []).
divide_string(String, [S,E|Rest], [FString|RString]) :-
	S2 is S+1,
	E2 is E-S-1,
	substring(String, S2, E2, FString),
	divide_string(String, [E|Rest], RString).
%% ?- divide_string('Todayisagoodday!', [2, 4, 10, 16], LS).
%@ LS = [a, "isago", "dday!"] 

%% -- remove_front(+LString, +LPadChars, -LString) :
%%    remove any char in LPadChars appearing in the front of LString
remove_front([], _, []) 			:- !.
remove_front(LString, LPadChars, LString) 	:- 
	LString=[C|_], \+ member(C, LPadChars), !.
remove_front([_|LString], LPadChars, LString2) 	:- 
	remove_front(LString, LPadChars, LString2).

%% -- remove_pad(+String, +LPadChars, -StringsNoPad) :
%%    remove any char in LPadChars appearing in the front or at the end of String
remove_pad(String, PadChars, StringsNoPad) :- 
	string_to_list(PadChars, LPadChars),
	string_to_list(String, LString),
	remove_front(LString, LPadChars, LString2),
	reverse(LString2, RLString2),
	remove_front(RLString2, LPadChars, RLStringsNoPad),
	reverse(RLStringsNoPad, LStringsNoPad),
	string_to_list(StringsNoPad, LStringsNoPad).
%% ?- remove_pad('helllo', ['o', 'h', 'e'], A).	
%@ A = "lll".
%% ?- remove_pad('hello', 'he', A).
%@ A = "llo".

%% -- split_string(+String, +SepChars, +PadChars, ?LSubStrings)
%%    decompose String into LSubStrings according to SepChars and PadChars.
split_string(String, SepChars, PadChars, SubStrings):-
	string_to_list(SepChars, LSepChars),
 	% Find all the start positions of separators in the string
	findall(Start, (substring(String, Start, 1, Sep),
	                string_to_list(Sep, [SepChar]),
	                member(SepChar, LSepChars)), ListStarts),
	string_length(String, StringLength),
	append(ListStarts,[StringLength], NListStarts),
	divide_string(String, [-1|NListStarts], SubStrings2),
	findall(S2, (member(S, SubStrings2),
                     remove_pad(S, PadChars, S2)), SubStrings).
%% ?- split_string('hellotoddtttttdayisagoodday', ['e', 'a', 'o'], 't', A).
%@ A = ["h", "ll", "", "ddtttttd", "yis", "g", "", "dd", "y"] ;

%% -- join_string(+List, +Glue, -String): 
%%    String is formed by concatenating the elements of List with an instance of Glue beween each of them.
join_string([], _, String) :- !,
	string_to_list(String,[]).
join_string([E|R], Glue, String) :- 
	string_concat(E, '', SE), % Convert anything into a string
	join_string(R, Glue, StringR),
	string_length(StringR, LStringR),
	(LStringR=0 -> concat_string([SE, StringR], String) ; 
		       concat_string([SE, Glue, StringR], String)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% temp

%% -- replace_char_string(+String, +Char, +Char2, -String2)
%%    String2 is String with Char replaced by Char2
replace_char_string(S, E1, E2, S2) :- 
        atom_codes(E1,[CE1]),
        atom_codes(E2,[CE2]),
        string_to_list(S,SL),
        replace_element_list(SL,CE1,CE2,SL2),
        string_to_list(S2,SL2).


%% --
%%    check if String is empty
emptyString(S) :- string_to_list(S,[]).

build_string([], S)    :- emptyString(S).
build_string([E|R], S) :- 
        build_string(R, S2),
        any_to_string(E, SE),
        concat_string([SE, ` `, S2],S).

% -- string_replace(S, E1, E2, S2)
%       String/atom S2 is string/atom S with all chars E1 replaced by E2
string_replace(S, E1, E2, S2) :- 
        lany_to_string([S,E1,E2],[SS,SE1,SE2]), 
        emptyString(ES),
        split_string(SS, SE1, ES, L),
        join_string(L, SE2, S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list

%% min/max
%% check min_list and max_list

%% -- get_random_element(-Element, +Domain)
get_random_element(Ele, Domain) :-
        length(Domain, Len),
        Len>0,
        I is random(Len),
        nth0(I, Domain, Ele).

%% -- shuffle(+List, -ShuffledList)
shuffle([], []).
shuffle(L, SL) :-
        get_random_element(E, L),
        delete(L, E, L2),
        shuffle(L2, NSL), SL=[E|NSL].

%% -- buildListRepeat(N,E,L)
%%    L is a list of N repetitions of element E
buildListRepeat(0,_,[])    :- !.
buildListRepeat(N,H,[H|L]) :- N2 is N-1, buildListRepeat(N2,H,L).

%% -- replace_element_list(+List,+Element,+Element2,-List2)
%%    List2 is List with Element replaced by Element2
replace_element_list([],_,_,[]).
replace_element_list([CE1|R],CE1,CE2,[CE2|RR]):- !,
        replace_element_list(R,CE1,CE2,RR).
replace_element_list([E|R],CE1,CE2,[E|RR]):- 
        replace_element_list(R,CE1,CE2,RR).

%% -- sublist(?SubList, +List)
%%    succeed if List is the list which contains all elements from SubList 
sublist([],_).
sublist([X|R], L) :-
	member(X,L),
	sublist(R,L).
%% ?- sublist([3, 3, 4], [3, 4]).
%@ true ;
%@ false.

/*  T2 is T1 with X1 replaced by X2  */
%% if T1 is a compound, var or interger. 
subv(X1,X2,T1,T2) :- var(T1), T1 == X1, !, T2 = X2. %% if X1 and X2 are grounded
subv(_,_,T1,T2)   :- var(T1), !, T2 = T1.           %%
subv(X1,X2,T1,T2) :- T1 == X1, !, T2 = X2.          %% if T1 is grounded, integer or others
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), !, T2 =..[F|L2]. %% translate compound to a list, and translate the result list back into a compund
%% ?- subv(X, 2, X, Y).
%@ Y = 2.
%% ?- subv(3, 4, [a(3), 3], Y).
%@ Y = [a(4), 4] ;

subvl(_,_,[],[]).
subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), subvl(X1,X2,L1,L2).
%% ?- subvl(3, 4, [a(3), 3, 3.3], Y).
%@ Y = [a(4), 4, 3.3]

% from vanilla
%        /*  T2 is T1 with X1 replaced by X2  */
% subv(_,_,T1,T2) :- (var(T1); integer(T1)), !, T2 = T1.
% subv(X1,X2,T1,T2) :- T1 = X1, !, T2 = X2.
% subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), T2 =..[F|L2].

% subvl(_,_,[],[]).
% subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), subvl(X1,X2,L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% math

%% -- get_integer(+Low, ?N, +High) :
%%    N is between Low and High (included)
get_integer(L, L, H) :- L=<H.
get_integer(L, N, H) :- L<H, L2 is L+1, get_integer(L2, N, H).
%% ?- get_integer(1, N, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% atom

%% -- liftAtom(?Atom, ?NameA, ?ArgA, -LiftedAtom)
%%    return a compound with all the arguments unground, used in holds/2
liftAtom(Atom, NameA, ArgA, LiftedAtom) :-
	Atom =..[NameA|ArgA],
	length(ArgA, L),
	length(ArgAVars, L),
	LiftedAtom =..[NameA|ArgAVars]. 
%% ?- liftAtom(A, good, [hei, jude], LA).
%@ A = good(hei, jude),
%@ LA = good(_G17, _G18).;

% -- join_atom(List, Glue, Atom)
%      Atom is the atom formed by concatenating the elements of List with an 
%      instance of Glue beween each of them.
join_atom(List, Glue, Atom) :-
        maplist(any_to_string, List, List2),
        join_string(List2, Glue, String),
        string_to_atom(String, Atom).

% -- split_atom(Atom, SepChars, PadChars, SubAtoms) 
%      Decompose atom Atom into SubAtoms according to separators SepChars 
%      and padding characters PadChars.
split_atom(Atom, SepChars, PadChars, SubAtoms) :-
        string_to_atom(SA1, Atom),
        string_to_atom(SA2, SepChars),
        string_to_atom(SA3, PadChars),
        split_string(SA1, SA2, SA3, SL),
        maplist(string_to_atom, SL, SubAtoms).
