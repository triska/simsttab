/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Simsttab -- Simplistic school time tabler
  Copyright (C) 2005, 2014 Markus Triska triska@gmx.at

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- use_module(library(clpfd)).
:- use_module(library(sgml)).

:- dynamic req/4, coupling/4, teacher_freeday/2, slots_per_day/1,
	   num_slots/1, free_slot/2, room_alloc/4.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 Posting constraints

   Initially, a requirement is represented as a term req(C,S,T,N),
   meaning that N times a week (on different days), class C is to be
   taught subject S by teacher T. A requirement R is then transformed
   to a term R-Ls, where Ls is a list of length N. The elements of
   this list are variables and act as placeholders for the time slots
   of the scheduled lessons of requirement R. To break symmetry, the
   elements of Ls are constrained to be strictly ascending (it follows
   that they are all_different). The time slots of each teacher are
   constrained to be all_different/1. The time slots divided by
   slots_per_day are constrained to be strictly ascending (= enforce
   distinct days), except for coupled lessons. The time slots of each
   class, and of lessons occupying the same room, are constrained to
   be all_different/1. Labeling is performed on all slot variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


all_reqs(Rs) :-
        setof(req(Class,Sub,Teacher,Num), req(Class,Sub,Teacher,Num), Rs0),
        maplist(req_with_joblist, Rs0, Rs).

all_classes(Classes) :- setof(C, S^N^T^req(C,S,T,N), Classes).

all_teachers(Teachers) :- setof(T, C^S^N^req(C,S,T,N), Teachers).

all_rooms(Rooms) :-
        findall(Room, room_alloc(Room,_C,_S,_Slot), Rooms0),
        sort(Rooms0, Rooms).

timetab_(Rs, Vars) :-
        all_reqs(Rs),
        reqs_varlist(Rs, Vars),
        num_slots(Numslots),
        Numslots1 is Numslots - 1,
        Vars ins 0..Numslots1,
        maplist(constrain_subject, Rs),
        all_classes(Classes),
        all_teachers(Teachers),
        all_rooms(Rooms),
        maplist(constrain_teacher(Rs), Teachers),
        maplist(constrain_class(Rs), Classes),
        maplist(constrain_room(Rs), Rooms).

slot_quotient(S, Q) :-
        slots_per_day(SPD),
        Q #= S / SPD.


ignore_nths(Is, Es0, Es) :- phrase(ignore_(Is, 0, Es0), Es).

ignore_([], _, Es) --> list(Es).
ignore_([I|Is], Pos0, [E|Es]) -->
        { Pos1 #= Pos0 + 1 },
        (   { I =:= Pos0 } -> ignore_(Is, Pos1, Es)
        ;   [E],
            ignore_([I|Is], Pos1, Es)
        ).


%:- ignore_nths([3], [a,b,c,d], [a,b,c]).
%:- ignore_nths([1,2], [a,b,c,d], [a,d]).

slots_couplings(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.

constrain_subject(req(Class,Subj,_Teacher,_Num)-Slots) :-
        strictly_ascending(Slots), % break symmetry
        maplist(slot_quotient, Slots, Qs0),
        findall(F-S, coupling(Class,Subj,F,S), Cs),
        maplist(slots_couplings(Slots), Cs),
        findall(Second, coupling(Class,Subj,_First,Second), Couplings0),
        sort(Couplings0, Couplings1),
        ignore_nths(Couplings1, Qs0, Qs),
        strictly_ascending(Qs).


all_diff_from(Vs, F) :- maplist(#\=(F), Vs).

constrain_class(Rs, Class) :-
        include(class_req(Class), Rs, Sub),
        reqs_varlist(Sub, Vs),
        all_different(Vs),
        findall(S, free_slot(Class,S), Frees),
        maplist(all_diff_from(Vs), Frees).


constrain_teacher(Rs, Teacher) :-
        include(teacher_req(Teacher), Rs, Sub),
        reqs_varlist(Sub, Vs),
        all_different(Vs),
        (   teacher_freeday(Teacher,F) ->
            maplist(slot_quotient, Vs, Qs),
            all_diff_from(Qs, F)
        ;   true
        ).


sameroom_var(Reqs, r(Class,Subject,Lesson), Var) :-
        memberchk(req(Class,Subject,_Teacher,_Num)-Slots, Reqs),
        nth0(Lesson, Slots, Var).

constrain_room(Reqs, Room) :-
        findall(r(Class,Subj,Less), room_alloc(Room,Class,Subj,Less), RReqs),
        maplist(sameroom_var(Reqs), RReqs, Roomvars),
        all_different(Roomvars).


strictly_ascending(Ls) :- chain(Ls, #<).

%room(r1,'1a',sjk,[1,2,3,4]).
%room(r1,'1b',sjk,[1,2,3,4]).
%room(r1,'1c',sjk,[1,2,3,4]).
%room(r1,'1d',sjk,[1,2,3,4]).

%coupling('1a',sjk,1,2).

%teacher_freeday(2,2).
%teacher_freeday(1,4).
%teacher_freeday(3,0).



req_with_joblist(R, R-Slots) :- arg(4, R, N), length(Slots, N).


class_req(C, req(C,_S,_T,_N)-_List).

teacher_req(T, req(_C,_S,T,_N)-_List).


reqs_varlist(Rs, Vs) :- phrase(reqs_varlist_(Rs), Vs).

reqs_varlist_([]) --> [].
reqs_varlist_([req(_C,_S,_N,_T)-Vars|Rs]) -->
        list(Vars),
        reqs_varlist_(Rs).

list([])     --> [].
list([E|Es]) --> [E], list(Es).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			       Printing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Teachers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_teachers(Rs) :-
        all_teachers(Ts),
        maplist(print_teacher(Rs), Ts).

print_teacher(Rs, Teacher) :-
        format("\n\n\n\nTeacher: ~w\n", [Teacher]),
        include(teacher_req(Teacher), Rs, Sub),
        print_objects(teacher_nth, Sub).


teacher_nth(Rs, N, C/Subj) :-
        member(req(C,Subj,_,_)-Times, Rs),
        member(N, Times).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Classes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_classes(Rs) :-
        all_classes(Cs),
        maplist(print_class(Rs), Cs).

print_class(Rs, Class) :-
        format("\n\n\n\nClass: ~w\n", Class),
        include(class_req(Class), Rs, Sub),
        print_objects(class_nth, Sub).

class_nth(Rs, N, Subj) :-
        member(req(_,Subj,_,_)-Times, Rs),
        member(N, Times).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Print objects in roster.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_objects(Goal, Rs) :-
        num_slots(NumSlots),
        slots_per_day(SPD),
        length(Ls, NumSlots),
        foldl(print_object_(Goal,Rs,SPD), Ls, 0, _).

print_object_(Goal, Rs, SPD, _, N0, N) :-
        (   0 =:= N0 mod SPD -> nl, nl
        ;   true
        ),
        (   call(Goal, Rs, N0, S) -> true
        ;   S = free
        ),
        format("~w   ",[S]),
        N #= N0 + 1.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parse XML file. This part of the program contains side-effects: It
   asserts facts read from the XML file to make them more conveniently
   accessible in the remainder of the program.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Extract option values from tag attributes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attr_values(Attr, Es, Vs) :-
        must_be(list(ground), Es),
        maplist(list_eqchk(Attr), Es, Vs).

list_eqchk(Attr, E, V) :-
        (   memberchk(E=V0, Attr) -> true
        ;   throw('attribute expected'-E)
        ),
        (   numeric_attribute(E) -> atom_number(V0, V)
        ;   V = V0
        ).

numeric_attribute(amount).
numeric_attribute(lesson1).
numeric_attribute(lesson2).
numeric_attribute(slot).
numeric_attribute(numslots).
numeric_attribute(slotsperday).
numeric_attribute(lesson).
numeric_attribute(day).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Processing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

process_input(File) :-
        load_xml_file(File, AST),
        memberchk(element(requirements,_,Content), AST),
        process_globals(Content),
        process_nodes(class, Content, process_class),
        process_nodes(room, Content, process_room),
        process_nodes(freeday, Content, process_freeday).

xml_element(E, element(E,_,_)).

process_nodes(What, Content0, Goal) :-
        include(xml_element(What), Content0, Content),
        maplist(Goal, Content).

process_req(ClassId, element(req,Attr,_)) :-
        attr_values(Attr, [subject,teacher,amount], [Subject,Teacher,Amount]),
        assertz(req(ClassId,Subject,Teacher,Amount)).

process_coupling(ClassId, element(coupling,Attr,_)) :-
        attr_values(Attr, [subject,lesson1,lesson2], [Subject,Slot1,Slot2]),
        assertz(coupling(ClassId,Subject,Slot1,Slot2)).

process_free(ClassId, element(free,Attr,_)) :-
        attr_values(Attr, [slot], [Slot]),
        assertz(free_slot(ClassId,Slot)).


process_class(element(class,Attr,Content)) :-
        attr_values(Attr, [id], [Id]),
        process_nodes(req, Content, process_req(Id)),
        process_nodes(coupling, Content, process_coupling(Id)),
        process_nodes(free, Content, process_free(Id)).

process_globals(Content) :-
        memberchk(element(global,GlobAttr,_), Content),
        attr_values(GlobAttr, [numslots,slotsperday], [NumSlots,SlotsPerDay]),
        assertz(slots_per_day(SlotsPerDay)),
        assertz(num_slots(NumSlots)).


process_room(element(room,Attr,Content)) :-
        attr_values(Attr, [id], [Id]),
        process_nodes(allocate, Content, process_allocation(Id)).

process_allocation(RoomId, element(allocate,Attr,_)) :-
        attr_values(Attr, [class,subject,lesson], [Class,Subject,Lesson]),
        assertz(room_alloc(RoomId,Class,Subject,Lesson)).

process_freeday(element(freeday,Attr,_)) :-
        attr_values(Attr, [teacher,day], [Teacher,Day]),
        assertz(teacher_freeday(Teacher,Day)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run :-
        process_input('reqs.xml'),
        timetab_(Rs, Vs),
        labeling([ff], Vs),
        print_classes(Rs),
        nl, nl,
        print_teachers(Rs),
        nl.

%?- time(run).

:- initialization((run,halt)).
