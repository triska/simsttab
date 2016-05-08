/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Simsttab -- Simplistic school time tabler
  Copyright (C) 2005, 2014, 2016 Markus Triska triska@metalevel.at

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
:- use_module(library(xpath)).

:- dynamic req/4, coupling/4, teacher_freeday/2, slots_per_day/1,
	   num_slots/1, class_freeslot/2, room_alloc/4.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 Posting constraints

   The most important data structure in this CSP are pairs of the form

      Req-Vs

   where Req is a term of the form req(C,S,T,N) (see below), and Vs is
   a list of length N. The elements of Vs are finite domain variables
   that denote the *time slots* of the scheduled lessons of Req. We
   call this list of Req-Vs pairs the requirements.

   To break symmetry, the elements of Vs are constrained to be
   strictly ascending (it follows that they are all_different/1).

   Further, the time slots of each teacher are constrained to be
   all_different/1.

   For each requirement, the time slots divided by slots_per_day are
   constrained to be strictly ascending to enforce distinct days,
   except for coupled lessons.

   The time slots of each class, and of lessons occupying the same
   room, are constrained to be all_different/1.

   Labeling is performed on all slot variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


requirements(Rs) :-
        setof(req(Class,Sub,Teacher,Num), req(Class,Sub,Teacher,Num), Rs0),
        maplist(req_with_slots, Rs0, Rs).

req_with_slots(R, R-Slots) :- R = req(_,_,_,N), length(Slots, N).

classes(Classes) :- setof(C, S^N^T^req(C,S,T,N), Classes).

teachers(Teachers) :- setof(T, C^S^N^req(C,S,T,N), Teachers).

rooms(Rooms) :-
        findall(Room, room_alloc(Room,_C,_S,_Slot), Rooms0),
        sort(Rooms0, Rooms).

requirements_variables(Rs, Vars) :-
        requirements(Rs),
        pairs_slots(Rs, Vars),
        num_slots(NumSlots0),
        NumSlots #= NumSlots0 - 1,
        Vars ins 0..NumSlots,
        maplist(constrain_subject, Rs),
        classes(Classes),
        teachers(Teachers),
        rooms(Rooms),
        maplist(constrain_teacher(Rs), Teachers),
        maplist(constrain_class(Rs), Classes),
        maplist(constrain_room(Rs), Rooms).

slot_quotient(S, Q) :-
        slots_per_day(SPD),
        Q #= S // SPD.


list_without_nths(Es0, Ws, Es) :-
        phrase(without_(Ws, 0, Es0), Es).

without_([], _, Es) --> list(Es).
without_([W|Ws], Pos0, [E|Es]) -->
        { Pos #= Pos0 + 1 },
        (   { W #= Pos0 } -> without_(Ws, Pos, Es)
        ;   [E],
            without_([W|Ws], Pos, Es)
        ).


%:- list_without_nths([a,b,c,d], [3], [a,b,c]).
%:- list_without_nths([a,b,c,d], [1,2], [a,d]).

slots_couplings(Slots, F-S) :-
        nth0(F, Slots, S1),
        nth0(S, Slots, S2),
        S2 #= S1 + 1.

constrain_subject(req(Class,Subj,_Teacher,_Num)-Slots) :-
        strictly_ascending(Slots), % break symmetry
        maplist(slot_quotient, Slots, Qs0),
        findall(F-S, coupling(Class,Subj,F,S), Cs),
        maplist(slots_couplings(Slots), Cs),
        pairs_values(Cs, Seconds0),
        sort(Seconds0, Seconds),
        list_without_nths(Qs0, Seconds, Qs),
        strictly_ascending(Qs).


all_diff_from(Vs, F) :- maplist(#\=(F), Vs).

constrain_class(Rs, Class) :-
        include(class_req(Class), Rs, Sub),
        pairs_slots(Sub, Vs),
        all_different(Vs),
        findall(S, class_freeslot(Class,S), Frees),
        maplist(all_diff_from(Vs), Frees).


constrain_teacher(Rs, Teacher) :-
        include(teacher_req(Teacher), Rs, Sub),
        pairs_slots(Sub, Vs),
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

list([])     --> [].
list([E|Es]) --> [E], list(Es).

class_req(C, req(C,_S,_T,_N)-_).

teacher_req(T, req(_C,_S,T,_N)-_).

pairs_slots(Ps, Vs) :-
        pairs_values(Ps, Vs0),
        append(Vs0, Vs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			       Printing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Teachers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_teachers(Rs) :-
        teachers(Ts),
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
        classes(Cs),
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
        (   0 #= N0 mod SPD ->
            Day #= N0 // SPD,
            format("\n\nDay ~w:  ", [Day])
        ;   true
        ),
        (   call(Goal, Rs, N0, S) -> true
        ;   S = free
        ),
        format("~w   ",[S]),
        N #= N0 + 1.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parse XML file.

   This part of the program translates the XML file to a list of
   Prolog terms that describe the requirements. library(xpath) is used
   to access nodes of the XML file. A DCG describes the list of Prolog
   terms. The most important of them are:

   *) req(C,S,T,N)
        Class C is to be taught subject S by teacher T; N times a week
        (on different days)

   *) coupling(C,S,J,K)
        In class C, subject S contains a coupling: The J-th lesson of S
        directly precedes the K-th lesson, on the same day.

   *) teacher_freeday(T, D)
        Teacher T must not have any lessons scheduled on day D.

   These terms can all be dynamically asserted to make the
   requirements globally accessible.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Extract option values from tag attributes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

attrs_values(Node, As, Vs) :-
        must_be(list(atom), As),
        maplist(attr_value(Node), As, Vs).

attr_value(Node, Attr, Value) :-
        (   xpath(Node, /self(@Attr), Value0) -> true
        ;   throw('attribute expected'-Node-Attr)
        ),
        (   numeric_attribute(Attr) -> atom_number(Value0, Value)
        ;   Value = Value0
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
   A DCG relates the XML file to a list of Prolog terms.

   Example query:

      ?- phrase(requirements('reqs.xml'), Rs).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

requirements(File) -->
        { load_xml_file(File, AST),
          xpath_chk(AST, //requirements, R) },
        globals(R),
        process_nodes(class, R, process_class),
        process_nodes(room, R, process_room),
        process_nodes(freeday, R, process_freeday).

process_nodes(What, R, Goal) -->
        { findall(Element, xpath(R, //What, Element), Elements) },
        elements_(Elements, Goal).

elements_([], _) --> [].
elements_([E|Es], Goal) -->
        call(Goal, E),
        elements_(Es, Goal).

process_req(ClassId, Node) -->
        { attrs_values(Node, [subject,teacher,amount], [Subject,Teacher,Amount]) },
        [req(ClassId,Subject,Teacher,Amount)].

process_coupling(ClassId, Node) -->
        { attrs_values(Node, [subject,lesson1,lesson2], [Subject,Slot1,Slot2]) },
        [coupling(ClassId,Subject,Slot1,Slot2)].

process_free(ClassId, Node) -->
        { attrs_values(Node, [slot], [Slot]) },
        [class_freeslot(ClassId,Slot)].


process_class(Node) -->
        { attrs_values(Node, [id], [Id]) },
        process_nodes(req, Node, process_req(Id)),
        process_nodes(coupling, Node, process_coupling(Id)),
        process_nodes(free, Node, process_free(Id)).

globals(Content) -->
        { xpath_chk(Content, //global, Global),
          attrs_values(Global, [numslots,slotsperday], [NumSlots,SlotsPerDay]) },
        [slots_per_day(SlotsPerDay),num_slots(NumSlots)].


process_room(Node) -->
        { attrs_values(Node, [id], [Id]) },
        process_nodes(allocate, Node, process_allocation(Id)).

process_allocation(RoomId, Node) -->
        { attrs_values(Node, [class,subject,lesson], [Class,Subject,Lesson]) },
        [room_alloc(RoomId,Class,Subject,Lesson)].

process_freeday(Node) -->
        { attrs_values(Node, [teacher,day], [Teacher,Day]) },
        [teacher_freeday(Teacher,Day)].


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Execution entry point.

   The parsed requirements are asserted to make them easily accessible
   as Prolog facts. On cleanup, all asserted facts are retracted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Core relation: timetable_(+File, -Rs, -Vs)
   The first argument is the XML file containing the requirements. Rs are
   the parsed requirements, and Vs is a list of finite domain variables
   that need to be labeled. An artificial choice point is introduced
   to retain the asserted facts until backtracking or commit.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

timetable_(File, Rs, Vs) :-
        phrase(requirements(File), Reqs),
        setup_call_cleanup(maplist(assertz, Reqs),
                           (   requirements_variables(Rs, Vs)
                           ;   true % retain the facts until cleanup
                           ),
                           maplist(retract, Reqs)).

timetable(File) :-
        timetable_(File, Rs, Vs),
        labeling([ff], Vs),
        print_classes(Rs),
        nl, nl,
        print_teachers(Rs),
        nl.


run :- timetable('reqs.xml').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ?- time(run).

   ?- timetable_('reqs.xml', Rs, Vs).

   ?- timetable_('reqs.xml', Rs, Vs), labeling([ff], Vs).
   Rs = [req('1a', anj, anj1, 3)-[3, 9, 16], req('1a', atvz, atvz1, 3)-[4, 10, 25], req('1a', bio, bio1, 2)-[11, 26], req('1a', fiz, fiz1, 2)-[13, 27], req('1a', geo, geo1, 2)-[17, 32], req('1a', kem, kem1, 2)-[18, 33], req('1a', mat, mat1, 5)-[2|...], req(..., ..., ..., ...)-[...|...], ... - ...|...],
   Vs = [3, 9, 16, 4, 10, 25, 11, 26, 13|...] .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
