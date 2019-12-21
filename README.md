## Simsttab &mdash; A simple timetabling engine for schools

See

   [**https://www.metalevel.at/simsttab/**](https://www.metalevel.at/simsttab/)

for more information.

Example query:


    ?- consult('reqs.pl'),
       requirements_variables(Rs, Vs),
       labeling([ff], Vs),
       print_classes(Rs).

This constructs a [timetable](timetable.txt) that satisifies the
requirements stated in [reqs.pl](reqs.pl).
