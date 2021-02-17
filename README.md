## Simsttab &mdash; A simple timetabling engine for schools

See

   [**https://www.metalevel.at/simsttab/**](https://www.metalevel.at/simsttab/)

for more information.

Sample invocation:

    $ scryer-prolog simsttab.pl reqs.pl

Sample query:


    ?- requirements_variables(Rs, Vs),
       labeling([ff], Vs),
       print_classes(Rs).

This constructs a [timetable](timetable.txt) that satisifies the
requirements stated in [`reqs.pl`](reqs.pl).
