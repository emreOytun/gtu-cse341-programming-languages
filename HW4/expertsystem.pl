place(engineering_bld).
place(library).
place(lecture_hall_a).
place(admin_office).
place(institute_x).
place(institute_y).
place(cafeteria).
place(social_sciences_bld).

edge(social_sciences_bld, library, 2).
edge(social_sciences_bld, institute_x, 8).
edge(library, institute_y, 3).
edge(library, engineering_bld, 5).
edge(engineering_bld, lecture_hall_a, 2).
edge(lecture_hall_a, institute_y, 3).
edge(admin_office, cafeteria, 4).
edge(admin_office, library, 1).
edge(admin_office, engineering_bld, 3).
edge(cafeteria, social_sciences_bld, 2).
edge(cafeteria, library, 5).

% Edges reverse
edge(institute_x, social_sciences_bld, 8).
edge(institute_y, library, 3).
edge(engineering_bld, library, 5).
edge(lecture_hall_a, engineering_bld, 2).
edge(institute_y, lecture_hall_a, 3).
edge(cafeteria, admin_office, 4).
edge(library, admin_office, 1).
edge(engineering_bld, admin_office, 3).
edge(social_sciences_bld, cafeteria, 2).
edge(library, cafeteria, 5).
edge(library, social_sciences_bld, 2).

deliveryPersonnel(p1, 10, [0, 4, 8], engineering_bld, none).
deliveryPersonnel(p2, 15, [20], library, obj1).
deliveryPersonnel(p3, 20, [4, 16], admin_office, none).

object(obj1, 2, library, institute_x, high, p2).
object(obj2, 3, engineering_bld, lecture_hall_a, medium, none).
object(obj3, 4, admin_office, cafeteria, low, none).
object(obj4, 5, social_sciences_bld, institute_y, medium, none).
object(obj5, 1, institute_x, social_sciences_bld, high, none).

% The below predicates are to traverse the map and find the shortest distance
% between given two places.
findRouteWithMaxBound(X, X, N, H) :- H >= 0, N is 0.
findRouteWithMaxBound(X, Y, N, H) :-
    H >= 0,
    edge(X, Z, N1),
    H1 is H - 1,
    findRouteWithMaxBound(Z, Y, N2, H1),
    N is N1 + N2.
    
findBoundaryByTryingBounds(X, Y, N, H) :-
    findRouteWithMaxBound(X, Y, N, H).
    
findBoundaryByTryingBounds(X, Y, N, H) :-
    H1 is H + 1,
    findBoundaryByTryingBounds(X, Y, N, H1).
    
traverseBound(X, Y, N, L) :-
    edge(X, Y, D),
    D < L,
    N is D.

traverseBound(X, Y, N, L) :-
    L >= 0,
    edge(X, Z, N1),
    L1 is L - N1,
    traverseBound(Z, Y, N2, L1),
    N is N1 + N2.

findMinTime(X, X, N) :-
    N is 0.

findMinTime(X, Y, N) :-
    place(X),
    place(Y),
    X \= Y,
    findBoundaryByTryingBounds(X, Y, N, 0),
    \+ traverseBound(X, Y, _, N),
    !.

findAvailablePersonnel(PersonnelId, Capacity, WorkHours, Location) :-
    deliveryPersonnel(PersonnelId, Capacity, WorkHours, Location, none).

findDeliveryPersonnel(ObjectId, PersonnelId, CurrentTime, TotalTime) :-
    object(ObjectId, Weight, Pickup, Dropoff, _, none),
    findAvailablePersonnel(PersonnelId, Capacity, WorkHours, PersonnelLocation),
    Weight =< Capacity,
    member(CurrentTime, WorkHours),
    findMinTime(PersonnelLocation, Pickup, Time1),
    findMinTime(Pickup, Dropoff, Time2),
    TotalTime is Time1 + Time2.

status(ObjectId, _) :-
    object(ObjectId, _, _, _, _, PersonelId),
    deliveryPersonnel(PersonelId, _, _, _, _),
    format('(PersonnelId, Time) List = ~w~n', [PersonelId]).
    
status(ObjectId, CurrentTime) :-
    findall((PersonnelId, Time), findDeliveryPersonnel(ObjectId, PersonnelId, CurrentTime, Time), PersonnelIdTimeList),
    length(PersonnelIdTimeList, Len),
    dif(Len, 0),
    format('(PersonnelId, Time) List = ~w~n', [PersonnelIdTimeList]).
    
% status(obj2, 4).
