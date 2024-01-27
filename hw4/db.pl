% id capacity workHours currentDeliveryJobId currentLocation
delivery_personnel(1, 50, [8, 12, 16], none, cafeteria).
delivery_personnel(2, 40, [8, 12, 16], 1, cafeteria).
delivery_personnel(3, 30, [8, 12], none, cafeteria).

% id place
place(1, cafeteria).
place(2, library).
place(3, social_science_bld).
place(4, institute_x).
place(5, institute_y).
place(6, lecture_hall_a).
place(7, engineering_bld).
place(8, admin_office).

% id weight pickUpPlace dropPlace urgency deliveryPersonId
object(1, 25, cafeteria, library, medium, 2).
object(2, 15, library, social_science_bld, medium, none).
object(3, 40, cafeteria, institute_x, medium, none).
object(4, 35, cafeteria, institute_y, medium, none).
object(5, 30, cafeteria, lecture_hall_a, medium, none).

% beginPlaceId endPlaceId deliveryHours
way(1, 2, 5).
way(2, 1, 5).
way(1, 3, 2).
way(3, 1, 2).
way(1, 8, 4).
way(8, 1, 4).
way(2, 3, 2).
way(3, 2, 2).
way(3, 4, 8).
way(4, 3, 8).
way(8, 2, 1).
way(2, 8, 1).
way(7, 2, 5).
way(2, 7, 5).
way(7, 6, 2).
way(6, 7, 2).
way(2, 5, 3).
way(5, 2, 3).
way(6, 5, 3).
way(5, 6, 3).
way(7, 8, 3).
way(8, 7, 3).
way(1, 4, 10).
way(4, 1, 10).
way(1, 5, 7).
way(5, 1, 7).
way(1, 6, 9).
way(6, 1, 9).
way(1, 7, 7).
way(7, 1, 7).
way(2, 6, 6).
way(6, 2, 6).
way(2, 4, 10).
way(4, 2, 10).
way(3, 8, 3).
way(8, 3, 3).
way(3, 7, 7).
way(7, 3, 7).
way(3, 6, 8).
way(6, 3, 8).
way(3, 5, 5).
way(5, 3, 3).
way(4, 5, 13).
way(5, 4, 13).
way(4, 6, 16).
way(6, 4, 16).
way(4, 7, 14).
way(7, 4, 14).
way(4, 8, 11).
way(8, 4, 11).
way(5, 7, 5).
way(7, 5, 5).
way(5, 8, 4).
way(8, 5, 4).
way(6, 8, 5).
way(8, 6, 5).
way(1, 1, 0).
way(2, 2, 0).
way(3, 3, 0).
way(4, 4, 0).
way(5, 5, 0).
way(6, 6, 0).
way(7, 7, 0).
way(8, 8, 0).

% Rule: Delivery status of a specific object
status_object(ObjectID) :-
    object(ObjectID, _, _, _, _, InTransit),
    (InTransit \= none ->
        write('Object is on delivery. Delivery personnel: '), write(InTransit), nl
    ;
        write('Object is not on delivery. '),
        available_delivery_personnel(ObjectID)
    ).

% Rule: Finding suitable delivery personnel for a specific object
available_delivery_personnel(ObjectID) :-
    object(ObjectID, Weight, Pick, Drop, _, none),
    place(PickId, Pick),
    place(DropId, Drop),
    delivery_personnel(ID, Capacity, _, none, Location),
    place(LocationId, Location),
    way(LocationId, PickId, TempHours),
    way(PickId, DropId, WayHours),
    add(TempHours, WayHours, Hours),
    Capacity >= Weight,
    total_work_hours(ID, TotalHours),
    TotalHours >= Hours,
    nl, write('ID: '), write(ID), write('   Hours: '), write(Hours), nl.

% Rule: Total working hours of a particular staff member
total_work_hours(PersonID, TotalHours) :-
    delivery_personnel(PersonID, _, WorkHours, _, _),
    sum_work_hours(WorkHours, TotalHours).

% Rule: I did it because each element in the list represents 4 hours.
sum_work_hours(List, MultipliedResult) :-
    length(List, Length),
    MultipliedResult is Length * 4.

% Rule: It adds up the time it takes for the delivery personnel to travel to the delivery area and the time it takes to transport the delivery after receiving it.
add(A, B, Result) :-
    Result is A + B.
