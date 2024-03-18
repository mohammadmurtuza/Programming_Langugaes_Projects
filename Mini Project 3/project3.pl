flight(6711, bos, ord, 0815, 1005).
flight(211, lga, ord, 0700, 0830).
flight(203, lga, lax, 0730, 1335).
flight(92221, ewr, ord, 0800, 0920).
flight(2134, ord, sfo, 0930, 1345).
flight(954, phx, dfw, 1655, 1800).
flight(1176, sfo, lax, 1430, 1545).
flight(205, lax, lga, 1630, 2210).

destination_from_phx(Destination) :- flight(_, phx, Destination, _, _).
flight_to_phx(FlightNumber) :- flight(FlightNumber, _, phx, _, _).
landing_time_from_bos(LandingTime) :- flight(_, bos, _, _, LandingTime).
depart_after_arrival(DepartureTime) :-
    flight(_, ewr, ord, _, ArrivalTime),
    flight(_, ord, sfo, DepartureTime, _),
    DepartureTime > ArrivalTime.
arrival_times_to_ord(ArrivalTime) :- flight(_, _, ord, _, ArrivalTime).

% Convert time to minutes since midnight for comparison
time_to_minutes(HHMM, Minutes) :-
    Hours is HHMM // 100,
    Mins is HHMM mod 100,
    Minutes is Hours * 60 + Mins.

% Check if there is enough layover time between flights
sufficient_layover(ArrivalTime, DepartureTime) :-
    time_to_minutes(ArrivalTime, ArrivalMinutes),
    time_to_minutes(DepartureTime, DepartureMinutes),
    Layover is DepartureMinutes - ArrivalMinutes,
    Layover >= 30.

% Direct flight from LGA to LAX
direct_route(lga, lax, FlightNumber, DepartureTime, ArrivalTime) :-
    flight(FlightNumber, lga, lax, DepartureTime, ArrivalTime).

% One-stop connection route rule, now with layover consideration
connection_route(lga, lax, FlightNumber1, FlightNumber2, DepartureTime1, ArrivalTime2) :-
    flight(FlightNumber1, lga, TransferPoint, DepartureTime1, ArrivalTime1),
    flight(FlightNumber2, TransferPoint, lax, DepartureTime2, ArrivalTime2),
    sufficient_layover(ArrivalTime1, DepartureTime2).

% All routes from LGA to LAX including direct flights and one-stop connections
route_lga_to_lax(Route) :-
    (   direct_route(lga, lax, FlightNumber, DepartureTime, ArrivalTime),
        Route = direct(FlightNumber, DepartureTime, ArrivalTime)
    ;   connection_route(lga, lax, FlightNumber1, FlightNumber2, DepartureTime1, ArrivalTime2),
        Route = connection(FlightNumber1, FlightNumber2, DepartureTime1, ArrivalTime2)
    ).




