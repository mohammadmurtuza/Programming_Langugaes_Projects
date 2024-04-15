flight(6711, bos, ord, 0815, 1005).
flight(211, lga, ord, 0700, 0830).
flight(203, lga, lax, 0730, 1335).
flight(92221, ewr, ord, 0800, 0920).
flight(2134, ord, sfo, 0930, 1345).
flight(954, phx, dfw, 1655, 1800).
flight(1176, sfo, lax, 1430, 1545).
flight(205, lax, lga, 1630, 2210).
flight(111, lga, bos, 0645, 0745).
flight(222, bos, ewr, 0750, 0845).

% Where does the flight from PHX go?
find_destinations_from_phx :-
    flight(_, phx, Destination, _, _),
    write('The flight from Phx goes to: '), write(Destination), nl,
    fail.
find_destinations_from_phx.

% Is there a flight to PHX?
check_flight_to_phx :-
    (   flight(_, _, phx, _, _)).
check_flight_to_phx.

% What time is does the flight from BOS land?
landing_details_from_bos :-
    flight(FlightNumber, bos, Destination, _, ArrivalTime),
    format('Flight ~w from BOS lands in ~w at: ~w', [FlightNumber, Destination, ArrivalTime]), nl,
    fail.
landing_details_from_bos.

% Does the flight from ORD to SFO depart after the flight from EWR to ORD lands?
ord_sfo_after_ewr_ord :-
    flight(EwrOrdFlight, ewr, ord, _, EwrOrdArrivalTime),
    flight(OrdSfoFlight, ord, sfo, OrdSfoDepartureTime, _),
    EwrOrdArrivalTime < OrdSfoDepartureTime,
    format('Yes, Flight ~w from ORD to SFO departs after Flight ~w from EWR to ORD lands.', [OrdSfoFlight, EwrOrdFlight]).
ord_sfo_after_ewr_ord.

% What time do the flights to ORD arrive?
arrival_details_to_ord :-
    flight(FlightNumber, DepartureAirport, ord, _, ArrivalTime),
    format('Flight ~w from ~w arrives at ord at: ~w', [FlightNumber, DepartureAirport, ArrivalTime]), nl,
    fail.
arrival_details_to_ord.


% Predicate to check for valid connection based on time
valid_connection(ArrivalTime1, DepartureTime2) :-
    % Convert HHMM format to minutes
    Hours1 is ArrivalTime1 // 100, Minutes1 is ArrivalTime1 mod 100,
    ArrivalMinutes1 is Hours1 * 60 + Minutes1,
    Hours2 is DepartureTime2 // 100, Minutes2 is DepartureTime2 mod 100,
    DepartureMinutes2 is Hours2 * 60 + Minutes2,
    % Check if there is enough time for the connection
    DepartureMinutes2 > ArrivalMinutes1.
:- style_check(-singleton).
% Direct flight from LGA to LAX
direct_flight_lga_lax :-
    flight(Number, lga, lax, Departure, Arrival),
    format('Direct flight: ~w from LGA to LAX, departing at ~w, arriving at ~w~n', [Number, Departure, Arrival]).


% Rule to find valid routes between two airports
route(Start, End, Route) :-
    route(Start, End, [Start], Route).

route(Start, End, Visited, Route) :-
    flight(FlightNumber, Start, Next, _, ArrivalTime),
    \+ member(Next, Visited),  % Ensure the next airport has not been visited yet
    flight(FlightNumberNext, Next, _, DepartureTime, _),
    ArrivalTime < DepartureTime, % Ensure arrival is before departure
    valid_connection(ArrivalTime, DepartureTime), % Check for valid connection time
    (Next = End ; route(Next, End, [Next|Visited], RouteRest)),
    Route = [[FlightNumber, Start, Next] | RouteRest].

