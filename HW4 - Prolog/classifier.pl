classify(_, _, PetalLength, _) :-
    PetalLength =< 2.45,
    format('yes~nIris-setosa~n').

classify(_, _, PetalLength, PetalWidth) :-
    PetalLength > 2.45,
    PetalLength =< 4.75,
    PetalWidth =< 1.65,
    format('yes~nIris-versicolor~n').

classify(_, _, PetalLength, PetalWidth) :-
    PetalLength > 2.45,
    PetalLength =< 4.75,
    PetalWidth > 1.65,
    format('yes~nIris-virginica~n').

classify(_, _, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth =< 1.75,
    PetalLength =< 4.95,
    format('yes~nIris-versicolor~n').

classify(_, _, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth =< 1.75,
    PetalLength > 4.95,
    PetalWidth =< 1.55,
    format('yes~nIris-virginica~n').

classify(SepalLength, _, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth =< 1.75,
    PetalLength > 4.95,
    PetalWidth > 1.55,
    SepalLength =< 6.95,
    format('yes~nIris-versicolor~n').

classify(SepalLength, _, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth =< 1.75,
    PetalLength > 4.95,
    PetalWidth > 1.55,
    SepalLength > 6.95,
    format('yes~nIris-virginica~n').

classify(_, SepalWidth, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth > 1.75,
    PetalLength =< 4.85,
    SepalWidth =< 3.10,
    format('yes~nIris-virginica~n').

classify(_, SepalWidth, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth > 1.75,
    PetalLength =< 4.85,
    SepalWidth > 3.10,
    format('yes~nIris-versicolor~n').

classify(_, _, PetalLength, PetalWidth) :-
    PetalLength > 4.75,
    PetalWidth > 1.75,
    PetalLength > 4.85,
    format('yes~nIris-virginica~n').

% classify(5.1, 3.5, 1.4, 0.2).

