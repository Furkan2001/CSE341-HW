% Classify function
classify(SepalLength, SepalWidth, PetalLength, PetalWidth) :-
    (   PetalLength =< 2.45
    ->  write('Class: '), write('Iris-setosa')
    ;   PetalLength > 2.45,
        (   PetalWidth =< 1.75
        ->  (   PetalLength =< 4.95
            ->  (   PetalWidth =< 1.65
                ->  write('Class: '), write('Iris-versicolor')
                ;   PetalWidth > 1.65,
                 write('Class: '), write('Iris-virginica')
                )
            ;   PetalLength > 4.95,
                (   PetalWidth =< 1.55
                ->  write('Class: '), write('Iris-virginica')
                ;   PetalWidth > 1.55,
                    (   SepalLength =< 6.95
                    ->  write('Class: '), write('Iris-versicolor')
                    ;   SepalLength > 6.95,
                        write('Class: '), write('Iris-virginica')
                    )
                )
            )
        ;   PetalWidth > 1.75,
            (   PetalLength =< 4.85
            ->  (   SepalWidth =< 3.1
                ->  write('Class: '), write('Iris-virginica')
                ;   SepalWidth > 3.10,
                    write('Class: '), write('Iris-versicolor')
                )
            ;   PetalLength > 4.85,
                write('Class: '), write('Iris-virginica')
            )
        )
    ).
