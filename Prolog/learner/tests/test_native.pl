:- use_module('../execution_handler').
:- use_module('../fsm_synthesis_engine').
:- use_module('../more_machine_learner').
:- use_module('../oracle_server').

test :-
    fsm_synthesis_engine:assert_oracle_backed_strategy(add, 'COBO', 'interp'),
    (   more_machine_learner:run_learned_strategy(s(s(0)), s(0), R, Name, Trace)
    ->  format('Success! R=~w, Name=~w, Trace=~w~n', [R, Name, Trace])
    ;   writeln('Failed!')
    ),
    halt.
