:- module(move_grammar, [
    classify_prompt/2,
    assessing_template/3,
    advancing_template/3,
    select_move/3
]).

:- use_module(library(strings)).
:- use_module(library(pcre)).

% ═══════════════════════════════════════════════════════════════
% Hermeneutic calculator — move grammar
% ═══════════════════════════════════════════════════════════════
%
% Two-tier classification of student prompts, mapped to the
% Hackenberg et al. (2024) decentering-move taxonomy:
%
%   PS   = Perturbing Student (create productive dissonance)
%   LST  = Leveraging Student's Thinking (build on what's there)
%   FMST = Follow-up to Make sense of Student's Thinking (probe)
%   AQST = Advancing Question about Student's Thinking (push forward)
%
% Amy's CGI instinct: when a student offers a computation without a
% strategy, the teacher's right move is NOT to hand them the answer.
% It's to elicit the strategy first (FMST). Only then can the teacher
% decide what to advance (LST/AQST).
%
% This module classifies the prompt into a `prompt_class/1` form and
% selects a move template. The template is a short English fragment
% that the LLM renders into a natural question.

% ── Classifier ──

% classify_prompt(+Text, -Class)
%   Class is one of:
%     arithmetic_computation(Op, Operands, strategy_unspoken)
%     arithmetic_computation(Op, Operands, strategy_stated(What))
%     vocabulary_question(Term)
%     strategy_report(Description)
%     unclear
classify_prompt(Text, Class) :-
    to_lower_string(Text, Lower),
    ( arithmetic_match(Lower, Op, Operands)
    -> ( strategy_mentioned(Lower, What)
       -> Class = arithmetic_computation(Op, Operands, strategy_stated(What))
       ;  Class = arithmetic_computation(Op, Operands, strategy_unspoken)
       )
    ; vocabulary_question_match(Lower, Term)
    -> Class = vocabulary_question(Term)
    ; strategy_report_match(Lower)
    -> Class = strategy_report(Text)
    ; strategy_mentioned(Lower, What)
    -> Class = strategy_declaration(What, Text)
    ;  Class = unclear
    ).

to_lower_string(X, L) :-
    ( string(X) -> S = X
    ; atom(X) -> atom_string(X, S)
    ; S = X
    ),
    string_lower(S, L).

% ── Arithmetic recognition ──
%
% We only claim `arithmetic_computation` when the text contains a
% recognisable computation form: two+ operands separated by + / - / x / ×
% with an optional trailing "=" or "= ___". This is deliberately loose
% tonight; the tokenisation is regex-based.
arithmetic_match(Lower, Op, Operands) :-
    re_matchsub("(\\d+)\\s*([+\\-x×*])\\s*(\\d+)\\s*=?", Lower, Dict, []),
    atom_to_term(Dict.1, A, _),
    atom_to_term(Dict.3, B, _),
    op_symbol(Dict.2, Op),
    Operands = [A, B].

op_symbol("+", addition).
op_symbol("-", subtraction).
op_symbol("x", multiplication).
op_symbol("×", multiplication).
op_symbol("*", multiplication).

% ── Strategy-mention recognition (CGI-aligned) ──
%
% Carpenter/Fennema CGI strategy lexicon plus common kid-talk phrasings
% a pre-service teacher will plausibly report. Ordered so more-specific
% matches fire first (counting_on_from_larger before counting_on;
% near_doubles before doubles). The cuts prevent double-classification.
strategy_mentioned(Lower, automatic_recall) :-
    ( sub_string(Lower, _, _, _, "just knew")
    ; sub_string(Lower, _, _, _, "just know")
    ; sub_string(Lower, _, _, _, "memorized")
    ; sub_string(Lower, _, _, _, "memorised")
    ; sub_string(Lower, _, _, _, "instant")
    ), !.
strategy_mentioned(Lower, counting_all) :-
    ( sub_string(Lower, _, _, _, "counted all")
    ; sub_string(Lower, _, _, _, "counting all")
    ; sub_string(Lower, _, _, _, "count all")
    ; sub_string(Lower, _, _, _, "from one")
    ; sub_string(Lower, _, _, _, "from 1")
    ; sub_string(Lower, _, _, _, "counted each")
    ), !.
strategy_mentioned(Lower, counting_on_from_larger) :-
    ( sub_string(Lower, _, _, _, "from the bigger")
    ; sub_string(Lower, _, _, _, "from the larger")
    ; sub_string(Lower, _, _, _, "start with the biggest")
    ; sub_string(Lower, _, _, _, "started with the bigger")
    ; sub_string(Lower, _, _, _, "bigger first")
    ), !.
strategy_mentioned(Lower, counting_on) :-
    ( sub_string(Lower, _, _, _, "count on")
    ; sub_string(Lower, _, _, _, "counted on")
    ; sub_string(Lower, _, _, _, "counting on")
    ; sub_string(Lower, _, _, _, "counted up")
    ; sub_string(Lower, _, _, _, "count up")
    ; sub_string(Lower, _, _, _, "went up")
    ), !.
strategy_mentioned(Lower, making_ten) :-
    ( sub_string(Lower, _, _, _, "making ten")
    ; sub_string(Lower, _, _, _, "make ten")
    ; sub_string(Lower, _, _, _, "makes ten")
    ; sub_string(Lower, _, _, _, "made a ten")
    ; sub_string(Lower, _, _, _, "made ten")
    ; sub_string(Lower, _, _, _, "making a ten")
    ; sub_string(Lower, _, _, _, "take from")
    ; sub_string(Lower, _, _, _, "gave 2 to")
    ; sub_string(Lower, _, _, _, "moved 2 to")
    ; sub_string(Lower, _, _, _, "go to ten first")
    ), !.
strategy_mentioned(Lower, near_doubles) :-
    ( sub_string(Lower, _, _, _, "near double")
    ; sub_string(Lower, _, _, _, "doubles plus")
    ; sub_string(Lower, _, _, _, "double plus")
    ; sub_string(Lower, _, _, _, "one more than double")
    ), !.
strategy_mentioned(Lower, doubles) :-
    ( sub_string(Lower, _, _, _, "double")
    ; sub_string(Lower, _, _, _, "doubled")
    ; sub_string(Lower, _, _, _, "doubles")
    ), !.
strategy_mentioned(Lower, decomposition) :-
    ( sub_string(Lower, _, _, _, "decompose")
    ; sub_string(Lower, _, _, _, "break apart")
    ; sub_string(Lower, _, _, _, "broke apart")
    ; sub_string(Lower, _, _, _, "split")
    ; sub_string(Lower, _, _, _, "tens and ones")
    ; sub_string(Lower, _, _, _, "place value")
    ), !.
strategy_mentioned(Lower, compensation) :-
    ( sub_string(Lower, _, _, _, "added one to make")
    ; sub_string(Lower, _, _, _, "took one")
    ; sub_string(Lower, _, _, _, "subtracted one and")
    ; sub_string(Lower, _, _, _, "gave it back")
    ), !.

% ── Vocabulary-question recognition ──
%
% "What is a quantity?" / "Define measurement." / "Tell me about counting."
vocabulary_question_match(Lower, Term) :-
    ( re_matchsub("what (is|are) (a |an |the )?([a-z0-9_ ]+?)( in [a-z0-9_ ]+)?[\\?\\.]?$", Lower, Dict, [])
    ; re_matchsub("define ([a-z0-9_ ]+?)( in [a-z0-9_ ]+)?[\\?\\.]?$", Lower, Dict, [])
    ; re_matchsub("tell me about ([a-z0-9_ ]+?)( in [a-z0-9_ ]+)?[\\?\\.]?$", Lower, Dict, [])
    ),
    Term = Dict.3.

% ── Student-work-report recognition ──
%
% "A student said ..." / "Jasmine wrote ..." / "I got 13 because ..." —
% the prompt is reporting student reasoning, not asking a question.
strategy_report_match(Lower) :-
    ( sub_string(Lower, _, _, _, "a student")
    ; sub_string(Lower, _, _, _, "my student")
    ; sub_string(Lower, _, _, _, "the student")
    ; sub_string(Lower, _, _, _, " said ")
    ; sub_string(Lower, _, _, _, " says ")
    ; sub_string(Lower, _, _, _, " wrote ")
    ; sub_string(Lower, _, _, _, " writes ")
    ), !.

% ═══════════════════════════════════════════════════════════════
% Move templates
% ═══════════════════════════════════════════════════════════════
%
% Each template is a short English sentence that the LLM will render
% into a natural question in Amy's voice. The template carries:
%   - a short Move tag (PS/LST/FMST/AQST)
%   - a Rationale sentence for the teacher
%   - a question-body that can include {{slot}} placeholders filled by
%     the classifier.
%
% The LLM's job is to re-voice the body in Amy's register (warm, probing,
% no "because it's easier," no "correct" answer given away).

% assessing_template(+Class, -MoveTag, -Template)
%   Assessing = FMST (figure out what the student is thinking).
%   Used when the prompt leaves the student's reasoning unknown.
assessing_template(
    arithmetic_computation(_, Operands, strategy_unspoken),
    'FMST',
    template(
        "Assessing question, FMST style, no answer given. Ask the student how they were thinking about {{op}} of {{operands}}: counting on, making ten, doubles, or something else. Be warm, curious, concrete. One or two sentences, no '?' emoji.",
        [operands=OperandsStr, op="this computation"]
    )
) :- operands_str(Operands, OperandsStr).

assessing_template(
    strategy_report(_Text),
    'FMST',
    template(
        "Assessing question, FMST style. The teacher is reporting a student's work. Ask the teacher what they notice about the student's reasoning and what they want to know next. Invite them to share their own reading of the student's strategy. One or two sentences.",
        []
    )
).

assessing_template(
    unclear,
    'FMST',
    template(
        "Assessing question, FMST style. The student's utterance is hard to parse. Ask a gentle clarifying question that lets them rephrase or offer an example. Do not guess their meaning.",
        []
    )
).

% advancing_template(+Class, -MoveTag, -Template)
%   Advancing = LST / AQST (extend a strategy or push to generalise).
%   Used when the student's thinking IS visible and the teacher can build.
advancing_template(
    arithmetic_computation(_, _, strategy_stated(What)),
    'LST',
    template(
        "Advancing question, LST style, building on the {{strategy}} strategy. Ask the student to try the same strategy on a slightly different computation, or to explain to another student why their approach works. Keep it inside the strategy they chose; don't hand them a different one. One or two sentences.",
        [strategy=WhatStr]
    )
) :- atom_string(What, WhatStr).

advancing_template(
    strategy_declaration(What, _),
    'LST',
    template(
        "Advancing question, LST style. The student reported using the {{strategy}} strategy. Affirm you heard them, then ask them to walk through the steps once more or try it on a slightly different problem. Stay in their strategy. One or two sentences.",
        [strategy=WhatStr]
    )
) :- atom_string(What, WhatStr).

advancing_template(
    vocabulary_question(Term),
    'AQST',
    template(
        "Advancing question, AQST style, after a vocabulary question. Give Amy's definition clearly, then ask the student to try applying it to an example they choose themselves. Do not provide or list examples; let the student generate the example. Do not use the phrase 'for example'. Two or three sentences.",
        [term=Term]
    )
).

% ── select_move(+Class, -MoveTag, -Template) ──
%
% One-stop selector. Tries assessing first (the default Amy move), then
% advancing (only when the classifier already sees student thinking).
select_move(Class, MoveTag, Template) :-
    assessing_template(Class, MoveTag, Template), !.
select_move(Class, MoveTag, Template) :-
    advancing_template(Class, MoveTag, Template), !.

% ── helpers ──

operands_str([A, B], S) :- format(string(S), "~w and ~w", [A, B]).
operands_str(Ops, S) :- atomic_list_concat(Ops, ", ", S).
