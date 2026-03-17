/** <module> PML Core Loader
 *
 *  This script loads all components of the Polarized Modal Logic (PML) Core Framework
 *  in the correct order.
 */

% Suppress singleton variable warnings, often common in DSL definitions.
:- style_check(-singleton).

% =================================================================
% Load Order
% =================================================================

% 1. Utilities and Core Vocabulary
:- use_module(utils).
:- use_module(pml_operators).

% 2. Core Prover (must be loaded before axioms that extend it)
:- use_module(incompatibility_semantics).

% 3. Semantic Foundations (Axioms extending the prover)
:- use_module(semantic_axioms).

% 4. Pragmatic Foundations
% Automata must be loaded before Pragmatic Axioms that use them (e.g., Trace).
:- use_module(automata).
:- use_module(pragmatic_axioms).
:- use_module(intersubjective_praxis).

% 5. The Dialectical Engine and Critique
:- use_module(critique).
:- use_module(dialectical_engine).

% =================================================================
% Initialization
% =================================================================

:- initialization(writeln('PML Core Framework Loaded.')).
