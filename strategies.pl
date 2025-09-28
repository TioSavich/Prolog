/** <module> Standardized Strategy Loader
 *
 * This module serves as a centralized loader for all defined student
 * reasoning strategies. It imports all `sar_*.pl` (Student Addition/Subtraction
 * Reasoning) and `smr_*.pl` (Student Multiplication/Division Reasoning)
 * modules.
 *
 * By centralizing the loading process, we ensure that the full library of
 * strategies is available to the reorganization engine for analysis,
 * synthesis, and validation.
 *
 * @author Jules
 * @license MIT
 */

:- module(strategies, []).

% Addition and Subtraction Strategies
:- use_module(sar_add_chunking).
:- use_module(sar_add_cobo).
:- use_module(sar_add_rmb).
:- use_module(sar_add_rounding).
:- use_module(sar_sub_cbbo_take_away).
:- use_module(sar_sub_chunking_a).
:- use_module(sar_sub_chunking_b).
:- use_module(sar_sub_chunking_c).
:- use_module(sar_sub_cobo_missing_addend).
:- use_module(sar_sub_decomposition).
:- use_module(sar_sub_rounding).
:- use_module(sar_sub_sliding).

% Multiplication and Division Strategies
:- use_module(smr_div_cbo).
:- use_module(smr_div_dealing_by_ones).
:- use_module(smr_div_idp).
:- use_module(smr_div_ucr).
:- use_module(smr_mult_c2c).
:- use_module(smr_mult_cbo).
:- use_module(smr_mult_commutative_reasoning).
:- use_module(smr_mult_dr).