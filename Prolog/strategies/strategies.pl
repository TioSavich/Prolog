/** <module> Standardized Strategy Loader
 *
 * This module serves as a documentation index for all defined student
 * reasoning strategies. It no longer imports modules to avoid namespace
 * conflicts between FSM strategy predicates.
 *
 * Individual modules should be loaded directly when needed using
 * module-qualified calls like: sar_add_chunking:run_chunking/4
 *
 * Available strategies:
 * - Addition: sar_add_chunking, sar_add_cobo, sar_add_rmb, sar_add_rounding
 * - Subtraction: sar_sub_cbbo_take_away, sar_sub_chunking_a/b/c, 
 *                sar_sub_cobo_missing_addend, sar_sub_decomposition,
 *                sar_sub_rounding, sar_sub_sliding
 * - Multiplication: smr_mult_c2c, smr_mult_cbo, smr_mult_commutative_reasoning,
 *                   smr_mult_dr
 * - Division: smr_div_cbo, smr_div_dealing_by_ones, smr_div_idp, smr_div_ucr
 *
 * @author Jules
 */

:- module(strategies, []).

% This module intentionally exports nothing and imports nothing
% to avoid namespace conflicts between strategy modules.