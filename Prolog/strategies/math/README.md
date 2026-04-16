# Prolog/math/ — Children's Arithmetic Strategy Automata

Formal models of children's arithmetic reasoning strategies, based on Carpenter
and Fennema's Cognitively Guided Instruction (CGI) research. Each file implements
one strategy as a finite state machine with execution trace.

## Epistemological status: reusable across epistemologies

These automata model **empirical findings** about children's mathematical behavior.
They are descriptive, not normatively committed to any particular epistemology.
Both System A (PML Core) and System B (Crisis Learning) can use them — System B
loads them through `oracle_server.pl` as strategies the learner can acquire.

## Strategy files

### Addition (SAR — Strategic Additive Reasoning)
| File | Strategy | Example |
|------|----------|---------|
| `sar_add_counting_on.pl` | Counting On | 8+5: start at 8, count 9,10,11,12,13 |
| `sar_add_cobo.pl` | Count On by Bases and Ones | 38+55: count on 5 tens (48,58,68,78,88) then 5 ones (89,...,93) |
| `sar_add_rmb.pl` | Rearranging to Make Bases | 8+5: move 2 from 5 to 8 to make 10, then 10+3=13 |
| `sar_add_chunking.pl` | Chunking | 8+5: 8+2=10, 10+3=13 |
| `sar_add_rounding.pl` | Rounding | 19+3: 20+3=23, 23-1=22 |

### Subtraction (SAR)
| File | Strategy | Example |
|------|----------|---------|
| `sar_sub_counting_back.pl` | Counting Back | 13-5: start at 13, count 12,11,10,9,8 |
| `sar_sub_cbbo_take_away.pl` | Count Back By Ones (Take Away) | 13-5: count back by tens then ones |
| `sar_sub_cobo_missing_addend.pl` | Count On (Missing Addend) | 13-8: count 9,10,11,12,13 → 5 |
| `sar_sub_chunking_a.pl` | Chunking A | 13-5: 13-3=10, 10-2=8 |
| `sar_sub_chunking_b.pl` | Chunking B | Variant chunking |
| `sar_sub_chunking_c.pl` | Chunking C | Variant chunking |
| `sar_sub_decomposition.pl` | Decomposition | 13-5: (10-5)+(3)=8 |
| `sar_sub_rounding.pl` | Rounding | 22-3: 22-2=20, 20-1=19 |
| `sar_sub_sliding.pl` | Sliding | 22-19: both slide by 1 → 3-0=3 |

### Multiplication (SMR — Single Multiplier Reasoning)
| File | Strategy | Example |
|------|----------|---------|
| `smr_mult_cbo.pl` | Count By Ones | 3×4: 4,8,12 (count groups) |
| `smr_mult_c2c.pl` | Count to Calculate | Use known facts to derive |
| `smr_mult_commutative_reasoning.pl` | Commutative Reasoning | 3×4 = 4×3 |
| `smr_mult_dr.pl` | Derived Reasoning | 6×7: 5×7=35, +7=42 |

### Division (SMR)
| File | Strategy | Example |
|------|----------|---------|
| `smr_div_cbo.pl` | Count By Ones | 12÷3: deal one at a time |
| `smr_div_dealing_by_ones.pl` | Dealing By Ones | Fair sharing model |
| `smr_div_idp.pl` | Inverse of Derived Product | 42÷7: what × 7 = 42? |
| `smr_div_ucr.pl` | Using Commutative Reasoning | 12÷3 via 3×?=12 |

### Fractions
| File | Strategy | Purpose |
|------|----------|---------|
| `jason_fsm.pl` | Jason's PFS/FCS (active) | FSM with traces, used by oracle_server.pl |
| `jason.pl` | Jason's PFS (grounded) | Uses recollection structures, depends on root modules |
| `counting2.pl` | Counting automaton variant | Extended counting with base decomposition |
| `counting_on_back.pl` | Bidirectional counting | Count forward and backward |
| `fraction_semantics.pl` | Equivalence rules | Grouping and composition rules for fractions |

## Interface

Each strategy module exports a `run_*/4` or `run_*/5` predicate that returns
a result and an execution trace. The oracle_server.pl maps operation types to
strategies via `list_available_strategies/2`.
