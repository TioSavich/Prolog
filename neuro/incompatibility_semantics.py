# -*- coding: utf-8 -*-
"""
This script is a Python conversion of the Prolog files 'incompatibility_semantics.pl'
and 'test_synthesis.pl'. It implements a logic engine based on incompatibility
semantics and provides a comprehensive test suite using Python's unittest framework.
"""

import math
import unittest
from fractions import Fraction
from itertools import product
from copy import deepcopy

# =================================================================
# Part 0: Term Representation (Python equivalent of Prolog terms)
# =================================================================

class Term:
    """Base class for all logical terms."""
    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.name == other.name and self.args == other.args
    def __hash__(self):
        return hash((self.__class__.__name__, self.name, tuple(self.args)))
    def __repr__(self):
        if not self.args:
            return str(self.name)
        return f"{self.name}({', '.join(map(repr, self.args))})"

class Var(Term):
    """Represents a variable in a logical expression."""
    def __init__(self, name):
        self.name = name
        self.args = []
    def __hash__(self):
        return hash((self.__class__.__name__, self.name))

class Atom(Term):
    """Represents an atomic value or constant."""
    def __init__(self, name):
        self.name = name
        self.args = []

class Predicate(Term):
    """Represents a predicate with a name and arguments."""
    def __init__(self, name, args=None):
        self.name = name
        self.args = args if args is not None else []
    
    def __call__(self, *args):
        return Predicate(self.name, list(args))

class Sequent:
    """Represents a sequent P => C (Premises => Conclusions)."""
    def __init__(self, premises, conclusions):
        self.premises = premises
        self.conclusions = conclusions
    def __repr__(self):
        return f"{self.premises} => {self.conclusions}"

# Define common predicates and connectives for convenience
s = Predicate('s')
o = Predicate('o')
n = Predicate('n')
neg = Predicate('neg')
comp_nec = Predicate('comp_nec')
exp_nec = Predicate('exp_nec')
exp_poss = Predicate('exp_poss')
comp_poss = Predicate('comp_poss')
conj = Predicate('conj')

# =================================================================
# Part 1 & 2: Core Logic Engine
# =================================================================

class IncompatibilitySemantics:
    """
    A logic engine implementing incompatibility semantics, translating the
    functionality from 'incompatibility_semantics.pl'.
    """

    def __init__(self):
        # --- Part 0: Setup ---
        self.current_domain = 'n'
        self._init_knowledge_base()

    def _init_knowledge_base(self):
        # --- Part 1.1: Geometry ---
        self.incompatible_pairs = {
            ('square', 'r1'), ('rectangle', 'r1'), ('rhombus', 'r1'), ('parallelogram', 'r1'), ('kite', 'r1'),
            ('square', 'r2'), ('rhombus', 'r2'), ('kite', 'r2'),
            ('square', 'r3'), ('rectangle', 'r3'), ('rhombus', 'r3'), ('parallelogram', 'r3'),
            ('square', 'r4'), ('rhombus', 'r4'), ('kite', 'r4'),
            ('square', 'r5'), ('rectangle', 'r5'), ('rhombus', 'r5'), ('parallelogram', 'r5'), ('trapezoid', 'r5'),
            ('square', 'r6'), ('rectangle', 'r6')
        }
        self.geometric_shapes = {'square', 'rectangle', 'rhombus', 'parallelogram', 'trapezoid', 'kite', 'quadrilateral'}
        
        # --- EML Axioms (for structural rule) ---
        self.eml_axioms = {
            Atom('u'): comp_nec(Atom('a')),
            Atom('u_prime'): comp_nec(Atom('a')),
            Atom('a'): [exp_poss(Atom('lg')), comp_poss(Atom('t'))],
            Atom('t'): comp_nec(neg(Atom('u'))),
            Atom('lg'): exp_nec(Atom('u_prime')),
            Atom('t_b'): comp_nec(Atom('t_n')),
            Atom('t_n'): comp_nec(Atom('t_b'))
        }

    # --- Part 1.2: Domain & Arithmetic Helpers ---
    def set_domain(self, domain):
        if domain in ['n', 'z', 'q']:
            self.current_domain = domain

    def obj_coll(self, val):
        if self.current_domain == 'n':
            return isinstance(val, int) and val >= 0
        if self.current_domain == 'z':
            return isinstance(val, int)
        if self.current_domain == 'q':
            return isinstance(val, (int, Fraction))
        return False

    def _arith_op(self, op, a, b):
        try:
            a_frac = Fraction(a)
            b_frac = Fraction(b)
            if op == '+': return a_frac + b_frac
            if op == '-': return a_frac - b_frac
            if op == '*': return a_frac * b_frac
            if op == '/': return a_frac / b_frac
        except ZeroDivisionError:
            return None
        return None

    # --- Part 1.3: Number Theory Helpers ---
    def _is_prime(self, n):
        if not isinstance(n, int) or n <= 1: return False
        if n <= 3: return True
        if n % 2 == 0 or n % 3 == 0: return False
        i = 5
        while i * i <= n:
            if n % i == 0 or n % (i + 2) == 0:
                return False
            i += 6
        return True

    def _find_prime_factor(self, n):
        if n % 2 == 0: return 2
        d = 3
        while d * d <= n:
            if n % d == 0:
                return d
            d += 2
        return n

    def _product_of_list(self, lst):
        return math.prod(lst)

    # --- Part 2.1: Incoherence Definitions ---
    def incoherent(self, premises):
        """Full check for incoherence. A set is incoherent if it's
           immediately inconsistent or proves a contradiction."""
        if self._is_incoherent_check(premises):
            return True
        # Check if premises prove an empty conclusion (contradiction)
        return self.proves(Sequent(premises, []))

    def _is_incoherent_check(self, x):
        """Non-recursive incoherence checks."""
        # Law of Non-Contradiction
        for p in x:
            if neg(p.args[0] if p.name == 'neg' else p) in x:
                return True
            if isinstance(p, Predicate) and len(p.args) == 1:
                # Check for s(p) and s(neg(p)) etc.
                if Predicate(p.name, [neg(p.args[0])]) in x:
                    return True
        
        # Geometric Incompatibility
        for p1, p2 in product(x, x):
            if (p1.name == 'n' and p2.name == 'n' and
                len(p1.args) == 1 and len(p2.args) == 1 and
                p1.args[0].name in self.geometric_shapes and
                p1.args[0].args == p2.args[0].args):
                shape = p1.args[0].name
                restriction = p2.args[0].name
                if (shape, restriction) in self.incompatible_pairs:
                    return True

        # Arithmetic Incompatibility
        if self.current_domain == 'n':
            for p in x:
                if (p.name == 'n' and len(p.args) > 0 and 
                    isinstance(p.args[0], Predicate) and p.args[0].name == 'obj_coll' and
                    isinstance(p.args[0].args[0], Predicate) and p.args[0].args[0].name == 'minus'):
                    a, b, _ = p.args[0].args[0].args
                    if Fraction(a) < Fraction(b):
                        return True

        # Euclid Case 1 Incoherence
        primes = {p.args[0].args[0] for p in x if p.name == 'n' and p.args[0].name == 'prime'}
        completes = [p.args[0].args[0] for p in x if p.name == 'n' and p.args[0].name == 'is_complete']
        for l in completes:
            ef = self._product_of_list(l) + 1
            if ef in primes:
                return True
        
        return False

    # --- Part 2.2: Sequent Calculus Prover ---
    def proves(self, sequent):
        """Public method to start the proof process."""
        # Use a frozenset for history items to ensure hashability
        return self._proves_impl(sequent, frozenset())

    def _proves_impl(self, sequent, history):
        premises, conclusions = sequent.premises, sequent.conclusions
        
        # PRIORITY 1: Identity and Explosion
        if any(p in conclusions for p in premises):
            return True
        if self._is_incoherent_check(premises):
            return True

        # PRIORITY 2: Material Inferences and Grounding
        # Arithmetic Grounding
        for c in conclusions:
            if c.name == 'o' and len(c.args) > 0:
                inner = c.args[0]
                if inner.name == 'plus' and len(inner.args) == 3:
                    a, b, res = inner.args
                    if self.obj_coll(a) and self.obj_coll(b) and self._arith_op('+', a, b) == res:
                        return True
                elif inner.name == 'minus' and len(inner.args) == 3:
                    a, b, res = inner.args
                    if self.obj_coll(a) and self.obj_coll(b):
                        calc_res = self._arith_op('-', a, b)
                        if calc_res == res and self.obj_coll(calc_res):
                             return True
                # Jason.pl Fraction Grounding
                elif inner.name == 'iterate' and len(inner.args) == 3:
                    u, m, r = inner.args
                    if self.obj_coll(u) and isinstance(m, int) and m >= 0 and self._arith_op('*', u, m) == r:
                        return True
                elif inner.name == 'partition' and len(inner.args) == 3:
                    w, n_val, u = inner.args
                    if self.obj_coll(w) and isinstance(n_val, int) and n_val > 0 and self._arith_op('/', w, n_val) == u:
                        return True
                        
        # Number Theory Grounding
        for c in conclusions:
            if c.name == 'n' and len(c.args) > 0 and isinstance(c.args[0], Predicate):
                inner = c.args[0]
                if inner.name == 'prime' and self._is_prime(inner.args[0]):
                    return True
                if inner.name == 'composite' and isinstance(inner.args[0], int) and inner.args[0] > 1 and not self._is_prime(inner.args[0]):
                    return True
        
        # PRIORITY 3: Structural and Logical Rules
        # We check rules that branch or add new premises recursively.
        # To avoid infinite loops, we check history.
        
        # --- Reduction Schemata (Negation) ---
        for i, p in enumerate(premises):
            if p.name == 'neg': # LN
                new_premises = premises[:i] + premises[i+1:]
                new_conclusions = conclusions + [p.args[0]]
                if self._proves_impl(Sequent(new_premises, new_conclusions), history): return True
            elif isinstance(p, Predicate) and len(p.args) == 1 and isinstance(p.args[0], Predicate) and p.args[0].name == 'neg':
                # e.g., s(neg(p))
                new_premises = premises[:i] + premises[i+1:]
                new_conclusions = conclusions + [Predicate(p.name, [p.args[0].args[0]])]
                if self._proves_impl(Sequent(new_premises, new_conclusions), history): return True

        for i, c in enumerate(conclusions):
            if c.name == 'neg': # RN
                new_premises = premises + [c.args[0]]
                new_conclusions = conclusions[:i] + conclusions[i+1:]
                if self._proves_impl(Sequent(new_premises, new_conclusions), history): return True
            elif isinstance(c, Predicate) and len(c.args) == 1 and isinstance(c.args[0], Predicate) and c.args[0].name == 'neg':
                # e.g., s(neg(p))
                new_premises = premises + [Predicate(c.name, [c.args[0].args[0]])]
                new_conclusions = conclusions[:i] + conclusions[i+1:]
                if self._proves_impl(Sequent(new_premises, new_conclusions), history): return True

        # --- Reduction Schemata (Conjunction) ---
        for i, p in enumerate(premises):
            if p.name == 'conj':
                new_premises = premises[:i] + [p.args[0], p.args[1]] + premises[i+1:]
                if self._proves_impl(Sequent(new_premises, conclusions), history): return True
            elif p.name in ['s', 'n', 'o'] and p.args[0].name == 'conj':
                x, y = p.args[0].args
                new_premises = premises[:i] + [Predicate(p.name, [x]), Predicate(p.name, [y])] + premises[i+1:]
                if self._proves_impl(Sequent(new_premises, conclusions), history): return True

        for i, c in enumerate(conclusions):
            if c.name == 'conj':
                x, y = c.args
                new_conclusions = conclusions[:i] + conclusions[i+1:]
                if (self._proves_impl(Sequent(premises, new_conclusions + [x]), history) and
                    self._proves_impl(Sequent(premises, new_conclusions + [y]), history)):
                    return True
            elif c.name in ['s', 'n', 'o'] and c.args[0].name == 'conj':
                x, y = c.args[0].args
                new_conclusions = conclusions[:i] + conclusions[i+1:]
                if (self._proves_impl(Sequent(premises, new_conclusions + [Predicate(c.name, [x])]), history) and
                    self._proves_impl(Sequent(premises, new_conclusions + [Predicate(c.name, [y])]), history)):
                    return True
        
        # --- General Forward Chaining (Modus Ponens) ---
        # This rule simulates applying material inferences.
        # This is one of the most complex parts to translate.
        
        # Arithmetic Commutativity
        for p in premises:
            if p.name == 'n' and p.args[0].name == 'plus':
                a, b, c = p.args[0].args
                new_premise = n(Predicate('plus', [b, a, c]))
                if new_premise not in premises and self._proves_impl(Sequent([new_premise] + premises, conclusions), history):
                    return True

        # Geometric Entailment
        for p in premises:
            if p.name == 'n' and p.args[0].name in self.geometric_shapes:
                p_shape = p.args[0].name
                p_var = p.args[0].args[0]
                for q_shape in self.geometric_shapes:
                    if p_shape != q_shape:
                        # Check if P entails Q
                        p_incomps = {r for s, r in self.incompatible_pairs if s == p_shape}
                        q_incomps = {r for s, r in self.incompatible_pairs if s == q_shape}
                        if q_incomps.issubset(p_incomps):
                            new_premise = n(Predicate(q_shape, [p_var]))
                            if new_premise not in premises and self._proves_impl(Sequent([new_premise] + premises, conclusions), history):
                                return True
        
        # --- EML Dynamics ---
        for i, p in enumerate(premises):
            if p.name == 's' and p.args[0] in self.eml_axioms:
                if (p,) not in history: # History check for this specific rule
                    new_history = history | frozenset([(p,)])
                    results = self.eml_axioms[p.args[0]]
                    if not isinstance(results, list): results = [results]
                    
                    for m_q in results:
                        q = m_q.args[0] if m_q.name in [comp_nec, exp_nec] else None
                        if q: # Necessity drives state transition
                            rest_premises = premises[:i] + premises[i+1:]
                            new_premises = [s(q)] + rest_premises
                            if self._proves_impl(Sequent(new_premises, conclusions), new_history):
                                return True
                        else: # Possibility is checked against conclusion
                            if s(m_q) in conclusions or m_q in conclusions:
                                return True

        # --- Euclid's Proof Structural Rules ---
        completes_in_premises = [p for p in premises if p.name == 'n' and p.args[0].name == 'is_complete']
        for p_is_complete in completes_in_premises:
            L = p_is_complete.args[0].args[0]
            
            # Euclid's Construction
            state = ('euclid_construction', tuple(L))
            if state not in history:
                ef = self._product_of_list(L) + 1
                
                # Case Analysis on EF
                new_history = history | frozenset([state])
                
                # Case 1: EF is prime
                p_prime = n(Predicate('prime', [ef]))
                if self._proves_impl(Sequent([p_prime] + premises, conclusions), new_history):
                    # Case 2: EF is composite
                    p_composite = n(Predicate('composite', [ef]))
                    if self._proves_impl(Sequent([p_composite] + premises, conclusions), new_history):
                        return True
        
        # Prime Factorization Rule
        composites_in_premises = [p for p in premises if p.name == 'n' and p.args[0].name == 'composite']
        for p_composite in composites_in_premises:
            N = p_composite.args[0].args[0]
            state = ('factorization', N)
            if state not in history:
                g = self._find_prime_factor(N)
                new_premises = [n(Predicate('prime', [g])), n(Predicate('divides', [g, N]))] + premises
                if self._proves_impl(Sequent(new_premises, conclusions), history | frozenset([state])):
                    return True

        # Euclid Material Inferences (M4, M5) applied via Forward Chaining
        # This requires finding premises that match the antecedents of the rules.
        primes_in_premises = {p.args[0].args[0]: p for p in premises if p.name == 'n' and p.args[0].name == 'prime'}
        divides_in_premises = {(p.args[0].args[0], p.args[0].args[1]): p for p in premises if p.name == 'n' and p.args[0].name == 'divides'}
        
        for p_is_complete in completes_in_premises:
            L = p_is_complete.args[0].args[0]
            ef = self._product_of_list(L) + 1
            
            # Rule M5
            if ef in primes_in_premises and (ef, ef) in divides_in_premises:
                new_premise = n(neg(Predicate('member', [ef, L])))
                if new_premise not in premises:
                    # Rule M4 application after M5
                    if n(neg(Predicate('is_complete', [L]))) not in premises:
                       if self._proves_impl(Sequent(premises + [new_premise, n(neg(Predicate('is_complete', [L])))], conclusions), history):
                           return True

        return False


# =================================================================
# Part 3: Test Suite (Python equivalent of test_synthesis.pl)
# =================================================================

class TestUnifiedSynthesis(unittest.TestCase):

    def setUp(self):
        """Create a new engine instance for each test."""
        self.engine = IncompatibilitySemantics()

    # --- Tests for Part 1: Core Logic and Domains ---
    def test_identity_subjective(self):
        self.assertTrue(self.engine.proves(Sequent([s(Atom('p'))], [s(Atom('p'))])))

    def test_incoherence_subjective(self):
        self.assertTrue(self.engine.incoherent([s(Atom('p')), s(neg(Atom('p')))]))

    def test_negation_handling_subjective_lem(self):
        # Law of Excluded Middle: [] => [s(p), s(neg(p))]
        self.assertTrue(self.engine.proves(Sequent([], [s(Atom('p')), s(neg(Atom('p')))])))

    # --- Tests for Part 2: Arithmetic Coexistence and Fixes ---
    def test_arithmetic_commutativity_normative(self):
        prem = [n(Predicate('plus', [2, 3, 5]))]
        conc = [n(Predicate('plus', [3, 2, 5]))]
        self.assertTrue(self.engine.proves(Sequent(prem, conc)))

    def test_arithmetic_subtraction_limit_n(self):
        self.engine.set_domain('n')
        term = n(Predicate('obj_coll', [Predicate('minus', [3, 5, Var('_')])]))
        self.assertTrue(self.engine.incoherent([term]))

    def test_arithmetic_subtraction_limit_n_persistence(self):
        self.engine.set_domain('n')
        term = n(Predicate('obj_coll', [Predicate('minus', [3, 5, Var('_')])]))
        self.assertTrue(self.engine.incoherent([term, s(Atom('p'))]))

    def test_arithmetic_subtraction_limit_z(self):
        self.engine.set_domain('z')
        term = n(Predicate('obj_coll', [Predicate('minus', [3, 5, Var('_')])]))
        self.assertFalse(self.engine.incoherent([term]))

    # --- Tests for Part 3: Embodied Modal Logic (EML) ---
    def test_eml_dynamic_u_to_a(self):
        # Proves by transitioning u -> comp_nec(a) -> a
        self.assertTrue(self.engine.proves(Sequent([s(Atom('u'))], [s(Atom('a'))])))

    def test_eml_dynamic_full_cycle(self):
        # lg -> exp_nec(u_prime) -> u_prime -> comp_nec(a) -> a
        self.assertTrue(self.engine.proves(Sequent([s(Atom('lg'))], [s(Atom('a'))])))

    def test_eml_tension_expansive_poss(self):
        self.assertTrue(self.engine.proves(Sequent([s(Atom('a'))], [s(exp_poss(Atom('lg')))])))
    
    def test_eml_tension_compressive_poss(self):
        self.assertTrue(self.engine.proves(Sequent([s(Atom('a'))], [s(comp_poss(Atom('t')))])))

    def test_eml_tension_conjunction(self):
        conc = conj(exp_poss(Atom('lg')), comp_poss(Atom('t')))
        self.assertTrue(self.engine.proves(Sequent([s(Atom('a'))], [s(conc)])))
    
    def test_eml_fixation_consequence(self):
        # t -> comp_nec(neg(u)) -> neg(u)
        self.assertTrue(self.engine.proves(Sequent([s(Atom('t'))], [s(neg(Atom('u')))])))

    def test_hegel_loop_prevention(self):
        # This should fail as there's no path from t_b to an arbitrary 'x'
        self.assertFalse(self.engine.proves(Sequent([s(Atom('t_b'))], [s(Atom('x'))])))

    # --- Tests for Quadrilateral Hierarchy ---
    def test_quad_incompatibility_square_r1(self):
        x = Var('x')
        premises = [n(Predicate('square', [x])), n(Predicate('r1', [x]))]
        self.assertTrue(self.engine.incoherent(premises))

    def test_quad_compatibility_trapezoid_r1(self):
        x = Var('x')
        premises = [n(Predicate('trapezoid', [x])), n(Predicate('r1', [x]))]
        self.assertFalse(self.engine.incoherent(premises))

    def test_quad_entailment_square_rectangle(self):
        x = Var('x')
        prem = [n(Predicate('square', [x]))]
        conc = [n(Predicate('rectangle', [x]))]
        self.assertTrue(self.engine.proves(Sequent(prem, conc)))
    
    def test_quad_entailment_rectangle_square_fail(self):
        x = Var('x')
        prem = [n(Predicate('rectangle', [x]))]
        conc = [n(Predicate('square', [x]))]
        self.assertFalse(self.engine.proves(Sequent(prem, conc)))

    def test_quad_entailment_transitive(self):
        x = Var('x')
        prem = [n(Predicate('square', [x]))]
        conc = [n(Predicate('parallelogram', [x]))]
        self.assertTrue(self.engine.proves(Sequent(prem, conc)))

    def test_quad_projection_contrapositive(self):
        x = Var('x')
        prem = [n(neg(Predicate('rectangle', [x])))]
        conc = [n(neg(Predicate('square', [x])))]
        self.assertTrue(self.engine.proves(Sequent(prem, conc)))

    # --- Tests for Number Theory (Euclid's Proof) ---
    def test_euclid_grounding_prime(self):
        self.assertTrue(self.engine.proves(Sequent([], [n(Predicate('prime', [7]))])))
        self.assertFalse(self.engine.proves(Sequent([], [n(Predicate('prime', [6]))])))

    def test_euclid_grounding_composite(self):
        self.assertTrue(self.engine.proves(Sequent([], [n(Predicate('composite', [6]))])))
        self.assertFalse(self.engine.proves(Sequent([], [n(Predicate('composite', [7]))])))

    def test_euclid_case_1_incoherence(self):
        premises = [n(Predicate('prime', [7])), n(Predicate('is_complete', [[2, 3]]))]
        # incoherent because is_complete([2,3]) -> EF=7, and prime(7) is in premises.
        self.assertTrue(self.engine.incoherent(premises))

    def test_euclid_case_2_incoherence(self):
        L = [2, 3, 5, 7, 11, 13]
        N = 30031  # 59 * 509
        premises = [n(Predicate('composite', [N])), n(Predicate('is_complete', [L]))]
        # This will be incoherent through the proof steps
        self.assertTrue(self.engine.incoherent(premises))

    def test_euclid_theorem_infinitude_of_primes(self):
        premises = [n(Predicate('is_complete', [[2, 5, 11]]))]
        self.assertTrue(self.engine.incoherent(premises))

    # --- Tests for Fractions (Jason.pl integration) ---
    def test_fraction_obj_coll_q(self):
        self.engine.set_domain('q')
        self.assertTrue(self.engine.obj_coll(Fraction(1, 2)))
        self.assertTrue(self.engine.obj_coll(5))
        self.assertFalse(self.engine.obj_coll(Var('X'))) # Cannot check non-grounded term
    
    def test_fraction_obj_coll_n(self):
        self.engine.set_domain('n')
        self.assertFalse(self.engine.obj_coll(Fraction(1, 2)))
        self.assertTrue(self.engine.obj_coll(5))

    def test_fraction_addition_grounding(self):
        self.engine.set_domain('q')
        conc = [o(Predicate('plus', [Fraction(1, 2), Fraction(1, 3), Fraction(5, 6)]))]
        self.assertTrue(self.engine.proves(Sequent([], conc)))

    def test_fraction_addition_mixed(self):
        self.engine.set_domain('q')
        conc = [o(Predicate('plus', [2, Fraction(1, 4), Fraction(9, 4)]))]
        self.assertTrue(self.engine.proves(Sequent([], conc)))

    def test_fraction_subtraction_grounding(self):
        self.engine.set_domain('q')
        conc = [o(Predicate('minus', [Fraction(1, 2), Fraction(1, 3), Fraction(1, 6)]))]
        self.assertTrue(self.engine.proves(Sequent([], conc)))

    def test_fraction_subtraction_limit_n(self):
        self.engine.set_domain('n')
        prem = [n(Predicate('obj_coll', [Predicate('minus', [Fraction(1, 3), Fraction(1, 2), Var('_')])]))]
        self.assertTrue(self.engine.incoherent(prem))

    def test_fraction_iteration_grounding(self):
        self.engine.set_domain('q')
        conc = [o(Predicate('iterate', [Fraction(1, 3), 4, Fraction(4, 3)]))]
        self.assertTrue(self.engine.proves(Sequent([], conc)))

    def test_fraction_partition_grounding(self):
        self.engine.set_domain('q')
        conc = [o(Predicate('partition', [Fraction(4, 3), 4, Fraction(1, 3)]))]
        self.assertTrue(self.engine.proves(Sequent([], conc)))

if __name__ == '__main__':
    unittest.main()