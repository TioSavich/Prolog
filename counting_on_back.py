from automata.pda.dpda import DPDA
from automata.base.exceptions import RejectionException

# --- Stack to integer converter ---
def stack_to_int_3digit(stack_tuple: tuple) -> int:
    if not (len(stack_tuple) == 4 and stack_tuple[0] == '#' and
            stack_tuple[1].startswith('H') and stack_tuple[2].startswith('T') and stack_tuple[3].startswith('U')):
        raise ValueError(f"Invalid stack state: {stack_tuple}")
    h = int(stack_tuple[1][1:])
    t = int(stack_tuple[2][1:])
    u = int(stack_tuple[3][1:])
    return h * 100 + t * 10 + u

# --- DPDA definition (0–999, up/down) ---
states = {
    'q_start', 'q_idle',
    'q_inc_tens', 'q_inc_hundreds', 'q_halt',
    'q_dec_tens', 'q_dec_hundreds', 'q_underflow'
}
input_symbols = {'tick', 'tock'}
stack_symbols = {'#'} | {f'H{i}' for i in range(10)} | {f'T{i}' for i in range(10)} | {f'U{i}' for i in range(10)}

transitions = {
    'q_start': {'': {'#': ('q_idle', ('U0', 'T0', 'H0', '#'))}},

    'q_idle': {
        'tick': {
            **{f'U{n}': ('q_idle', (f'U{n+1}',)) for n in range(9)},
            'U9': ('q_inc_tens', ())
        },
        'tock': {
            **{f'U{n}': ('q_idle', (f'U{n-1}',)) for n in range(1, 10)},
            'U0': ('q_dec_tens', ())
        }
    },

    'q_inc_tens': {'': {
        **{f'T{m}': ('q_idle', ('U0', f'T{m+1}')) for m in range(9)},
        'T9': ('q_inc_hundreds', ())
    }},

    'q_inc_hundreds': {'': {
        **{f'H{k}': ('q_idle', ('U0', 'T0', f'H{k+1}')) for k in range(9)},
        'H9': ('q_halt', ('U0', 'T0', 'H0'))
    }},

    'q_dec_tens': {'': {
        **{f'T{m}': ('q_idle', ('U9', f'T{m-1}')) for m in range(1, 10)},
        'T0': ('q_dec_hundreds', ())
    }},

    'q_dec_hundreds': {'': {
        **{f'H{k}': ('q_idle', ('U9', 'T9', f'H{k-1}')) for k in range(1, 10)},
        'H0': ('q_underflow', ('U9', 'T9', 'H9'))
    }},

    'q_halt': {},
    'q_underflow': {}
}

initial_state = 'q_start'
initial_stack_symbol = '#'
final_states = {'q_idle'}

# Instantiate once
dpda = DPDA(
    states=states,
    input_symbols=input_symbols,
    stack_symbols=stack_symbols,
    transitions=transitions,
    initial_state=initial_state,
    initial_stack_symbol=initial_stack_symbol,
    final_states=final_states,
    acceptance_mode='final_state'
)

# --- Counting function ---
def count_dpda(N: int, k: int, direction: str) -> int:
    symbol = 'tick' if direction == 'up' else 'tock'
    # combine initial ticks and offset
    seq = ['tick'] * N + [symbol] * k
    final_config = dpda.read_input(seq)
    return stack_to_int_3digit(final_config.stack.stack)

# --- Tests ---
tests = [
    (42, 'up', 7),
    (42, 'down', 7),
    (0, 'down', 1),
    (999, 'up', 1),
]

print("Testing extended 3-digit DPDA:")
for N, dirn, k in tests:
    try:
        result = count_dpda(N, k, dirn)
        print(f"{N} {dirn} {k} → {result}")
    except RejectionException:
        print(f"{N} {dirn} {k} → REJECTED (overflow/underflow)")
    except Exception as e:
        print(f"Error testing {N} {dirn} {k}: {e}")
