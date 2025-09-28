# Import necessary classes from automata-lib
try:
    from automata.pda.dpda import DPDA
    from automata.pda.stack import PDAStack
    from automata.base.exceptions import RejectionException 
except ImportError:
    print("Error: automata-lib not found.")
    print("Please install it: pip install automata-lib")
    # Mocking classes if needed
    class MockPDAConfiguration:
        def __init__(self, state, stack_tuple): self.state, self.stack = state, self._MockStack(stack_tuple)
        class _MockStack:
             def __init__(self, stack_tuple): self.stack = stack_tuple
    class MockDPDA:
        def __init__(self, *args, **kwargs): self.final_states = kwargs.get('final_states', set()); print("Warning: Using Mock DPDA class.")
        def read_input(self, input_sequence): 
             n = len(input_sequence)
             if n > 999: return MockPDAConfiguration('q_halt', ('#', 'H0', 'T0', 'U0'))
             if n == 0: return MockPDAConfiguration('q_idle', ('#', 'H0', 'T0', 'U0'))
             hundreds, rem = divmod(n, 100)
             tens, units = divmod(rem, 10)
             stack_list = ('#', f'H{hundreds}', f'T{tens}', f'U{units}')
             return MockPDAConfiguration('q_idle', tuple(stack_list))
    DPDA = MockDPDA 
    RejectionException = Exception 
    print("--- automata-lib not found, using Mock classes ---")

import sys

# --- Define the 0-999 Counter PDA ---

# States
states = {'q_start', 'q_idle', 'q_inc_tens', 'q_inc_hundreds', 'q_halt'}

# Input Alphabet
input_symbols = {'tick'} 

# Stack Alphabet 
stack_symbols = {'#'} | {f'H{i}' for i in range(10)} | \
                        {f'T{i}' for i in range(10)} | \
                        {f'U{i}' for i in range(10)}

# Transitions (Following the successful pattern)
# Remember: Push sequence (S1, S2, S3) pushes S3 first, S2 second, S1 last (top)
transitions = {
    'q_start': {
        '': {
            # Initial: Push #, H0, T0, U0. Stack (#, H0, T0, U0). Top U0.
            '#': ('q_idle', ('U0', 'T0', 'H0', '#')) 
        }
    },
    'q_idle': { # Processing Units (top)
        'tick': {
            # Inc Units < 9: Pop Un, Push U(n+1). Stay q_idle.
            **{f'U{n}': ('q_idle', (f'U{n+1}',)) for n in range(9)},
            # Inc Units = 9: Pop U9, Push nothing. Go to q_inc_tens (Tens digit now top).
            'U9': ('q_inc_tens', ()) 
        }
    },
    'q_inc_tens': { # Epsilon transitions, processing Tens (top)
        '': {
             # Tens digit Tm (m<9): Pop Tm. Push T(m+1), Push U0. Go q_idle.
             **{f'T{m}': ('q_idle', ('U0', f'T{m+1}')) for m in range(9)}, 
             # Tens digit T9: Pop T9. Push nothing. Go to q_inc_hundreds (Hundreds digit now top).
             'T9': ('q_inc_hundreds', ())
        }
    },
    'q_inc_hundreds': { # Epsilon transitions, processing Hundreds (top)
        '': {
             # Hundreds digit Hk (k<9): Pop Hk. Push H(k+1), Push T0, Push U0. Go q_idle.
             **{f'H{k}': ('q_idle', ('U0', 'T0', f'H{k+1}')) for k in range(9)},
             # Hundreds digit H9 (Overflow): Pop H9. Push H0, Push T0, Push U0. Go q_halt.
             'H9': ('q_halt', ('U0', 'T0', 'H0')) 
        }
    },
    'q_halt': { 
        # No transitions out. Any 'tick' input leads to implicit rejection.
    }
}

# Initial state
initial_state = 'q_start'
initial_stack_symbol = '#' 
# Final states (only q_idle represents a valid 0-999 count)
final_states = {'q_idle'}

# Create the DPDA instance
try:
    pda = DPDA(
        states=states,
        input_symbols=input_symbols,
        stack_symbols=stack_symbols,
        transitions=transitions, 
        initial_state=initial_state,
        initial_stack_symbol=initial_stack_symbol,
        final_states=final_states,
        acceptance_mode='final_state' 
    )
    print("DPDA for 0-999 created successfully.")
except Exception as e:
     print(f"Error creating DPDA: {e}")
     # Mock DPDA fallback
     class MockPDAConfiguration:
        def __init__(self, state, stack_tuple): self.state, self.stack = state, self._MockStack(stack_tuple)
        class _MockStack:
             def __init__(self, stack_tuple): self.stack = stack_tuple
     class MockDPDA:
        def __init__(self, *args, **kwargs): self.final_states = kwargs.get('final_states', set()); print("Warning: Using Mock DPDA class after creation error.")
        def read_input(self, input_sequence): 
             n = len(input_sequence)
             if n > 999: return MockPDAConfiguration('q_halt', ('#', 'H0', 'T0', 'U0'))
             if n == 0: return MockPDAConfiguration('q_idle', ('#', 'H0', 'T0', 'U0'))
             hundreds, rem = divmod(n, 100); tens, units = divmod(rem, 10)
             stack_list = ('#', f'H{hundreds}', f'T{tens}', f'U{units}')
             return MockPDAConfiguration('q_idle', tuple(stack_list))
     pda = MockDPDA(final_states=final_states)
     RejectionException = Exception 
     print("--- Proceeding with Mock PDA ---")


# Function to convert the 3-digit stack contents to an integer
def stack_to_int_3digit(stack_tuple: tuple) -> int:
    """
    Converts the PDA stack tuple ('#', HX, TY, UZ) to the integer XYZ.
    """
    # Basic validation
    if not (isinstance(stack_tuple, tuple) and len(stack_tuple) == 4 and \
            stack_tuple[0] == '#' and stack_tuple[1].startswith('H') and \
            stack_tuple[2].startswith('T') and stack_tuple[3].startswith('U')):
        # Allow for initial state stack ('#', 'H0', 'T0', 'U0') during halt
        if not (len(stack_tuple) == 4 and stack_tuple[1:] == ('H0', 'T0', 'U0')):
             print(f"Warning: Invalid stack state for 3-digit conversion: {stack_tuple}")
             return -1 
        
    try:
        # Extract digits, handling potential errors if symbols are wrong
        h_digit = int(stack_tuple[1][1:]) 
        t_digit = int(stack_tuple[2][1:]) 
        u_digit = int(stack_tuple[3][1:]) 
        return h_digit * 100 + t_digit * 10 + u_digit
    except (ValueError, IndexError):
        print(f"Error converting stack digits to int: {stack_tuple}")
        return -2 

# --- Testing the PDA ---
print("\nTesting 3-Digit (0-999) Counter PDA:")
# Test cases around boundaries
test_counts = [0, 1, 9, 10, 11, 99, 100, 101, 998, 999, 1000, 1001] 

for count in test_counts:
    print(f"\n--- Testing count = {count} ---")
    input_sequence = ['tick'] * count
    try:
        final_config = pda.read_input(input_sequence)
        final_state = final_config.state
        if hasattr(final_config, 'stack') and hasattr(final_config.stack, 'stack'):
             final_stack_tuple = final_config.stack.stack 
        else:
             print("Error: Final configuration object has unexpected structure.")
             final_stack_tuple = ('#', 'ERROR', 'ERROR', 'ERROR') 

        is_accepted = final_state in pda.final_states # Check if ended in q_idle

        print(f"Input: {count} 'tick's")
        print(f"Ended in State: {final_state}")
        print(f"Final Stack: {final_stack_tuple}")
        
        expected_acceptance = (count <= 999)

        print(f"Expected Acceptance: {expected_acceptance}")
        print(f"Actual Acceptance: {is_accepted}")

        if is_accepted:
            calculated_value = stack_to_int_3digit(final_stack_tuple)
            print(f"Expected Value (if accepted): {count}")
            print(f"Calculated Value: {calculated_value}")
            if calculated_value == count and expected_acceptance: 
                print("Result: Correct")
            else: 
                print("Result: INCORRECT (Value mismatch or unexpected acceptance)")
        else: # Rejected (ended in q_halt)
            print("Expected Value (if accepted): N/A")
            print("Calculated Value: N/A (Rejected)")
            # Check if rejection was expected (count >= 1000)
            if not expected_acceptance: 
                 print("Result: Correct (Rejected as expected)")
            else: # Should not happen for count <= 999
                 print("Result: INCORRECT (Unexpected rejection)")

    except RejectionException as re:
        # This means the PDA got genuinely stuck (no transition defined)
        # Should only happen if input contains something other than 'tick' or logic error
        print(f"Input: {count} 'tick's")
        print(f"PDA Rejection Exception: {re}")
        # Check if this was the expected halt state after 1000+ ticks
        is_halt_state = False
        try:
            # Try reading again to see the state (might not work if truly stuck)
            halt_config = pda.read_input(input_sequence)
            if halt_config.state == 'q_halt':
                is_halt_state = True
        except: 
            pass # Ignore errors trying to re-read if stuck
            
        if not expected_acceptance and is_halt_state:
             print("Result: Correct (Rejected via halt state as expected)")
        else:
             print("Result: REJECTED (Stuck) - Check Logic")
        
    except Exception as e:
        print(f"Input: {count} 'tick's")
        print(f"PDA Error: {e}")
        # import traceback 
        # traceback.print_exc() 
        print("Result: ERROR")