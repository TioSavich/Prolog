/** <module> Knowledge Management Utilities
 *
 * Utilities for managing learned knowledge:
 * - Reset (clear all learned strategies)
 * - Backup (save current state)
 * - Restore (load previous state)
 * - Inspect (view current strategies)
 *
 * The system DOES retain learned knowledge across sessions by default,
 * stored in learned_knowledge.pl. These utilities give you control.
 */

:- module(knowledge_manager, [
    reset_learned_knowledge/0,
    backup_learned_knowledge/1,
    restore_learned_knowledge/1,
    inspect_learned_knowledge/0,
    count_learned_strategies/1
]).

:- use_module(more_machine_learner).
:- use_module(library(filesex)).

%!      reset_learned_knowledge is det.
%
%       CLEARS all learned strategies and deletes learned_knowledge.pl.
%       This resets the system to primordial state.
%
%       Use this when you want to test the full learning trajectory from scratch.
reset_learned_knowledge :-
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Resetting Learned Knowledge                               ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    % Count current strategies
    findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
    length(Strategies, Count),
    format('Current learned strategies: ~w~n', [Count]),
    (   Count > 0
    ->  format('  ~w~n', [Strategies])
    ;   true
    ),
    
    writeln(''),
    writeln('Clearing dynamic predicates...'),
    
    % Abolish all learned strategy clauses
    abolish(more_machine_learner:run_learned_strategy/5),
    dynamic(more_machine_learner:run_learned_strategy/5),
    
    writeln('Deleting learned_knowledge.pl...'),
    
    % Delete the knowledge file
    (   exists_file('learned_knowledge.pl')
    ->  delete_file('learned_knowledge.pl'),
        writeln('✓ learned_knowledge.pl deleted')
    ;   writeln('• learned_knowledge.pl not found (already clean)')
    ),
    
    writeln(''),
    writeln('✓ System reset to primordial state'),
    writeln('  Only Counting All strategy remains'),
    writeln('').

%!      backup_learned_knowledge(+BackupName) is det.
%
%       Saves current learned knowledge to a backup file.
%       Useful before experiments or testing.
%
%       @param BackupName Atom or string for backup filename (without extension)
backup_learned_knowledge(BackupName) :-
    atomic_list_concat(['learned_knowledge_', BackupName, '.pl'], BackupFile),
    
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Backing Up Learned Knowledge                              ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    (   exists_file('learned_knowledge.pl')
    ->  copy_file('learned_knowledge.pl', BackupFile),
        format('✓ Backed up to: ~w~n', [BackupFile]),
        
        % Count strategies in backup
        findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
        length(Strategies, Count),
        format('  Strategies backed up: ~w~n', [Count])
    ;   writeln('⚠️  No learned_knowledge.pl to backup'),
        writeln('  System is in primordial state')
    ),
    writeln('').

%!      restore_learned_knowledge(+BackupName) is det.
%
%       Restores learned knowledge from a backup file.
%
%       @param BackupName Atom or string for backup filename (without extension)
restore_learned_knowledge(BackupName) :-
    atomic_list_concat(['learned_knowledge_', BackupName, '.pl'], BackupFile),
    
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Restoring Learned Knowledge                               ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    (   exists_file(BackupFile)
    ->  % Clear current knowledge
        abolish(more_machine_learner:run_learned_strategy/5),
        dynamic(more_machine_learner:run_learned_strategy/5),
        
        % Copy backup to active file
        copy_file(BackupFile, 'learned_knowledge.pl'),
        
        % Reload
        consult('learned_knowledge.pl'),
        
        format('✓ Restored from: ~w~n', [BackupFile]),
        
        % Count restored strategies
        findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
        length(Strategies, Count),
        format('  Strategies restored: ~w~n', [Count]),
        (   Count > 0
        ->  format('  ~w~n', [Strategies])
        ;   true
        )
    ;   format('✗ Backup file not found: ~w~n', [BackupFile])
    ),
    writeln('').

%!      inspect_learned_knowledge is det.
%
%       Displays all currently learned strategies.
inspect_learned_knowledge :-
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Current Learned Knowledge                                 ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
    length(Strategies, Count),
    
    format('Total Strategies: ~w~n', [Count]),
    writeln(''),
    
    (   Count > 0
    ->  writeln('Strategy Names:'),
        forall(member(S, Strategies),
               format('  • ~w~n', [S]))
    ;   writeln('No strategies learned yet.'),
        writeln('System is in primordial state (Counting All only).')
    ),
    
    writeln(''),
    
    (   exists_file('learned_knowledge.pl')
    ->  size_file('learned_knowledge.pl', Size),
        format('Knowledge file size: ~w bytes~n', [Size])
    ;   writeln('Knowledge file: not found')
    ),
    writeln('').

%!      count_learned_strategies(-Count) is det.
%
%       Returns the number of currently learned strategies.
count_learned_strategies(Count) :-
    findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
    length(Strategies, Count).
