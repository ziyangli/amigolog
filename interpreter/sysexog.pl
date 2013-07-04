%% SOME SYSTEM BUILT-IN EXOGENOUS ACTIONS

exog_action(debug).             % Show debugging information	
exog_action(halt).		% Terminate program execution by jumping to the empty program
exog_action(abort).		% Abort program execution by jumping to ?(false) program
exog_action(break).		% Pause the execution of the program
exog_action(reset).		% Reset agent execution from scratch
exog_action(start).		% Start the execution of the program

%% BUILT-IN exogenous actions that will be mapped to system actions for the cycle
exog_action(debug_exec).	% Show debugging information	
exog_action(halt_exec).		% Terminate program execution by jumping to the empty program
exog_action(abort_exec). % Abort program execution by jumping to ?(false) program
exog_action(break_exec). % Pause the execution of the program
exog_action(reset_exec). % Reset agent execution from scratch
exog_action(start_exec). % Start the execution of the program

%% This are special actions that if they are in the current history
%% they are interpreted by the interpreter in a particular way
%% This should be seen as meta-actions that deal with the interpreter itself
system_action(debug_exec).	% Special action to force debugging
system_action(halt_exec).	% Action to force clean termination
system_action(abort_exec).	% Action to force sudden nonclean termination
system_action(start_exec).	% Action to start execution
system_action(break_exec).	% Action to break the agent execution to top-level Prolog
system_action(reset_exec).	% Reset agent execution from scratch

%% define actions that are used by the projector for management
system_action(e(_, _)).