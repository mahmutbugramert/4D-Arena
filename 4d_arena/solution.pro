% gulsen sabak
% 2020400072
% compiling: yes
% complete: yes

distance(0, 0, 0).  % a dummy predicate to make the sim work.

% distance(Agent, TargetAgent, Distance).
% multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
% nearest_agent(StateId, AgentId, NearestAgentId, Distance).
% nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
% num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues).
% difficulty_of_state(StateId, Name, AgentClass, Difficulty).
% easiest_traversable_state(StateId, AgentId, TargetStateId).
% basic_action_policy(StateId, AgentId, Action).

distance(Agent, TargetAgent, Distance):-
    % reaching the x and y values of Agent from dictionary.
    get_dict(x, Agent, X),
    get_dict(y, Agent, Y),
    %reaching the x and y values of TargetAgent from dictionary.
    get_dict(x, TargetAgent, X0),
    get_dict(y, TargetAgent, Y0),
    %calculate distance from the given formula in the description.
    Distance is abs(Y0-Y)+ abs(X0-X).



multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance):-
    % creating state to reach Agents dictionary
    state(StateId, Agents,_,_),
    % getting the agent and its x and y values
    A1 = Agents.AgentId,
    get_dict(x, A1, X),
    get_dict(y, A1, Y),
    % creating history and taking its parameters values to use in the given formula
    history(StateId, UniverseId, Time,_),
    arg(2, history(StateId, UniverseId, Time,_), U),
    arg(3, history(StateId, UniverseId, Time,_), Ti),

    % creating state to reach NewAgents dictionary.
    state(TargetStateId, NewAgents,_,_),
    % getting the agent and its x and y values
    B = NewAgents.TargetAgentId,
    get_dict(x, B, X0),
    get_dict(y, B, Y0),
    % creating history and taking its parameters values to use in the given formula
    history(TargetStateId, UniverseId1, Time1,_),
    arg(2, history(TargetStateId, UniverseId1,Time1,_), U2),
    arg(3, history(TargetStateId,UniverseId1, Time1,_), Ti2),

    % getting the class of the agent which has AGENTID
    get_dict(class, A1, Cl1),

    % determining the travelcost depending on the class
    (Cl1 == wizard -> TravelCost is 2 ; TravelCost is 5 ),

    % calculating the given formula
    Distance is abs(Y0-Y)+ abs(X0-X) + TravelCost*(abs(Ti-Ti2) + abs(U- U2)).


nearest_agent(StateId, AgentId, NearestAgentId, Distance):-
    % creating state and getting the agent
    state(StateId, Agents, _, _),
    MyAgent= Agents.get(AgentId),

    % using findall to traverse all states and collect Distances and Id numbers as tuples
    findall((Dist, Key),
    (get_dict(Key, Agents, TargetAgent),
    % controlling the given conditions
    MyAgent.name \= TargetAgent.name,
    % calculating distances
    distance(MyAgent, TargetAgent, Dist)), List1),
    % finding the minimum element of the findall list wrt Distances.
    min_member((Distance, NearestAgentId), List1).


nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance):-
    % creating state and getting the agent
    state(StateId, Agents,_,_),
    MyAgent = Agents.AgentId,

    % using findall to traverse all states and collect Distances TargetIds and TargetStates
    findall((Dist, Target, Key),(
    state(Target, TargetAgents, _, _),
    get_dict(Key, TargetAgents, TargetAgent),
    % controlling the given conditions
    MyAgent.name \= TargetAgent.name,
    % calculating multiverse distances
    multiverse_distance(StateId, AgentId, Target, Key, Dist) 
    ),List1),
    % finding the minimum element of the findall list wrt Distances.
    min_member((Distance, TargetStateId, TargetAgentId), List1).



num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues):-
    % creating state
    state(StateId, Agents, _, _),

% using findall to find all TargetAgents which classes are warriors
findall(TargetAgent, (
    get_dict(Key, Agents, TargetAgent),
    TargetAgent.name \= Name,
    TargetAgent.class = 'warrior'), List1),
    % finding the length of the list to find the numberofwarriors
    length(List1, Length1),
    NumWarriors is Length1,

% using findall to find all TargetAgents which classes are wizards
findall(TargetAgent, (
    get_dict(Key, Agents, TargetAgent),
    TargetAgent.name \= Name,
    TargetAgent.class = 'wizard'), List2),
    % finding the length of the list to find the numberofwizards
    length(List2, Length2),
    NumWizards is Length2,

% using findall to find all TargetAgents which classes are rogues
findall(TargetAgent, (
    get_dict(Key, Agents, TargetAgent),
    TargetAgent.name \= Name,
    TargetAgent.class = 'rogue'), List3),
    % finding the length of the list to find the numberofrogues
    length(List3, Length3),
    NumRogues is Length3.   



difficulty_of_state(StateId, Name, AgentClass, Difficulty):-
    % firstly finding the number of agents
    num_agents_in_state(StateId,Name, NW, NWi, NR),
    % comparing the class type to determine Difficulty correctly with the given formulas in description file
    (AgentClass = 'warrior' -> Difficulty is (5*NW + 8*NWi + 2*NR) ; (AgentClass = 'wizard' -> Difficulty is (2*NW + 5*NWi + 8*NR)); (AgentClass = 'rogue' -> Difficulty is (8*NW + 2*NWi + 5*NR))).


% portal_to_now is used for determining the state is traversable or not wrt right now time  
portal_to_now(StateId, AgentId, TargetStateId):-
    % this codes comes from the simulator.pro file 
    % differently I create state for given StateId and also I create 2 history, one of them is used for StateId, the other is used for creating history via TargetUniverseId.
    state(StateId, Agents, _, TurnOrder),
    history(_,TargetUniverseId, TargetTime, _),
    history(StateId, UniverseId, Time, _),
    % taking agent from Agents dictionary
    Agent = Agents.get(AgentId),
    % agent cannot time travel if there is only one agent in the universe
    length(TurnOrder, NumAgents),
    NumAgents > 1,
    % agent can only travel to now if it's the first turn in the target universe
    current_time(TargetUniverseId, TargetTime, 0), 
    % agent cannot travel to current universe's now (would be a no-op)
    \+(TargetUniverseId = UniverseId),
    % check whether there is enough mana
    (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
    Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
    Agent.mana >= Cost,
    % check whether the target location is occupied
    get_latest_target_state(TargetUniverseId, TargetTime, TargetStateId),
    state(TargetStateId, TargetAgents, _, TargetTurnOrder),
    TargetState = state(TargetStateId, TargetAgents, _, TargetTurnOrder),
    \+tile_occupied(Agent.x, Agent.y, TargetState).

% portal is used for determining the state is traversable or not wrt past time  
portal(StateId, AgentId, TargetStateId):-
    % this codes comes from the simulator.pro file 
    % differently I create state for given StateId and also I create 2 history, one of them is used for StateId, the other is used for creating history via TargetUniverseId.
    state(StateId, Agents, _, TurnOrder),
    history(StateId, UniverseId, Time, _),
    history(_,TargetUniverseId, TargetTime,_),
    % taking agent from Agents dictionary.
    Agent = Agents.get(AgentId),
     % check whether global universe limit has been reached
    global_universe_id(GlobalUniverseId),
    universe_limit(UniverseLimit),
    GlobalUniverseId < UniverseLimit,
    % agent cannot time travel if there is only one agent in the universe
    length(TurnOrder, NumAgents),
    NumAgents > 1,
    %[TargetUniverseId, TargetTime] = ActionArgs,
    % check whether target is now or in the past
    current_time(TargetUniverseId, TargetUniCurrentTime, _),
    TargetTime < TargetUniCurrentTime,
    % check whether there is enough mana
    (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
    Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
    Agent.mana >= Cost,
    % check whether the target location is occupied
    get_earliest_target_state(TargetUniverseId, TargetTime, TargetStateId),
    state(TargetStateId, TargetAgents, _, TargetTurnOrder),
    TargetState = state(TargetStateId, TargetAgents, _, TargetTurnOrder),
    \+tile_occupied(Agent.x, Agent.y, TargetState).


easiest_traversable_state(StateId, AgentId, TargetStateId) :-
    % firstly I create state from StateId 
    state(StateId, Agents, _, _),

    % this findall is used for finding the tuples (Difficulty, TargetStateId)
    findall((Diff,Target), (
        history(Target, _,_,_),
        % creating agent from Agents dictionary.
        Agent = Agents.get(AgentId),
        % controlling the given 
        (portal_to_now(StateId, AgentId, Target); portal(StateId, AgentId, Target)),  difficulty_of_state(Target, Agent.name, Agent.class, Diff),
        Diff >0), List2),

    % creating agent from Agents dictionary
    Agent = Agents.get(AgentId),
    % calculating the difficulty of the given state which has StateId
    difficulty_of_state(StateId, Agent.name, Agent.class, Diff1),
    % Controlling whether the Difficulty is bigger than zero or not. If it is bigger than zero, than append the list which comes from the findall.
    (Diff1>0 -> append([(Diff1, StateId)], List2, FulList)),
    % finding the min member (wrt difficulty) of the latest list that we get from findall and append
    min_member((_, TargetStateId), FulList).
     


basic_action_policy(StateId, AgentId, Action):-
    % creating state from StateId
    state(StateId, Agents, _, _),
    % creating agent from Agents dictionary
    Agent = Agents.get(AgentId),

    % finding the easiest state
    easiest_traversable_state(StateId, AgentId, EasiestState),
    % creating history to get the UnivId
    history(EasiestState, UnivId,_,_),
    % finding the nearest agent
    nearest_agent(StateId, AgentId, Trgt, Distance),
    % creating the Agent2 from Agents dictionary with the Trgt
    Agent2 = Agents.Trgt,

    % if, else statements to control conditions
    (
    % firstly control the Easiest state comes from portal or portal_to_now
    portal_to_now(StateId, AgentId, EasiestState) -> Action = ([portal_to_now , UnivId]); 
    portal(StateId, AgentId, EasiestState) -> Action = ([portal, UnivId]);

    % comparing the Agent class and putting the Distance restriction to determine the Action 
    ((Agent.class = warrior, Distance =< 1) -> Action = [melee_attack, Trgt]);
    ((Agent.class = wizard, Distance =< 10) -> Action = [magic_missile, Trgt]);
    ((Agent.class = rogue, Distance =< 5 ) -> Action = [ranged_attack, Trgt]);

    % determining and comparing the x and y values of Agent and Agent2
    Agent2.get(x) > Agent.get(x) -> Action = ['move_right'];
    Agent2.get(x) < Agent.get(x) -> Action = ['move_left'];
    Agent2.get(y) > Agent.get(y) -> Action = ['move_up'];
    Agent2.get(y) < Agent.get(y) -> Action = ['move_down'];

    % if the action is none of the above, then rest.
    Action = ([rest])).

