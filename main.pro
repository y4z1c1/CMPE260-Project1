% yusuf anil yazici
% 2021400207
% compiling: yes
% complete: yes

:- ['cmpefarm.pro']. 
:- init_from_map.

% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
    X1 = Agent1.x,
    Y1 = Agent1.y,
    X2 = Agent2.x,
    Y2 = Agent2.y,
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents(State, NumberOfAgents) :-
    State = [Agents, _, _, _],  
    dict_keys(Agents, Keys), 
    length_list(Keys, NumberOfAgents).
    
% 3- value_of_farm(+State, -Value)
value_of_farm(State,Value):-
    State = [Agents, Objects, _, _],
    value_of_agents(Agents,AgentValue),
    value_of_objects(Objects,ObjectValue),
    Value is AgentValue + ObjectValue.

% Helpers to calculate the value of agents and objects.
value_of_agents(AgentsDict, TotalAgentValue) :-
    dict_pairs(AgentsDict, _, Pairs),
    findall(Value, (
        member(_Key-Agent, Pairs),
        get_subtype(Agent, Subtype),
        agent_value(Subtype, Value)
    ), Values),
    sum_list(Values, TotalAgentValue).

value_of_objects(ObjectsDict, TotalObjectValue) :-
    dict_pairs(ObjectsDict, _, Pairs),
    findall(Value, (
        member(_Key-Object, Pairs),
        get_subtype(Object, Subtype),
        value(Subtype, Value)
    ), Values),
    sum_list(Values, TotalObjectValue).

get_subtype(Agent, Subtype) :-
    get_dict(subtype, Agent, Subtype).

agent_value(wolf, 0).

agent_value(SubType,Value):-value(SubType,Value).

% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
find_food_coordinates(State, AgentId, Coordinates) :-
    State = [Agents, Objects, _, _],
    dict_pairs(Agents, _, AgentPairs),
    member(AgentId-Agent, AgentPairs),
    get_subtype(Agent, Subtype),
    (   Subtype = wolf
    ->  find_carnivore_food(Agents, Subtype, Coordinates)  
    ;   find_herbivore_food(Objects, Subtype, Coordinates)  
    ),
    Coordinates \= [],
    !. 

% Helpers for finding food based on agent type (carnivore or herbivore).
find_herbivore_food(Objects, Subtype, Coordinates) :-
    dict_pairs(Objects, _, ObjectPairs),
    findall([X, Y], (
        member(_-Object, ObjectPairs),
        get_dict(subtype, Object, FoodType),
        can_eat(Subtype, FoodType),
        get_dict(x, Object, X),
        get_dict(y, Object, Y)
    ), Coordinates).

find_carnivore_food(Agents, Subtype, Coordinates) :-
    dict_pairs(Agents, _, AgentPairs),
    findall([X, Y], (
        member(_-Agent, AgentPairs),
        get_subtype(Agent, PreySubtype),
        can_eat(Subtype, PreySubtype),  
        get_dict(x, Agent, X),
        get_dict(y, Agent, Y)
    ), Coordinates).

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
    State = [Agents, _, _, _],  % Decompose the state to get agents.
    dict_pairs(Agents, _, AgentPairs),
    member(AgentId-CurrentAgent, AgentPairs),
    findall(Dist-OtherId, (
        member(OtherId-OtherAgent, AgentPairs),
        OtherId \= AgentId,  % Ensure not to compare the agent with itself.
        agents_distance(CurrentAgent, OtherAgent, Dist)
    ), Distances),
    min_distance(Distances, AgentPairs, Coordinates, NearestAgent),
    !.

% Helper predicate to find the agent with the minimum distance.
min_distance(Distances, AgentPairs, [X, Y], NearestAgent) :-
    sort_list(Distances, [_-NearestId|_]),  % Sort the list of distances and select the closest one.
    member(NearestId-NearestAgent, AgentPairs),
    get_dict(x, NearestAgent, X),  % Retrieve the coordinates of the nearest agent.
    get_dict(y, NearestAgent, Y).
    
% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :-
    State = [Agents, Objects, _, _],
    dict_pairs(Agents, _, AgentPairs),
    member(AgentId-Agent, AgentPairs),
    get_subtype(Agent, Subtype),
    (   Subtype = wolf
    ->  find_nearest_carnivore_food(Agents, AgentId, Subtype, Coordinates, FoodType, Distance)
    ;   find_nearest_herbivore_food(Agents, Objects, AgentId, Subtype, Coordinates, FoodType, Distance)
    ),
    Coordinates \= [],
    !.

% Specialized predicates for carnivores and herbivores to find food.
find_nearest_carnivore_food(Agents, AgentId, Subtype, Coordinates, FoodType, Distance):-
    dict_pairs(Agents, _, AgentPairs),
    member(AgentId-CurrentAgent, AgentPairs),
    findall(Dist-OtherId, (
        member(OtherId-OtherAgent, AgentPairs),
        can_eat(Subtype, OtherAgent.subtype),
        OtherId \= AgentId,
        agents_distance(CurrentAgent, OtherAgent, Dist)
    ), Distances),
    min_food_distance(Distances, AgentPairs, Coordinates, FoodType, Distance).

find_nearest_herbivore_food(Agents, Objects, AgentId, Subtype, Coordinates, FoodType, Distance):-
    dict_pairs(Agents, _, AgentPairs),
    dict_pairs(Objects, _, ObjectPairs),
    member(AgentId-CurrentAgent, AgentPairs),
    findall(Dist-OtherId, (
        member(OtherId-OtherAgent, ObjectPairs),
        can_eat(Subtype, OtherAgent.subtype),
        agents_distance(CurrentAgent, OtherAgent, Dist)
    ), Distances),
    min_food_distance(Distances, ObjectPairs, Coordinates, FoodType, Distance).

% Helper predicate to determine the closest food item and its distance.
min_food_distance(Distances, Pairs, [X, Y], FoodType, Distance) :-
    sort_list(Distances, [MinDist-NearestId|_]),
    member(NearestId-NearestFood, Pairs),
    get_dict(x, NearestFood, X),
    get_dict(y, NearestFood, Y),
    get_dict(subtype, NearestFood, FoodType),
    Distance = MinDist.

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State, AgentId, X, Y, ActionList, MaxDepthLimit) :-
    State = [Agents, _, _, _],
    dict_pairs(Agents, _, AgentPairs),
    memberchk(AgentId-CurrentAgent, AgentPairs),
    get_dict(x, CurrentAgent, CurX),
    get_dict(y, CurrentAgent, CurY),
    (   agent_is_wolf_and_blocked(Agents, X, Y, AgentId)
    ->  fail  
    ;   
    is_surrounded(State, AgentId, X, Y, Surrounded),
    Surrounded = false,  
    iterative_deepening(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, 1, MaxDepthLimit, ActionList)
),
!.

% Checks if a wolf is trying to move to a block occupied by another wolf.
agent_is_wolf_and_blocked(Agents, X, Y, AgentId) :-
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    Subtype = wolf,  
    dict_pairs(Agents, _, Pairs),
    member(_-OtherAgent, Pairs),
    get_dict(subtype, OtherAgent, OtherSubtype),
    OtherSubtype = wolf,
    get_dict(x, OtherAgent, X),
    get_dict(y, OtherAgent, Y).

% Iterative deepening to explore paths with increasing depth limits.
iterative_deepening(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, DepthLimit, MaxDepthLimit, ActionList) :-
    DepthLimit =< MaxDepthLimit,
    path_to_coordinate(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, [], ReverseActionList, DepthLimit),
    reverse_list(ReverseActionList, ActionList).

iterative_deepening(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, DepthLimit, MaxDepthLimit, ActionList) :-
    DepthLimit < MaxDepthLimit,
    NewDepthLimit is DepthLimit + 1,
    iterative_deepening(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, NewDepthLimit, MaxDepthLimit, ActionList).

% Pathfinding for an agent from current to target coordinates, considering depth left.
path_to_coordinate(_, _, _, CurX, CurY, X, Y, CurrentPath, CurrentPath, _) :-
    CurX == X, CurY == Y.

path_to_coordinate(Agents, AgentId, CurrentAgent, CurX, CurY, X, Y, CurrentPath, ActionList, DepthLeft) :-
    DepthLeft > 0,
    get_subtype(CurrentAgent, Subtype),
    move(CurX, CurY, NX, NY, Move, Subtype),
    \+ blocked(NX, NY, Agents, AgentId),
    NewDepth is DepthLeft - 1,
    path_to_coordinate(Agents, AgentId, CurrentAgent, NX, NY, X, Y, [Move|CurrentPath], ActionList, NewDepth).

% Movement options based on agent type and direction.
move(X, Y, NX, NY, Move, Subtype) :-
    (   (Move = move_up, NX = X, NY is Y - 1);
        (Move = move_down, NX = X, NY is Y + 1);
        (Move = move_left, NX is X - 1, NY = Y);
        (Move = move_right, NX is X + 1, NY = Y);
        (Move = move_up_right, NX is X + 1, NY is Y - 1);
        (Move = move_up_left, NX is X - 1, NY is Y - 1);
        (Move = move_down_right, NX is X + 1, NY is Y + 1);
        (Move = move_down_left, NX is X - 1, NY is Y + 1)
    ),
    can_move(Subtype, Move),
    within_bounds([NX, NY]).

% Check if the agent is surrounded based on subtype-specific rules.
is_surrounded(State, AgentId, X, Y, Surrounded) :-
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    (
        (Subtype = cow -> check_cow_surrounded(Agents, X, Y, AgentId, Surrounded));
        (Subtype = chicken -> check_chicken_surrounded(Agents, X, Y, AgentId, Surrounded));
        Surrounded = false 
    ),!.

% Specific checks for items being surrounded for cow.
check_cow_surrounded(Agents, X, Y, AgentId, Surrounded) :-
    UpY is Y - 1,       
    DownY is Y + 1,     
    LeftX is X - 1,     
    RightX is X + 1,    

    (within_bounds([RightX, Y]) -> (blocked(RightX, Y, Agents, AgentId) -> RightBlocked = true; RightBlocked = false); RightBlocked = true),

    (within_bounds([X, DownY]) -> (blocked(X, DownY, Agents, AgentId) -> DownBlocked = true; DownBlocked = false); DownBlocked = true),

    (within_bounds([LeftX, Y]) -> (blocked(LeftX, Y, Agents, AgentId) -> LeftBlocked = true; LeftBlocked = false); LeftBlocked = true),

    (within_bounds([X, UpY]) -> (blocked(X, UpY, Agents, AgentId) -> UpBlocked = true; UpBlocked = false); UpBlocked = true),

    (RightBlocked, DownBlocked, LeftBlocked, UpBlocked -> Surrounded = true; Surrounded = false).

% Specific checks for items being surrounded for chicken.
check_chicken_surrounded(Agents, X, Y, AgentId, Surrounded) :-
    UpY is Y - 1,       
    DownY is Y + 1,     
    LeftX is X - 1,     
    RightX is X + 1,    

    (within_bounds([LeftX, UpY]) -> (blocked(LeftX, UpY, Agents, AgentId) -> ULBlocked = true; ULBlocked = false); ULBlocked = true),

    (within_bounds([RightX, DownY]) -> (blocked(RightX, DownY, Agents, AgentId) -> LRBlocked = true; LRBlocked = false); LRBlocked = true),

    (within_bounds([LeftX, DownY]) -> (blocked(LeftX, DownY, Agents, AgentId) -> LLBlocked = true; LLBlocked = false); LLBlocked = true),

    (within_bounds([RightX, UpY]) -> (blocked(RightX, UpY, Agents, AgentId) -> URBlocked = true; URBlocked = false); URBlocked = true),

    (ULBlocked, LRBlocked, LLBlocked, URBlocked -> Surrounded = true; Surrounded = false).


% Define spatial bounds for movement based on the dimensions of the area.
within_bounds([X, Y]) :-
    width(W), height(H),
    Width is W -1,
    Height is H -1,
    X > 0, X < Width, Y > 0, Y < Height.    

% Check if a position is blocked by another agent (not applicable to wolves).
blocked(X, Y, Agents, AgentId) :-
    dict_pairs(Agents, _, AgentPairs),
    member(AgentId-CurrentAgent, AgentPairs),
    get_subtype(CurrentAgent, Subtype),
    Subtype \= wolf, 
    member(OtherId-OtherAgent, AgentPairs),   
    AgentId \= OtherId,
    get_dict(x, OtherAgent, AX),
    get_dict(y, OtherAgent, AY),
    X == AX, Y == AY.

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
    find_nearest_food(State, AgentId, Coordinates, _, _),  
    Coordinates = [X, Y],  
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit),  
    !. 

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)
consume_all(State, AgentId, NumberOfMovements, Value, NumberOfChildren, DepthLimit) :-
    consume_all_recursive(State, AgentId, 0, 0, DepthLimit, 0, NumberOfMovements, FinalValue, NumberOfChildren, 0, ValueDebt), 
    NumberOfMovements \= 0, 
    Value is FinalValue + ValueDebt.  

% Recursive helper for consume_all, manages state changes, movement and consumption tracking.
consume_all_recursive(State, AgentId, MovesAcc, ChildrenAcc, DepthLimit, DepthAcc, FinalMoves, FinalValue, FinalChildren, DebtAcc, ValueDebt) :-
    DepthAcc < DepthLimit,  
    find_nearest_food(State, AgentId, [X, Y], _, _), 
    (   move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) ->  
        (   apply_action_list(State, AgentId, ActionList, StateAfterMove, MovesAcc, NewMovesAcc), 
            length_list(ActionList, MoveCount), 
            NewDepthAcc is DepthAcc + 1,  
            NewMovesAcc is MovesAcc + MoveCount,  
            eat(StateAfterMove, AgentId, NewState),  
            get_agent_children(NewState, AgentId, NewChildrenAcc), 
            consume_all_recursive(NewState, AgentId, NewMovesAcc, NewChildrenAcc, DepthLimit, NewDepthAcc, FinalMoves, FinalValue, FinalChildren, DebtAcc, ValueDebt)  % Continue recursively.
        )
    ;   find_value_at(State, [X, Y], Debt),  % If unable to move, find the value at the current position.
        remove_from_state(State, [X, Y], NewState),  % Remove the unconsumable resource from the state.
        NewDebtAcc is DebtAcc + Debt,  
        consume_all_recursive(NewState, AgentId, MovesAcc, ChildrenAcc, DepthLimit, DepthAcc, FinalMoves, FinalValue, FinalChildren, NewDebtAcc, ValueDebt)  % Continue recursion with updated debt.
    ),
    !.

% Helper to calculate the value of the farm at the end of consumption.
consume_all_recursive(State, _, MovesAcc, ChildrenAcc, _, _, MovesAcc, FinalValue, ChildrenAcc, DebtAcc, DebtAcc):-
    value_of_farm(State, FinalValue).  

% Retrieve the number of children an agent has, useful for breeding simulations.
get_agent_children(State, AgentId, NumberOfChildren) :-
    State = [Agents, _, _, _], 
    get_dict(AgentId, Agents, Agent), 
    get_dict(children, Agent, NumberOfChildren).  

% Applies a list of actions to move an agent within the state.
apply_action_list(State, _, [], State, MovesAcc, MovesAcc). 
apply_action_list(State, AgentId, [Action|Rest], NewFinalState, MovesAcc, FinalMoveCount) :-
    move(State, AgentId, Action, NewState), 
    NewMoveAcc is MovesAcc + 1,  
    apply_action_list(NewState, AgentId, Rest, NewFinalState, NewMoveAcc, FinalMoveCount).  

% Updates the state by removing an entity at specified coordinates, used after consumption.
remove_from_state(State, Coordinates, ModifiedState) :-
    State = [Agents, Objects, Time, TurnOrder],  
    Coordinates = [X, Y],  
    filter_dictionary(Agents, X, Y, ModifiedAgents), 
    filter_dictionary(Objects, X, Y, ModifiedObjects), 
    ModifiedState = [ModifiedAgents, ModifiedObjects, Time, TurnOrder].  

% Filters dictionary entries by coordinates, removing those that match.
filter_dictionary(Dict, X, Y, ModifiedDict) :-
    dict_pairs(Dict, Tag, Pairs), 
    filter_coordinates(Pairs, X, Y, FilteredPairs), 
    dict_pairs(ModifiedDict, Tag, FilteredPairs).  

% Helper predicate to filter out dictionary pairs that match given coordinates.
filter_coordinates([], _, _, []). 
filter_coordinates([Key-Value|Tail], X, Y, Result) :-
    get_dict(x, Value, XDict),  
    get_dict(y, Value, YDict), 
    (   XDict = X, YDict = Y
    ->  filter_coordinates(Tail, X, Y, Result)  
    ;   Result = [Key-Value|FilteredTail], 
        filter_coordinates(Tail, X, Y, FilteredTail)  
    ).

% Determines the value of an entity at specific coordinates, useful for assessing the value of consumed items.
find_value_at(State, [X, Y], Value) :-
    State = [AgentsDict, ObjectsDict | _],  
    (   find_entity_value_at(AgentsDict, X, Y, Value)  
    ;   find_entity_value_at(ObjectsDict, X, Y, Value) 
    ).

% Helper to find an entity's value based on its coordinates and type.
find_entity_value_at(EntityDict, X, Y, Value) :-
    dict_pairs(EntityDict, _, EntityPairs), 
    find_entity_in_pairs(EntityPairs, X, Y, Subtype), 
    value(Subtype, Value).  

% Identifies an entity in pairs based on coordinates and returns its subtype.
find_entity_in_pairs(EntityPairs, X, Y, Subtype) :-
    member(_-Entity, EntityPairs), 
    get_dict(x, Entity, X), 
    get_dict(y, Entity, Y), 
    get_dict(subtype, Entity, Subtype), 
    !.  

% Calculate the length of a list.
length_list([], 0).  

length_list([_Head|Tail], Length) :-
    length_list(Tail, TailLength),  
    Length is TailLength + 1. 

% Insert an element into a sorted list to maintain sorted order.
insert_sort_list(X, [], [X]).  
insert_sort_list(X, [Y|Sorted], [X,Y|Sorted]) :- X =< Y.  

insert_sort_list(X, [Y|Sorted], [Y|SortedWithX]) :-
    X > Y, 
    insert_sort_list(X, Sorted, SortedWithX).  
    
% Sort a list using the insertion sort algorithm.
sort_list([], []). 

sort_list([Head|Tail], Sorted) :-
    sort_list(Tail, SortedTail), 
    insert_sort_list(Head, SortedTail, Sorted).  

% Calculate the sum of a list of numbers.
sum_list_list([], 0).  

sum_list_list([Head|Tail], TotalSum) :-
    sum_list_list(Tail, SumTail), 
    TotalSum is Head + SumTail.  

% Reverse a list.
reverse_list(InputList, Reversed) :-
    reverse_list_acc(InputList, [], Reversed).  

% Helper predicate for reversing a list using an accumulator.
reverse_list_acc([], Acc, Acc). 

reverse_list_acc([Head|Tail], Acc, Reversed) :-
    reverse_list_acc(Tail, [Head|Acc], Reversed). 

