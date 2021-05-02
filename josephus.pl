
% NumberofSoldiers: the total number of soldiers including Josephus and his accomplice (>2)
% N: The selected number to count down
% J: Output position for Josephus (<NumberSoldiers)
% A: Output position for the accomplice (<NumberSoldiers)

josephus(NumberSoldiers, N, J, A) :-    % J and A are the two surviving spots IF
    make_soldiers(NumberSoldiers, L),   % L is the list of soldiers [1..N]
    K is N + 1,                         % K = N + 1
    death(1, K, L, [J, A]).             % Begining the ritual at the first index results in [J, A]

make_soldiers(X, L) :-                  % L is the list of soldiers [1..N] IF
    make_soldiers1(X, Y),               % Y is the list of soldiers [N..1] AND
    myreverse(Y, L), !.                 % L is the reverse of Y
make_soldiers1(1, [1]).                 % [1] is the reverse list of soldiers if N = 1
make_soldiers1(X, [X|T]) :-             % [X|T] is the reverse list of soldiers if N = X IF
    Y is X - 1,                         % Y = X - 1 AND
    make_soldiers1(Y, T).               % T is the reverse list of soldiers of Y AND

death(_, _, [X, Y], [X, Y]) :- !.                   % [X, Y] are the survivors of the list of soldiers [X, Y]
death(S_i, K, Soldiers, After_soldiers) :-          % After_soldiers is the list of surviving soldiers IF
    deletespot(S_i, K, Soldiers, Next_soldiers),    % Next_soldiers is soldiers after one round of the ritual AND
    newStartIndex(S_i, K, Soldiers, S2),            % S2 is the new start index AND
    death(S2, K, Next_soldiers, After_soldiers).    % After_soldiers it the list of surviving soldiers 
    

% Helper Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deletespot(S_i, K, L, A) :-   % A is L after killing one soldier IF
    S_offset is S_i - 1,      % S_offset = index - 1
    K2 is K + S_offset,       % K2 = K + S_offset
    correctK(K2, L, K3),      % K3 is the correct killspot after adjusting for the offset
    deleteIndex(K3, L, A).    % A is L with index K3 deleted

newStartIndex(S_i, K, L, 1) :-  % 1 is the new start index IF
    S_offset is S_i - 1,        % S_offset = S_i - 1 AND
    K2 is K + S_offset,         % K2 = K + S_offset AND
    correctK(K2, L, K3),        % K3 is the correct killspot after adjusting for the offset AND
    listlength(L, K3), !.       % the killspot is at the end of the list

newStartIndex(S_i, K, L, K3) :- % K3 is the new start index IF
    S_offset is S_i - 1,        % S_offset = S_i - 1 AND
    K2 is K + S_offset,         % K2 = K + S_offset AND
    correctK(K2, L, K3).        % K3 is the killspot AND

deleteIndex(1, [H|T], T) :- !.     % T is [H|T] with the first index deleted
deleteIndex(X, [H|T], [H|A]) :-    % [H|A] is [H|T] with index X deleted IF
    Y is X - 1,                    % Y = X - 1 AND
    deleteIndex(Y, T, A).          % A is T with Y index deleted

correctK(K, L, K) :-            % K is the correct killspot of L IF
    listlength(L, X),           % the listlength of L is X AND
    K =< X, !.                  % K is less than or equal to X
correctK(K, L, X) :-            % X is the correct killspot of L with initial killspot K IF
    listlength(L, Y),           % Y is the list length of L
    N is K - Y,                 % N - K - Y
    correctK(N, L, X).          % X is the correct killspot of L with initial killspot N

% X is a member of Y
% mymember(X, Y).

    mymember(X , [X|Tail]).     % X is a member of [X|Tail]
    mymember(X, [H|Tail]) :-    % X is a member of [H|Tail] IF
        mymember(X, Tail).      % X is a member of Tail

% Y is the reverse of X
% myreverse(X, Y).

    getfirst([X|Tail], X).   % X is first of some list with head of X and a Tail

    getlast([X], X).         % X is last of a list with only X inside
    getlast([H|Tail], X) :-  % X is last of some list [H|T] IF
        getlast(Tail, X).    % X is last of list T

    removefirst([X], []).           % [X] with the first element removed is []
    removefirst([H|Tail], Tail).    % [H|Tail] with the first element removed is Tail

    removelast([X, Y], [X]).            % [X] is the list with the last element deleted of [X, Y]
    removelast([H|Tail], [H|Tail1]) :-  % [H|Tail1] is the list with the last element deleted of [H|Tail] IF
        removelast(Tail, Tail1).        % Tail1 is the list with the last element removed of Tail

    myreverse([X], [X]).            % [X] is the reverse of [X]
    myreverse([X, Y], [Y, X]).      % [Y,X] is the reverse of [X, Y]
    myreverse(X, [H|Tail]) :-       % [H|Tail] is the reverse of X IF
        getlast(X, H),              % H is the last element of X AND
        removelast(X, Y),           % Y is X with its last element removed AND
        myreverse(Y, Tail).         % Tail is the reverse of Y

listlength([], 0).              % length of [] is 0
listlength([H|Tail], X):-       % the length of [H|Tail] is X IF
    listlength(Tail, Y),        % the length of Tail is Y AND
    X is Y+1.                   % X is Y + 1
