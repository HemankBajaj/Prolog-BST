% Hemank Bajaj
% 2020CS10349

% integer binary tree predicate
ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

% trivial but helpful predicates
% finding max(A,B)
maxim(A,B,A) :- A>=B.
maxim(A,B,B) :- B>A.
% join two lists X and Y into XY
join([], Y, Y).
join([X|Xs],Ys,[X|Z]) :- join(Xs,Ys,Z).
% Absolute Value of a number
abs(X, X):- X >= 0.
abs(X, Y) :- X < 0, Y is 0-X.
% to check if a list is sorted
isSorted([]).
isSorted([_]).
isSorted([X|Xs]):- [Y|_] = Xs, X =< Y , isSorted(Xs).
% to get the first N elements in a list
firstN(0,_, []).
firstN(N, [X|Xs], [X|Ls]) :- Nminus1 is N-1, firstN(Nminus1, Xs, Ls). 
% node list to int list
extract([], []).
extract([node(N, _, _)|L], [N|F]) :- extract(L, F).


% size of the binary tree
size(empty, 0).
size(node(_,L,R),M) :- size(L, K) , size(R, T), M is K+T+1.

% height of the binary tree
height(empty, 0).
height(node(_, L, R), H) :- height(L, L1) ,  height(R, R1), maxim(L1, R1, X), H is X+1.

% preorder traversal
preorder(empty, []).
preorder(node(N,L,R), Pre) :- preorder(L, A), preorder(R, B), join([N|A], B, Z), Pre=Z.

% postorder traversal
postorder(empty, []).
postorder(node(N,L,R), Po) :- postorder(L,A), postorder(R,B), join(A,B, Z),join(Z,[N], T), Po = T.

% inorder traversal
inorder(empty,[]).
inorder(node(N,L,R), In) :- inorder(L,A) , inorder(R, B) , join(A,[N|B], Z), In = Z.

% Preorder Tail Recursive
% helper function that finds the preorder tail recursilvely.
preorder_util(X, Y, []) :- X = Y.
preorder_util(X,Pre,[empty|Stk]) :- preorder_util(X,Pre,Stk).
preorder_util(X,Pre , [Top|Stk]) :- node(N,L,R) = Top, join(Pre, [N], NewPre),join([L,R], Stk,NewStk),preorder_util(X,NewPre, NewStk).

trPreorder(empty, []).
trPreorder(node(N, L, R), Pre) :- preorder_util(X,[N],[L,R]), Pre = X.

% Inorder Tail Recursive
% helper function that finds the inorder tail recursilvely.
inorder_util(X, In,[]) :- In = X.
inorder_util(X, In, [empty|Stk]) :- inorder_util(X, In, Stk).
inorder_util(X, In, [Top|Stk]) :- integer(Top), inorder_util(X, [Top|In],Stk).
inorder_util(X, In, [Top|Stk]) :- ibt(Top), node(N, L, R) = Top, inorder_util(X, In, [R, N ,L|Stk]).

trInorder(empty, []).
trInorder(node(N, L, R), In) :- inorder_util(X, [], [node(N, L, R)]), In = X.

% Postorder Tail Recursive
% helper function that finds the postorder tail recursilvely.
postorder_util(X,Post,[]) :- Post = X.
postorder_util(X, Post, [empty|Stk]) :- postorder_util(X, Post, Stk).
postorder_util(X, Post, [Top|Stk]) :- node(N,L,R) = Top, NewPost  = [N|Post], NewStk = [R,L| Stk] , postorder_util(X, NewPost, NewStk).

trPostorder(empty, []).
trPostorder(node(N, L, R), Post) :- postorder_util(X, [], [node(N,L,R)]), Post = X.

% Euler Tour
eulerTour(empty, []).
eulerTour(node(N, L, R), Eu) :- eulerTour(L, Eu1), eulerTour(R, Eu2), join(Eu1, [N], X1), join(Eu2, [N], X2), join([N|X1], X2, Z), Z = Eu.

% Euler Tour Helper Functions
etUtil([], L1, L2, L3, X1, X2, X3):- X1 = L1, X2 = L2, X3 = L3.
etUtil([X|Xs], L1, L2, L3,X1,X2, X3):- \+ member(X, L3), member(X, L1), member(X, L2), etUtil(Xs, L1, L2, [X|L3], X1, X2, X3).
etUtil([X|Xs], L1, L2, L3,X1,X2, X3):- \+ member(X, L2),member(X, L1), etUtil(Xs, L1, [X|L2], L3, X1, X2, X3).
etUtil([X|Xs], L1, L2, L3,X1,X2, X3):- \+ member(X,L1),etUtil(Xs, [X|L1], L2, L3, X1, X2, X3).

eulerTourDist(empty, []).
eulerTourDist(node(N, L, R), Eu) :- eulerTourDist(L, Eu1), eulerTourDist(R, Eu2), join(Eu1, [node(N,L,R)], X1), join(Eu2, [node(N, L, R)], X2), join([node(N, L, R)|X1], X2, Z), Z = Eu.

% preorder from euler tour
preET(empty, []).
preET(node(N,L,R), Pre):-  eulerTourDist(node(N,L,R), Eu), etUtil(Eu, [],[],[],L1,_, _), reverse(L1, P),extract(P, P1), Pre = P1.

% inorder from euler tour
inET(empty, []).
inET(node(N, L, R), In) :- eulerTourDist(node(N,L,R), Eu), etUtil(Eu, [],[],[],_,L2, _), reverse(L2, P),extract(P, P1), In = P1.

% postorder from euler tour
postET(empty, []).
postET(node(N, L, R), Post) :- eulerTourDist(node(N,L,R), Eu), etUtil(Eu, [],[],[],_,_,L3), reverse(L3, P), extract(P, P1),Post = P1.

% To String
listToString(L, S) :- maplist(atom_chars,L, Ls),append(Ls, List),atom_chars(S, List).  % joins a list of strings into a single string
toString(empty, "()").
toString(node(N,L,R),S):-number_string(N,Sn),toString(L,SL), toString(R, SR), listToString(["(",Sn, ", ",SL, ", ",SR, ")"], X),X=S.

% predicate to check if a binary tree is balanced or not
isBalanced(empty).
isBalanced(node(_,L,R)) :- height(L,H1), height(R, H2), A is H1-H2, abs(A,X), X =< 1.

% is BST
isBST(BT):- inorder(BT, In), isSorted(In). 

% lookup BST
lookup(N, node(N0,_,_)) :- N = N0.
lookup(N, node(N0,_,R)) :- N>N0 , lookup(N, R).
lookup(N, node(N0,L,_)) :- N<N0, lookup(N, L).

% make BST
makeBST([], empty).
makeBST(ULs, node(N, L, R)) :- sort(ULs,Ls) ,length(Ls, Len), Nby2 is Len//2, nth0(Nby2, Ls,X1), N is X1,  firstN(Nby2,Ls, L1), reverse(Ls, RL), RLen is Len-1-Nby2, firstN(RLen, RL,RL2), reverse(RL2, L2), makeBST(L1, L), makeBST(L2, R). 

% insert in BST
insert(N, empty, node(N, empty, empty)).
insert(N, node(N0, L, R), node(N0, L1, R)) :- N =< N0 , insert(N, L, L1).
insert(N, node(N0, L, R), node(N0, L, R1)) :- N > N0 , insert(N, R, R1).

% delete bst
delete_minNode(node(N, empty, R), R, N).
delete_minNode(node(N, L, R), node(N, L1, R), Min) :- delete_minNode(L, L1, Min).

delete(N, node(N, L, empty), L).
delete(N, node(N, empty, R), R).
delete(N, node(N, L, R), node(Min, L, R1)) :- delete_minNode(R, R1, Min).
delete(N, node(N0, L, R), node(N0,L1,R)) :- N<N0, delete(N, L, L1).
delete(N, node(N0, L, R), node(N0, L,R1)) :- N>N0, delete(N,R, R1).


% Test Cases
tree0(empty).
tree1(node(1, empty, empty)).
tree2(node(1,node(2,empty,empty),node(3,node(3,empty,empty),empty))).
tree3(
    node(
        1,
        node(
            2, 
            node(
                4,empty,empty
            ),
            node(
                5,
                node(
                    7,empty,empty
                ),
                empty
            )
        ),
        node(
            3,
            node(8,empty, empty),
            empty
        )
    )
).