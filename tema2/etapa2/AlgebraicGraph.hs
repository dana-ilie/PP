module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node x) = S.insert x S.empty
nodes (Overlay x y) = S.union (nodes x) (nodes y)
nodes (Connect x y) = S.union (nodes x) (nodes y)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node x) = S.empty
edges (Overlay x y) = S.union (edges x) (edges y)
edges (Connect x y) = (edges x) `S.union` (S.cartesianProduct (nodes x) (nodes y)) `S.union` (edges y)
{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors x Empty = S.empty
outNeighbors x (Node y) = S.empty
outNeighbors x (Overlay y z) = S.union (outNeighbors x y) (outNeighbors x z)
outNeighbors x (Connect y z) = if S.member x (nodes y) then (nodes z) `S.union` (outNeighbors x z) else (outNeighbors x y) `S.union` (outNeighbors x z)
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors x Empty = S.empty
inNeighbors x (Node y) = S.empty
inNeighbors x (Overlay y z) = S.union (inNeighbors x y) (inNeighbors x z)
inNeighbors x (Connect y z) = if S.member x (nodes z) then (nodes y) `S.union` (inNeighbors x y) `S.union` (inNeighbors x z) else (inNeighbors x y) `S.union` (inNeighbors x z)
{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode x Empty = Empty
removeNode x (Node y) = if x == y then Empty else (Node y)
removeNode x (Overlay y z) = Overlay (removeNode x y) (removeNode x z)
removeNode x (Connect y z) = Connect (removeNode x y) (removeNode x z)


{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old [] (Node y) = if old == y then Empty else (Node y)
splitNode old news (Node y) = if old == y then add news else (Node y)
splitNode old news (Overlay y z) = Overlay (splitNode old news y) (splitNode old news z)
splitNode old news (Connect y z) = Connect (splitNode old news y) (splitNode old news z)


add [] = Empty
add news = Overlay (Node (head news)) (add (tail news))

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node x) = if prop x then (Node node) else (Node x)
mergeNodes prop node (Overlay x y) = Overlay (mergeNodes prop node x) (mergeNodes prop node y)
mergeNodes prop node (Connect x y) = Connect (mergeNodes prop node x) (mergeNodes prop node y)
