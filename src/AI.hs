module AI where

import SushiGo

-- | The type of AI functions. Do not change this.
--
-- The test program will repeatedly call an AI function with
-- increasing lookahead values until it takes too long to generate a
-- result, and use the final result it returns as the "best" move your
-- AI could find.
type AIFunc
  = GameState -- ^ The current game
  -> Int -- ^ How far you should look ahead
  -> Move
data RoseTree a = RoseNode a [RoseTree a] | Null
  deriving (Show,Eq,Ord)


-- | getPossibleMove is used get a certain card in a list
getPossibleMove :: [a]-> Int -> a
getPossibleMove list n = case list of
  [] -> error"It is impossible"
  x:xs
    | n == length (x:xs) -> last (x:xs)
    | n == 1 -> x
    | n < 1 || n> length(x:xs) -> error"It is impossible"
    | otherwise -> getPossibleMove xs (n-1)

-- | sushigoTreeHelper makes part of sushigoTree when list contains chopsticks
sushigoTreeHelper :: GameState ->Int->Int-> [RoseTree Int]
sushigoTreeHelper gamestate@(GameState turn h1 c1 h2 c2) x n = case n of
   0 -> []
   _ -> case turn of
     (Turn Player1)
       | h1 == [] || h2==[] ->[]
       | (x > length h1) -> []
       |otherwise -> (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
                     (generateSushigoTree(GameState turn
                     ([Chopsticks]++(fst(pickCard (getPossibleMove h1 x)h1 c1)))
                     (removeCard(snd(pickCard (getPossibleMove h1 x)h1 c1))Chopsticks)
                     h2
                     c2)
                     1 (n-1)))
                     :sushigoTreeHelper gamestate (x+1) n
     (Turn Player2)
       | h1 == [] || h2==[] ->[]
       | (x > length h2) -> []
       |otherwise -> (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                      (generateSushigoTree(GameState turn
                      h1
                      c1
                      ([Chopsticks]++(fst(pickCard (getPossibleMove h2 x)h2 c2)))
                      (removeCard(snd(pickCard (getPossibleMove h2 x)h2 c2))Chopsticks))
                      1 (n-1)))
                      :sushigoTreeHelper gamestate (x+1) n
     Finished -> error"The game has finished yet"

-- | generateSushigoTree generates a Rose tree for enemy move which gives value to every possible Move.
-- | This tree is nearly the same as tree for player, just a little simple.
-- | If cards contains chopsticks,
-- | the generateSushigoTree will use sushigoTreeHelper to generate part of tree.
-- | And the function also contains some pruning.
-- | 1.When a list has four or more Tofu, Tofu will be pruned.
-- | 2.When the list has repeated elements,
-- |   the element that appears after the first time will be pruned.
-- | 3.When a list has Nigiri 3, Nigiri 2, Nigiri 1,
-- |   Nigiri 1 and  Nigiri 2 will be pruned
-- | 4.When a list has Nigiri  2 and Nigiri 1, Nigiri 1 will be pruned.
generateSushigoTree :: GameState ->Int->Int-> [RoseTree Int]
generateSushigoTree gamestate@(GameState turn h1 c1 h2 c2) x depth = case depth of
  0 -> []
  _ -> case turn of
    Finished-> [Null]
    Turn Player1
       | (x > length h1) -> []
       | h1 ==[] || h2 ==[] -> []
       | (getPossibleMove h1 x) == Tofu &&
         (length (filter (== Tofu) h1)+(length (filter(==Tofu)h2))>=4) ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
         :generateSushigoTree gamestate (x+1) depth
       | filter (== getPossibleMove h1 x) (take (x-1) h1)/= []&& (x/=1) ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
         :generateSushigoTree gamestate (x+1) depth
       | ((getPossibleMove h1 x) == (Nigiri 2 )||(getPossibleMove h1 x) == (Nigiri 1 )) &&
         (filter (==Nigiri 3)h1 /= [])
         &&filter(==Wasabi Nothing) h2 == []->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
         :generateSushigoTree gamestate (x+1) depth
       | (getPossibleMove h1 x) == (Nigiri 1 ) && (filter (==Nigiri 2)h1 /= [])
          &&filter(==Wasabi Nothing) h2 == []->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree gamestate (x+1) depth
       |(getPossibleMove h1 x) == Chopsticks ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
            (sushigoTreeHelper (GameState turn
                (fst(pickCard (miniMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                   (snd(pickCard (getPossibleMove h1 x)h1 c1)))h2 c2))
                (snd(pickCard (getPossibleMove h1 x)h1 c1))
                (fst(pickCard (getPossibleMove h1 x)h1 c1))
                (snd(pickCard (miniMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                   (snd(pickCard (getPossibleMove h1 x)h1 c1)))h2 c2)))
                 1 (depth-1)))
          :generateSushigoTree gamestate (x+1) depth

       | otherwise ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
                       (generateSushigoTree(GameState turn
                       (fst(pickCard (miniMove h2 c2  (fst(pickCard (getPossibleMove h1 x)h1 c1))
                             (snd(pickCard (getPossibleMove h1 x)h1 c1)))h2 c2))
                        (snd(pickCard (getPossibleMove h1 x)h1 c1))
                        (fst(pickCard (getPossibleMove h1 x)h1 c1))
                        (snd(pickCard (miniMove h2 c2  (fst(pickCard (getPossibleMove h1 x)h1 c1))
                              (snd(pickCard (getPossibleMove h1 x)h1 c1)) )h2 c2)))
                         1 (depth-1)))
                     :generateSushigoTree gamestate (x+1) depth
    Turn Player2
       | (x > length h2) -> []
       | h1==[] || h2==[] -> []
       | (getPossibleMove h2 x) == Tofu &&
         (length (filter (== Tofu) h1)+(length (filter(==Tofu)h2))>=4) ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree gamestate (x+1) depth
       | filter (== getPossibleMove h2 x) (take (x-1) h2)/= []&& (x>1) ->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
          :generateSushigoTree gamestate (x+1) depth
       | ((getPossibleMove h2 x) == (Nigiri 2 )||(getPossibleMove h2 x) == (Nigiri 1)) &&
         (filter (==Nigiri 3) h2 /= [])&&filter(==Wasabi Nothing) h1 == [] ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree gamestate (x+1) depth
       | (getPossibleMove h2 x) == (Nigiri 1 ) && (filter (==Nigiri 2) h2 /= [])
         &&(filter(==Wasabi Nothing) h1) == [] ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree gamestate (x+1) depth
       |(getPossibleMove h2 x) == Chopsticks ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
         (sushigoTreeHelper  (GameState turn
         (fst(pickCard (getPossibleMove h2 x)h2 c2))
         (snd (pickCard (miniMove h1 c1 (fst(pickCard (getPossibleMove h2 x)h2 c2))
              (snd(pickCard (getPossibleMove h2 x)h2 c2)))h1 c1))
         (fst(pickCard (miniMove h1 c1  (fst(pickCard (getPossibleMove h2 x)h2 c2))
              (snd(pickCard (getPossibleMove h2 x)h2 c2)))h1 c1))
         (snd(pickCard (getPossibleMove h2 x)h2 c2)))
         1 (depth-1)))
         :generateSushigoTree gamestate (x+1) depth
       | otherwise -> (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                      (generateSushigoTree (GameState turn
                      (fst(pickCard (getPossibleMove h2 x)h2 c2))
                      (snd (pickCard (miniMove h1 c1  (fst(pickCard (getPossibleMove h2 x)h2 c2))
                           (snd(pickCard (getPossibleMove h2 x)h2 c2)))h1 c1))
                      (fst(pickCard (miniMove h1 c1  (fst(pickCard (getPossibleMove h2 x)h2 c2))
                            (snd(pickCard (getPossibleMove h2 x)h2 c2)))h1 c1))
                      (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                      1 (depth-1)))
                     :generateSushigoTree gamestate (x+1) depth

-- | miniMove simply predict move of enemy
-- | It also contains some basic move that enemy use to prevent player get points
miniMove ::  [Card] ->[Card]->[Card]->[Card] -> Card
miniMove l1 l2 l3 l4= case l1 of
  [] -> error"There is no card"
  x:xs
    | mod (length(filter (== Sashimi) l4)) 3 == 2
      && (filter (==Sashimi) l1 == [Sashimi])
      && (filter (==Sashimi) l3 == []) ->  Sashimi
    | filter(==Eel) l4  == [Eel] && (filter (==Eel) l1 == [Eel] &&
       filter (==Eel) l3  == []) ->  Eel
    | otherwise -> helper (x:xs) l2 Eel (-4)
  where helper:: [Card]->[Card]->Card->Int->Card
        helper list list2 y n = case list of
          [] -> y
          x:xs
            | scoreCards (x:list2) > n -> helper xs list2 x (scoreCards (x:list2))
            | scoreCards (x:list2) <= n -> helper xs list2 y n
          _ -> error"The input is wrong"

-- | tellMax can find the biggest value in a certain depth, and tell the best current move
tellMax :: Int-> Int ->[RoseTree Int] -> (Int,Int)
tellMax x n b = case b of
  [] -> (x,n)
  RoseNode y list : ys -> case list of
    []
        | y <= x -> maxHelper (x,n) (tellMax x (n+1)  ys)
      | otherwise -> maxHelper (y,n) (tellMax x (n+1)  ys)
    z:zs -> maxHelper (fst(tellMax x n (z:zs)),n) (tellMax x (n+1) ys)
  _ -> error"The input is wrong"
  where maxHelper :: (Int,Int)->(Int,Int)->(Int,Int)
        maxHelper (a,e) (c,d)
               | a>=c= (a,e)
               | otherwise = (c,d)

-- | removeCard can remove one certain card from a list
removeCard :: [Card]->Card ->[Card]
removeCard list a = case list of
   [] -> list
   x:xs
     | x == a -> xs
     | otherwise -> x: removeCard xs a

-- | differenceCards will calculate the additional points of  a certain move
differenceCards :: Move ->[Card]-> Int
differenceCards move list = case move of
  TakeCard card -> scoreCards (card : list) - scoreCards list
  UseChopsticks card (move1) -> differenceCards move1 (card:list) -scoreCards list + scoreCards (card:list)

------------------------------------------------code for minimax(dead code)
chopsticksMove :: GameState -> Int -> Move
chopsticksMove gs@(GameState (Turn player) h1 c1 h2 c2) x =case player of
  Player1
          | differenceCards (UseChopsticks (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))
            (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h1 c1)))
            (snd (tellMax (-4) 1(generateSushigoTree gs1 1 x)))))) c1 >
            differenceCards ((TakeCard (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x)))))) c1
            ->
            UseChopsticks (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))
            (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h1 c1)))
             (snd (tellMax (-4) 1(generateSushigoTree gs1 1 x)))))
          | otherwise ->TakeCard (getPossibleMove h1
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))
  Player2
           | differenceCards (UseChopsticks (getPossibleMove h2
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))
            (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard (getPossibleMove h2
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h2 c2)))
            (snd (tellMax (-4)1(generateSushigoTree gs2 1 x)))))) c2 >
            differenceCards (TakeCard (getPossibleMove h2
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))) c2
            ->
            UseChopsticks (getPossibleMove h2 (snd
            (tellMax (-4) 1(generateSushigoTree gs 1 x))))
            (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard (getPossibleMove h2
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h2 c2)))
            (snd (tellMax (-4) 1(generateSushigoTree gs2 1 x)))))
            | otherwise ->TakeCard (getPossibleMove h2
            (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))

 where gs1:: GameState
       gs1  = GameState (Turn Player1)
              ((fst(pickCard (getPossibleMove h1
              (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h1 c1))++[Chopsticks])
              (removeCard(snd(pickCard (getPossibleMove h1
              (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h1 c1))Chopsticks)h2 c2
       gs2 :: GameState
       gs2  = GameState (Turn Player2)   h1 c1
              ((fst(pickCard (getPossibleMove h2
              (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h2 c2))++[Chopsticks])
              (removeCard(snd (pickCard (getPossibleMove h2
              (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))h2 c2))Chopsticks)
chopsticksMove (GameState Finished _ _ _ _ ) _ = error"The game is finished"

interpretAIFunc :: AIFunc
interpretAIFunc gs@(GameState turn _ _ _ _) x = case turn of
  Finished -> error"The game is finished"
  Turn player
    | mod (length(filter (== Sashimi) (cardsFor (otherPlayer player) gs))) 3 == 2
      && (filter (==Sashimi) (handFor player gs) == [Sashimi])
      && (filter (==Sashimi)
      (handFor (otherPlayer player)gs) == []) -> TakeCard Sashimi
    | filter(==Eel) (cardsFor (otherPlayer player) gs) == [Eel]
      && (filter (==Eel) (handFor player gs) == [Eel] &&
      filter (==Eel) (handFor (otherPlayer player)gs) == []) -> TakeCard Eel
    | filter (==Chopsticks)(cardsFor player gs)  /= [] -> chopsticksMove gs x
    | otherwise ->TakeCard (getPossibleMove (handFor player gs)
     (snd (tellMax (-4) 1(generateSushigoTree gs 1 x))))

------------------------------------------------code for minimax(dead code)
-- | The table of all AIs you have implemented. We will mark the AI
-- called "default" as your submission, but you may include other AIs
-- for testing.
ais :: [(String, AIFunc)]
ais = [("oldDefault", interpretAIFunc),("basic",firstCard),("default",interpretAIFunc1)]

-- Equivalently: firstLegal :: GameState -> Int -> Move
-- firstLegal simply takes the first card it sees
firstCard :: AIFunc
firstCard state _ = case gameStatus state of
  Turn player -> TakeCard (head (handFor player state))
  _ -> error "firstCard: called on finished game"


-- | sushigoTreeHelper is the helper function of generateSushigoTree
-- |  It is used to finish part of sushigo tree
-- | When the list contains chopsticks, this function will be used

sushigoTreeHelper1 :: GameState ->Int->Int-> [RoseTree Int]
sushigoTreeHelper1 gamestate@(GameState turn h1 c1 h2 c2) x n = case n of
   0 -> []
   _ -> case turn of
     (Turn Player1)
       | h1 == [] || h2==[] ->[]
       | (x > length h1) -> []
       |otherwise -> (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
                     (generateSushigoTree1(GameState turn
                     ([Chopsticks]++(fst(pickCard (getPossibleMove h1 x)h1 c1)))
                     (removeCard(snd(pickCard (getPossibleMove h1 x)h1 c1))Chopsticks)
                      h2
                      c2)
                     1 (n-1)))
                     :sushigoTreeHelper1 gamestate (x+1) n
     (Turn Player2)
       | h1 == [] || h2==[] ->[]
       | (x > length h2) -> []
       |otherwise -> (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                     (generateSushigoTree1(GameState turn
                      h1
                      c1
                     ([Chopsticks]++(fst(pickCard (getPossibleMove h2 x)h2 c2)))
                     (removeCard(snd(pickCard(getPossibleMove h2 x)h2 c2))Chopsticks))
                      1 (n-1)))
                     :sushigoTreeHelper1 gamestate (x+1) n
     Finished -> error" The game is over"

-- | generateSushigoTree1 is used to generate sushigo tree for AI
-- | When the list contains chopsticks,
-- | the function will call for sushigoTreeHelper1 for help
-- | The function also contains some clever pruning and strategy
-- | 1.When there are four or more Tofu in total cards or player has already got 2 Tofu,
-- |   Tofu will be pruned.
-- | 2.When there are five or more Eel in total cards, Eel will be pruned.
-- | 3.When there are seven ore more Sashimi in total cards, Sashimi will be pruned
-- | 4.When the list has repeated elements,
-- |   the element that appears after the first time will be pruned.
-- | 5.When a list has Nigiri 3, Nigiri 2, Nigiri 1, Nigiri 1 and  Nigiri 2 will be pruned
-- | 6.When a list has Nigiri  2 and Nigiri 1, Nigiri 1 will be pruned.

generateSushigoTree1 :: GameState ->Int->Int-> [RoseTree Int]
generateSushigoTree1 gamestate@(GameState turn h1 c1 h2 c2) x depth = case depth of
  0 -> []
  _ -> case turn of
    Finished-> [Null]
    Turn Player1
       | (x > length h1) -> []
       | h1 ==[] || h2 ==[] -> []
       | (getPossibleMove h1 x) == Tofu &&
          (length (filter (== Tofu) h1)+(length (filter(==Tofu)h2))>=4) ->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h1 x) == Tofu &&(length (filter(== Tofu) c1)  ==2)->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h1 x) == Eel
          && (length (filter (== Eel) h1)+(length (filter(==Eel)h2))>=5)  ->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h1 x) == Sashimi &&
          (length (filter (== Sashimi) h1)+(length (filter(==Sashimi)h2))>=7)  ->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | filter (== getPossibleMove h1 x) (take (x-1) h1)/= []&& (x/=1) ->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | ((getPossibleMove h1 x) == (Nigiri 2 )||(getPossibleMove h1 x) == (Nigiri 1 )) &&
          (filter (==Nigiri 3) h1 /= [])&&filter(==Wasabi Nothing) h2 == []
          ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h1 x) == (Nigiri 1 ) &&
          (filter (==Nigiri 2) h1 /= [])
          &&filter(==Wasabi Nothing) h2 == []->
          (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))[])
          :generateSushigoTree1 gamestate (x+1) depth
       |(getPossibleMove h1 x) == Chopsticks ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
         (sushigoTreeHelper (GameState turn
         (fst(pickCard (enemyMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                              (snd(pickCard (getPossibleMove h1 x)h1 c1)) )h2 c2))
         (snd(pickCard (getPossibleMove h1 x)h1 c1))
         (fst(pickCard (getPossibleMove h1 x)h1 c1))
         (snd(pickCard (enemyMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                             (snd(pickCard (getPossibleMove h1 x)h1 c1)) )h2 c2)))
          1 (depth-1)))
          :generateSushigoTree1 gamestate (x+1) depth

       | otherwise ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h1 x)h1 c1)))
                     (generateSushigoTree1(GameState turn
                     (fst(pickCard (enemyMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                                          (snd(pickCard (getPossibleMove h1 x)h1 c1)) )h2 c2))
                     (snd(pickCard (getPossibleMove h1 x)h1 c1))
                     (fst(pickCard (getPossibleMove h1 x)h1 c1))
                     (snd(pickCard (enemyMove h2 c2 (fst(pickCard (getPossibleMove h1 x)h1 c1))
                                         (snd(pickCard (getPossibleMove h1 x)h1 c1)) )h2 c2)))
                      1 (depth-1)))
                     :generateSushigoTree1 gamestate (x+1) depth
    Turn Player2
       | (x > length h2) -> []
       | h1==[] || h2==[] -> []
       | (getPossibleMove h2 x) == Tofu &&
         (length (filter (== Tofu) h1)+(length (filter(==Tofu)h2))>=4)->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h2 x) == Tofu &&(length (filter(== Tofu) c2)  ==2)->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h2 x) == Eel &&
         (length (filter (== Eel) h2)+(length (filter(==Eel)h1))>=5)  ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h2 x) == Sashimi &&
         (length (filter (== Sashimi) h2)+(length (filter(==Sashimi)h1))>=7)
         ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | filter (== getPossibleMove h2 x) (take (x-1) h2)/= []&& (x>1) ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | ((getPossibleMove h2 x) == (Nigiri 2 )||(getPossibleMove h2 x) == (Nigiri 1 )) &&
         (filter (==Nigiri 3) h2 /= [])&&filter(==Wasabi Nothing) h1 == []
          ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       | (getPossibleMove h2 x) == (Nigiri 1 ) &&
         (filter (==Nigiri 2) h2 /= [])
         &&(filter(==Wasabi Nothing) h1) == [] ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))[])
         :generateSushigoTree1 gamestate (x+1) depth
       |(getPossibleMove h2 x) == Chopsticks ->
         (RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
         (sushigoTreeHelper  (GameState turn
         (fst(pickCard (getPossibleMove h2 x)h2 c2))
         (snd (pickCard (enemyMove h1 c1 (fst(pickCard (getPossibleMove h2 x)h2 c2))
                               (snd(pickCard (getPossibleMove h2 x)h2 c2)) )h1 c1))
         (fst(pickCard (enemyMove h1 c1 (fst(pickCard (getPossibleMove h2 x)h2 c2))
                               (snd(pickCard (getPossibleMove h2 x)h2 c2)) )h1 c1))
         (snd(pickCard (getPossibleMove h2 x)h2 c2)))
         1 (depth-1)))
         :generateSushigoTree1 gamestate (x+1) depth
       | otherwise ->(RoseNode (scoreCards (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                     (generateSushigoTree1 (GameState turn
                     (fst(pickCard (getPossibleMove h2 x)h2 c2))
                     (snd (pickCard (enemyMove h1 c1 (fst(pickCard (getPossibleMove h2 x)h2 c2))
                                           (snd(pickCard (getPossibleMove h2 x)h2 c2)) )h1 c1))
                     (fst(pickCard (enemyMove h1 c1 (fst(pickCard (getPossibleMove h2 x)h2 c2))
                                           (snd(pickCard (getPossibleMove h2 x)h2 c2)) )h1 c1))
                     (snd(pickCard (getPossibleMove h2 x)h2 c2)))
                      1 (depth-1)))
                     :generateSushigoTree1 gamestate (x+1) depth

-- | use the generatesushigoTree to predict the enemy move  more accurate than miniMove
enemyMove :: [Card] ->[Card]->[Card]->[Card]->Card
enemyMove l1 l2 l3 l4
 | mod (length(filter (== Sashimi) l4)) 3 == 2
   && (filter (==Sashimi) l1 == [Sashimi])
   && (filter (==Sashimi) l3 == []) =  Sashimi
 | filter(==Eel) l4  == [Eel] && (filter (==Eel) l1 == [Eel] &&
   filter (==Eel) l3  == []) =  Eel
 | otherwise = getPossibleMove l1
   (snd (tellMax (-4) 1 (generateSushigoTree (GameState (Turn Player1) l1 l2 l3 l4 )1 3)))

-- |interpretAIfunc1 can generate the move for certain gamestate
-- | interpretAIfunc1 also contains some different strategy
-- | 1. When AI choose Sashimi or Eel can stop enemy get points in certain conditions, do that.
-- | 2. When AI's card contains chopsticks, the function will call for chopsticksMove
-- | 3. When AI's score is 5 more than enemy, it begins to try to prevent enemy to get points.
interpretAIFunc1 :: AIFunc
interpretAIFunc1 gs@(GameState turn _ _ _ _) depth = case turn of
  Finished -> error"The game is finished"
  Turn player
    | (filter (==Sashimi) (handFor player gs) == [Sashimi])
      && mod (length(filter (== Sashimi) (cardsFor (otherPlayer player) gs))) 3 == 2
      && (filter (==Sashimi) (handFor (otherPlayer player)gs) == []) -> TakeCard Sashimi
    | filter(==Eel) (cardsFor (otherPlayer player) gs) == [Eel] &&
     (filter (==Eel) (handFor player gs) == [Eel] &&
      filter (==Eel) (handFor (otherPlayer player)gs) == []) -> TakeCard Eel
    | filter (==Chopsticks)(cardsFor player gs) /= [] -> chopsticksMove1 gs depth
    | (scoreCards (cardsFor player gs)- scoreCards(cardsFor (otherPlayer player )gs))>5
       &&(length (handFor player gs)> 2) && (length (handFor (otherPlayer player) gs) >2)
       && length(filter(==(enemyCard gs 3))(handFor player gs))==1
       && length (filter(==(enemyCard gs 3))(handFor (otherPlayer player) gs))<=2
       && -(differenceCards (TakeCard (getPossibleMove (handFor player gs)
              (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 depth)))))
              (cardsFor player gs))
       + differenceCards (TakeCard (enemyCard gs 3))(cardsFor player gs)
       < differenceCards  (TakeCard (enemyCard gs 3)) (cardsFor (otherPlayer player) gs)
       - differenceCards (TakeCard  (enemyCard (setHandFor player
              (removeCard (handFor player gs)(enemyCard gs 3)) gs ) 3))
              (cardsFor (otherPlayer player) gs) -> TakeCard (enemyCard gs 3)
    | otherwise ->TakeCard (getPossibleMove (handFor player gs)
                  (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 depth))))

-- | enemyCard is used predict what enemy will get next turn
-- | When AI have certain advantage, it uses the function to predict how to prevent enemy

enemyCard :: GameState ->Int -> Card
enemyCard gs@(GameState turn h1 c1 h2 c2) x = case turn of
   Turn Player1 ->miniMove (fst(pickCard
     (getPossibleMove h1 (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x)))) h1 c1))
     (snd(pickCard (enemyMove h2 c2
     (fst(pickCard (getPossibleMove h1
        (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x)))) h1 c1))
     (snd(pickCard (getPossibleMove  h1
        (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h1 c1) ) ) h2 c2)) h2 c1
   Turn Player2 -> miniMove (fst(pickCard (getPossibleMove h2
      (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x)))) h2 c2))
      (snd(pickCard (enemyMove h1 c1
      (fst(pickCard (getPossibleMove h2
          (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h2 c2 ))
       (snd(pickCard (getPossibleMove h2
           (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x)))) h2 c2))
       )h1 c1)) h1 c2
   Finished -> error"The game is over"

-- |chopsticksMove is used to tell whether to use chopsticks
chopsticksMove1 :: GameState -> Int -> Move
chopsticksMove1 gs@(GameState (Turn player) h1 c1 h2 c2) x =case player of
  Player1
   | differenceCards (UseChopsticks (getPossibleMove h1
      (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))
   (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard
      (getPossibleMove h1 (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))h1 c1)))
      (snd (tellMax (-4) 1(sushigoTreeHelper1 gs1 1 x)))))) c1 >
    differenceCards ((TakeCard (getPossibleMove h1
      (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x)))))) c1
     ->
    UseChopsticks (getPossibleMove h1 (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))
    (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard (getPossibleMove h1
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))h1 c1)))
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs1 1 x)))))
    | otherwise ->TakeCard (getPossibleMove h1
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))
  Player2
    | differenceCards (UseChopsticks (getPossibleMove h2
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))
     (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard
        (getPossibleMove h2 (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))h2 c2)))
        (snd (tellMax (-4)1(sushigoTreeHelper1 gs2 1 x)))))) c2 >
     differenceCards (TakeCard (getPossibleMove h2
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))) c2
     ->
     UseChopsticks (getPossibleMove h2 (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))
     (TakeCard (getPossibleMove  ([Chopsticks]++(fst(pickCard
        (getPossibleMove h2 (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))h2 c2)))
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs2 1 x)))))
     | otherwise ->TakeCard (getPossibleMove h2
        (snd (tellMax (-4) 1(sushigoTreeHelper1 gs 1 x))))

 where gs1:: GameState
       gs1  = GameState (Turn Player1)
              ([Chopsticks]++(fst(pickCard (getPossibleMove h1
                (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h1 c1)))
              (removeCard(snd(pickCard (getPossibleMove h1
                (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h1 c1))Chopsticks)
              h2
              c2
       gs2 :: GameState
       gs2  = GameState (Turn Player2)   h1 c1
              ([Chopsticks]++(fst(pickCard
              (getPossibleMove h2 (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h2 c2)))
              (removeCard(snd (pickCard (getPossibleMove h2
              (snd (tellMax (-4) 1(generateSushigoTree1 gs 1 x))))h2 c2))Chopsticks)
chopsticksMove1 (GameState Finished _ _ _ _ ) _ = error"The game is finished"