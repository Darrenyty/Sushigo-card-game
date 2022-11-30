module Main where

import SushiGoTests
import Testing
import AI
import SushiGo
-- | The list of tests to run. When you define additional test groups,
-- you must list them here or they will not be checked.
--
-- We wrote a number of tests while developing the assignment - they
-- are in SushiGoTests.hs. This leaves AllTests.hs free for your
-- tests. You may want to read them to see how the tests are written,
-- or to get a handle on how the game works.
allTests :: Test
allTests = TestGroup "allTests"
  [ sushiGoTests
    ,getPossibleMoveTest
    ,sushigoTreeHelperTest
    ,generateSushigoTreeTest
    ,miniMoveTest
    ,tellMaxTest
    ,differenceCardsTest
    ,sushigoTreeHelper1Test
    ,generateSushigoTree1Test
    ,enemyMoveTest
    ,enemyCardTest
    ,chopsticksMove1Test
    ,interpretAIFunc1Test
  ]
-- | Test getPossibleMove can get correct card from a list
getPossibleMoveTest :: Test
getPossibleMoveTest = TestGroup "Test getPossibleMove"

  [Test "get the last card from a list"
     (assertEqual (getPossibleMove [Nigiri 3,Eel,Tofu] 3) Tofu )

    , Test "Get the first card from a list"
     (assertEqual (getPossibleMove [Nigiri 1,Sashimi,Tofu] 1) (Nigiri 1) )

    , Test "Get the middle card from a list"
     (assertEqual (getPossibleMove [Nigiri 1,Eel,Tofu] 2 ) Eel )
-- Because when the Int less than 1 or greater than the length of the list
-- and when list is empty will cause an error, so I cannot test them.
     ]

-- | Test for sushigoTreeHelper whether can generate correct tree
sushigoTreeHelperTest :: Test
sushigoTreeHelperTest = TestGroup "Test sushigoTreeHelper"
  [Test "Generate a tree when one of the player's hand is empty"
   (assertEqual (sushigoTreeHelper (GameState (Turn Player1)[][Eel,Chopsticks][Sashimi][])1 3) [])
   -- In actual AI move, it is impossible to just one of player's hand is empty.
   -- Normally, when it is finished, both hands will be empty

   ,Test "Return chopsticks after using and generate correct tree "
   (assertEqual (sushigoTreeHelper
   (GameState (Turn Player1)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])1 2)
   [RoseNode (-3) [RoseNode (-3) [],RoseNode 0 []],RoseNode 3 [RoseNode 3 [],RoseNode 0 []]])

   ,Test "Output [] when the first int is greater than the length of player's cards"
    (assertEqual (sushigoTreeHelper
    (GameState (Turn Player1)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])4 1)[])

   ,Test "Output [] when the depth(secong Int) is zero"
    (assertEqual (sushigoTreeHelper
    (GameState (Turn Player2)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])1 0)[])
    --Because when the turn is finished, there will be an error.
    --Therefore, we can not test the situation.
    ,Test "the different answer of a situation compared to sushigoTreeHelper1 "
           (assertEqual ( sushigoTreeHelper
            (GameState (Turn Player2)
            [Eel,Nigiri 3][Chopsticks]
            [Eel,Eel,Eel][Tofu])1 3)
            [RoseNode (-1) [RoseNode (-1) [RoseNode 9 []],RoseNode 9 [RoseNode 9 []],
            RoseNode 9 []],RoseNode (-1) [RoseNode (-1) [RoseNode 9 []],RoseNode 9
            [RoseNode 9 []],RoseNode 9 []],RoseNode (-1) [RoseNode (-1) [RoseNode 9 []],
            RoseNode 9 [RoseNode 9 []
            ],RoseNode 9 []]])
   ]

-- | Test for generateSushigoTree whether can generate correct tree
generateSushigoTreeTest :: Test
generateSushigoTreeTest = TestGroup "Test generateSushigoTree"
    [ Test "When the turn is Finished"
     (assertEqual (generateSushigoTree
     (GameState (Finished)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 3)[Null])

    ,Test "When the depth is equal to zero"
     (assertEqual (generateSushigoTree
     (GameState (Turn Player1)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 0)[])

    ,Test "When the first int is greater than the length of player's cards"
     (assertEqual (generateSushigoTree
     (GameState (Turn Player2)[Eel,Dumplings][Sashimi][Sashimi,Tofu][Nigiri 1])4 0)[])

    ,Test "When one of the player's hand are empty"
      (assertEqual (generateSushigoTree
      (GameState (Turn Player2)[][Eel,Dumplings,Sashimi][Sashimi,Tofu][Nigiri 1])1 2)[])

    , Test "When the function can generate tree of continuing game"
       (assertEqual (generateSushigoTree
       (GameState (Turn Player1)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 3)
       [RoseNode (-3) [RoseNode (-3) []],RoseNode 3 [RoseNode 3 []]])

    , Test "When the AI's hand contains chopsticks"
       (assertEqual (generateSushigoTree
       (GameState (Turn Player2)
       [Eel,Nigiri 3,Tofu]
       [Sashimi]
       [Sashimi,Chopsticks,Tofu]
       [Nigiri 1])1 2)

       [RoseNode 1 [RoseNode (-2) [],RoseNode 3 []],RoseNode 1
       [RoseNode (-2) [],RoseNode 3 []],RoseNode 3 [RoseNode 0 [],RoseNode 7 []]])

    ,Test "When there are repeated card"
         (assertEqual (generateSushigoTree
          (GameState
          (Turn Player1)
          [Eel,Eel,Eel]
          [Sashimi]
          [Sashimi,Tofu]
          [Nigiri 1])1 3)
          [RoseNode (-3) [RoseNode (-3) []],RoseNode (-3) [],RoseNode (-3) []] )

    ,Test "When there are 4 Tofu in palyer's hand"
         (assertEqual (generateSushigoTree
          (GameState
          (Turn Player1)
          [Tofu,Tofu,Tofu,Eel]
          [Sashimi]
          [Sashimi,Tofu]
          [Nigiri 1])1 3)
          [RoseNode 2 [],RoseNode 2 [],RoseNode 2 [],RoseNode (-3) [RoseNode (-3) []]])
    ,Test "Whether AI will prun when there are other Nigiri after Nigiri 3"
          (assertEqual (generateSushigoTree
          (GameState
          (Turn Player1)
          [Nigiri 3,Nigiri 2]
          [Sashimi,Sashimi]
          [Sashimi,Tofu]
          [Nigiri 1])1 3)
          [RoseNode 3 [RoseNode 5 []],RoseNode 2 []])
    ,Test "the different answer of a situation compared to generateSushigoTree1 "
          (assertEqual ( generateSushigoTree
          (GameState (Turn Player2)
          [Eel,Nigiri 3][]
          [Eel,Eel,Eel][Tofu])1 3)
          [RoseNode (-1) [RoseNode 9 []],RoseNode (-1) [],RoseNode (-1) []])]

-- | Test for miniMove
miniMoveTest :: Test
miniMoveTest = TestGroup "Test for miniMove"
   [ Test "Whether miniMove can prevent the other to get points from Eel"
     (assertEqual (miniMove [Eel,Nigiri 3][][Nigiri 1][Eel])
      Eel)

    , Test "Whether the function can output correct card in common situation"
      (assertEqual (miniMove [Eel,Nigiri 3][Eel][Nigiri 1][Tofu])
      Eel)

    , Test "Whether miniMove can prevent the other to get points from Eel"
      (assertEqual (miniMove [Sashimi,Nigiri 3][][Nigiri 1][Sashimi,Sashimi])
       Sashimi)
   ]

-- | test whether tellMax can correctly find the tuple
tellMaxTest :: Test
tellMaxTest = TestGroup "Test for tellMax"
    [ Test "Whether the function can find the correct tuple"
      (assertEqual (tellMax (-4) 1  [RoseNode 1 [RoseNode 3 []],RoseNode 1 [RoseNode 3
      [RoseNode 3 []]],RoseNode 3 [RoseNode 7 []],RoseNode 3 []]) (7,3))
  --Because the least score of this game is (-4), and tuple always should begin
  -- with 1.Thus, the input should be (-4) 1
  -- Actually using AI, the list cannot be empty, so I didn't test this
  -- In other situations will raise error, so I don't test it.
  ]

-- | Test for removeCard
removeCardTest :: Test
removeCardTest = TestGroup "Test for removeCard"
   [Test "whether it can remove certain card from a list"
   (assertEqual (removeCard [Eel,Nigiri 3] Eel) [Nigiri 3])

   ,Test"When the list is empty"
    (assertEqual (removeCard []Eel)[])
   ]

-- | Test for differenceCard
differenceCardsTest :: Test
differenceCardsTest = TestGroup "Test for differenceCards"
  [Test "whether it can output correct answer of without using of chopsticks"
  (assertEqual (differenceCards (TakeCard Eel)[Eel])10)
  ,Test "whether it can output correct answwer of with using of chopsticks"
  (assertEqual (differenceCards (UseChopsticks Eel (TakeCard Eel))[Nigiri 1]) 7)
  ]

-- | Test for sushigoTreeHelper1
-- Actually the function is nearly same as sushigoTreeHelper, so the tests are nearly same
-- the difference is tested in the last one
sushigoTreeHelper1Test :: Test
sushigoTreeHelper1Test = TestGroup "Test for sushigoTreeHelper1"
     [Test "Generate a tree when one of the player's hand is empty"
      (assertEqual (sushigoTreeHelper1 (GameState (Turn Player1)[][Eel,Chopsticks][Sashimi][])1 3) [])
      -- In actual AI move, it is impossible to just one of player's hand is empty.
      -- Normally, when it is finished, both hands will be empty

      ,Test "Return chopsticks after using and generate correct tree "
      (assertEqual (sushigoTreeHelper1
      (GameState (Turn Player1)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])1 2)
      [RoseNode (-3) [RoseNode (-3) [],RoseNode 0 []],RoseNode 3 [RoseNode 3 [],RoseNode 0 []]])

      ,Test "Output [] when the first int is greater than the length of player's cards"
       (assertEqual (sushigoTreeHelper1
       (GameState (Turn Player1)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])4 1)[])

      ,Test "Output [] when the depth(secong Int) is zero"
       (assertEqual (sushigoTreeHelper1
       (GameState (Turn Player2)[Eel,Nigiri 3][Chopsticks][Sashimi,Tofu][Nigiri 1])1 0)[])
       --Because when the turn is finished, there will be an error.
       --Therefore, we can not test the situation.
       ,Test "the different answer of a situation compared to sushigoTreeHelper "
        (assertEqual ( sushigoTreeHelper1
         (GameState (Turn Player2)
         [Eel,Nigiri 3][Chopsticks]
         [Eel,Eel,Eel][Tofu])1 3)
         [RoseNode (-1) [RoseNode (-1) [RoseNode 2 []],RoseNode 9 [RoseNode 9 []],RoseNode 9 []],
         RoseNode (-1) [RoseNode (-1) [RoseNode 2 []],RoseNode 9 [RoseNode 9 []],RoseNode 9 []],
         RoseNode (-1) [RoseNode (-1) [RoseNode 2 []],RoseNode 9 [RoseNode 9 []
         ],RoseNode 9 []]])
    ]

  -- | Test for generateSushigoTree1 whether can generate correct tree
  -- Most of thefunction is same as generateSushigoTree,
  -- but when input contains 4 Tofu and the last test, there is difference
generateSushigoTree1Test :: Test
generateSushigoTree1Test  = TestGroup "Test generateSushigoTree1"
      [ Test "When the turn is Finished"
       (assertEqual (generateSushigoTree1
       (GameState (Finished)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 3)[Null])

      ,Test "When the depth is equal to zero"
       (assertEqual (generateSushigoTree1
       (GameState (Turn Player1)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 0)[])

      ,Test "When the first int is greater than the length of player's cards"
       (assertEqual (generateSushigoTree1
       (GameState (Turn Player2)[Eel,Dumplings][Sashimi][Sashimi,Tofu][Nigiri 1])4 0)[])

      ,Test "When one of the player's hand are empty"
        (assertEqual (generateSushigoTree1
        (GameState (Turn Player2)[][Eel,Dumplings,Sashimi][Sashimi,Tofu][Nigiri 1])1 2)[])

      , Test "When the function can generate tree of continuing game"
         (assertEqual (generateSushigoTree1
         (GameState (Turn Player1)[Eel,Nigiri 3][Sashimi][Sashimi,Tofu][Nigiri 1])1 3)
         [RoseNode (-3) [RoseNode (-3) []],RoseNode 3 [RoseNode 3 []]])

      , Test "When the AI's hand contains chopsticks"
         (assertEqual (generateSushigoTree1
         (GameState (Turn Player2)
         [Eel,Nigiri 3,Tofu]
         [Sashimi]
         [Sashimi,Chopsticks,Tofu]
         [Nigiri 1])1 2)

         [RoseNode 1 [RoseNode (-2) [],RoseNode 3 []],RoseNode 1
         [RoseNode (-2) [],RoseNode 3 []],RoseNode 3 [RoseNode 0 [],RoseNode 7 []]])

      ,Test "When there are repeated card"
           (assertEqual (generateSushigoTree1
            (GameState
            (Turn Player1)
            [Eel,Eel,Eel]
            [Sashimi]
            [Sashimi,Tofu]
            [Nigiri 1])1 3)
            [RoseNode (-3) [RoseNode (-3) []],RoseNode (-3) [],RoseNode (-3) []] )

      ,Test "When there are 4 Tofu in palyer's hand(different with generatSushigoTree)"
           (assertEqual (generateSushigoTree1
            (GameState
            (Turn Player1)
            [Tofu,Tofu,Tofu,Eel]
            [Sashimi]
            [Sashimi,Tofu]
            [Nigiri 1])1 3)
            [RoseNode 2 [],RoseNode 2 [],RoseNode 2 [],RoseNode (-3) [RoseNode (-1) []]])
      ,Test "Whether AI will prun when there are other Nigiri after Nigiri 3"
            (assertEqual (generateSushigoTree1
            (GameState
            (Turn Player1)
            [Nigiri 3,Nigiri 2]
            [Sashimi,Sashimi]
            [Sashimi,Tofu]
            [Nigiri 1])1 3)
            [RoseNode 3 [RoseNode 5 []],RoseNode 2 []])
      ,Test "the different answer of a situation compared to generateSushigoTree "
            (assertEqual ( generateSushigoTree1
            (GameState (Turn Player2)
            [Eel,Nigiri 3][]
            [Eel,Eel,Eel][Tofu])1 3)
            [RoseNode (-1) [RoseNode 2 []],RoseNode (-1) [],RoseNode (-1) []])
      ]

-- | Test for enemyMove
enemyMoveTest :: Test
enemyMoveTest = TestGroup "Test for enemyMove"
  [Test "whetehr output correct answer for a common input"
    (assertEqual ( enemyMove
    [Eel,Nigiri 3]
    []
    [Eel,Eel]
    [Tofu]) Eel)

  ,Test "whether can prevent opponent get points(Sashimi)"
   (assertEqual ( enemyMove
   [Sashimi,Nigiri 3]
   []
   [Eel]
   [Sashimi,Sashimi])Sashimi)

  ,Test "whether can prevent opponent get points(Eel))"
     (assertEqual ( enemyMove
     [Eel,Nigiri 3]
     []
     [Tofu]
     [Eel])Eel)
   ]

-- | Test for enemyCard
-- Because in actual use, the function will be used in certain situation
-- So the test is simple (unlike to have emptylist)
enemyCardTest :: Test
enemyCardTest = TestGroup "Test for enemyMove"
  [Test "whether can output correct answers with common input"
   (assertEqual ( enemyCard(GameState (Turn Player2)
        [Nigiri 3,Nigiri 1]
        [Eel]
        [Tofu,Eel,Nigiri 3]
        [Nigiri 1])3)Eel)
        ]

-- | Test for chopsticksMove1
chopsticksMove1Test :: Test
chopsticksMove1Test = TestGroup "Test for chopsticksMove1"
   [Test "When UseChopsticks is better"
     (assertEqual ( chopsticksMove1 (GameState (Turn Player1)
      [Eel,Eel,Nigiri 3]
      [Chopsticks]
      [Nigiri 3]
      []) 3) (UseChopsticks Eel (TakeCard Eel)))
      ,Test "When TakeCard is better"
       (assertEqual ( chopsticksMove1 (GameState (Turn Player1)
      [Eel,Nigiri 3]
      [Chopsticks]
      [Nigiri 3]
      []) 3)  (TakeCard (Nigiri 3)))
   ]
-- | Test for interpretAIFunc1
interpretAIFunc1Test :: Test
interpretAIFunc1Test = TestGroup "Test for chopsticks"
    [Test "prevent opponent  to get points(Eel)"
    (assertEqual (interpretAIFunc1 (GameState (Turn Player1)
     [Eel,Nigiri 3]
     []
     [Nigiri 2]
     [Eel] ) 3) (TakeCard Eel))

     ,Test "cards of AI contains Chopsticks"
     (assertEqual (interpretAIFunc1 (GameState (Turn Player1)
     [Eel,Eel,Nigiri 3]
     [Chopsticks]
     [Nigiri 3]      []) 3) (UseChopsticks Eel (TakeCard Eel)))

     ,Test "prevent opponent to get points (Sashimi)"
      (assertEqual (interpretAIFunc1 (GameState (Turn Player1)
      [Sashimi,Nigiri 3]
      []
      [Nigiri 2]
      [Sashimi,Sashimi] ) 3) (TakeCard Sashimi))

      ,Test "input with a common state"
       (assertEqual (interpretAIFunc1 (GameState (Turn Player1)
       [Nigiri 2,Tofu]
       [Nigiri 3]
       [Nigiri 2]
       [Sashimi,Sashimi,Tofu] ) 3) (TakeCard (Nigiri 2)))
       ,Test "prevent opponent  to get points (more than 5 points)"
       (assertEqual (interpretAIFunc1 (GameState (Turn Player2)
       [Nigiri 2]
       [Tofu]
       [Nigiri 2,Tofu]
       [Sashimi,Sashimi,Tofu] ) 3) (TakeCard Tofu))

       ,Test "Whether the bug met before exists"
       -- Because I have met a bug in this situation, so I add this one to test this bug
       (assertEqual (interpretAIFunc1 (GameState (Turn Player2)
       [Sashimi,Sashimi]
       [Tofu,Tofu]
       [Sashimi,Tofu]
       [Tofu,Tofu] ) 3) (TakeCard Sashimi))
   ]
-- | A haskell program starts by running the computation defined by
-- 'main'. We run the tree of tests that we defined above.
main :: IO ()
main = runTests allTests
