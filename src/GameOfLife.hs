module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick board =
  [ [ applyRules c (countLiveNeighbors i j)
      | (j, c) <- zip [0 ..] row
    ]
    | (i, row) <- zip [0 ..] board
  ]
  where
    rows = length board
    cols = if null board then 0 else length (head board)

    countLiveNeighbors :: Int -> Int -> Int
    countLiveNeighbors i j =
      length $
        filter (== 1) [board !! x !! y | (x, y) <- validNeighbors i j]
      where
        validNeighbors a b =
          [ (x, y)
            | dx <- [-1, 0, 1],
              let x = a + dx,
              dy <- [-1, 0, 1],
              let y = b + dy,
              (x /= a || y /= b) && x >= 0 && x < rows && y >= 0 && y < cols
          ]

    applyRules :: Int -> Int -> Int
    applyRules c n
      | c == 1 = if n == 2 || n == 3 then 1 else 0 -- Survival rule
      | c == 0 = if n == 3 then 1 else 0 -- Birth rule
      | otherwise = 0 -- Death rule