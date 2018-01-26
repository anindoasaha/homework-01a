{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

{- Author: Anindo Ashim Saha -}

import CodeWorld

-- Replace with exercise2, exercise3 as necessary
main :: IO ()
main = exercise1

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0 2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

data Phase = LongGreen | ShortAmber | LongRed | ShortRedAmber

trafficLight :: Phase -> Picture
trafficLight LongGreen  = botCircle green & midCircle black & topCircle black & frame
trafficLight ShortAmber  = botCircle black & midCircle yellow & topCircle black & frame
trafficLight LongRed  = botCircle black & midCircle black & topCircle red & frame
trafficLight ShortRedAmber = botCircle black & midCircle yellow & topCircle red & frame

trafficController :: Integer -> Picture
trafficController t
  | t >= 0 && t < 4  = trafficLight LongGreen
  | t >= 4 && t < 5  = trafficLight ShortAmber
  | t >= 5 && t < 9  = trafficLight LongRed
  | otherwise        = trafficLight ShortRedAmber

trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 10)

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree f 0 = f
tree f n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree f (n-1)) & rotated (- pi/10) (tree f (n-1)))

drawBlossom :: Double -> Picture
drawBlossom t = colored yellow (solidCircle ((min 10 t)/35))

blossomTree :: Double -> Picture
blossomTree t = (tree (drawBlossom t) 8)

exercise2 :: IO ()
exercise2 = animationOf blossomTree

-- Exercise 3
wall, ground, storage, box :: Picture
wall =    colored (grey 0.5) (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored black (solidCircle 0.25) & ground
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

drawColumn ::  Integer -> Integer -> Integer -> Picture
drawColumn r cl ch
  | cl > ch = blank
  | otherwise = translated (fromIntegral r) (fromIntegral cl) (drawTile (maze r cl)) & drawColumn r (cl+1) ch

drawRow :: Integer -> Integer -> Picture
drawRow rl rh
  | rl > rh = blank
  | otherwise = drawColumn rl (-10) 10 & (drawRow (rl+1) rh)

pictureOfMaze :: Picture
pictureOfMaze = drawRow (-10) 10

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
