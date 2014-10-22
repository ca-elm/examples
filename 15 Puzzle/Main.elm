import Window
import Keyboard

main = lift2 scene Window.dimensions state
state = foldp update initialState input
input = sampleOn Keyboard.lastPressed Keyboard.arrows

initialState : [[Int]]
initialState =
  [ [ 9,1, 3,11]
  , [ 6,8,14, 0]
  , [ 5,2,10, 4]
  , [13,7,12,15]]

scene (w,h) s = container w h middle <| foldl1 below <| map row s
row l = foldr1 beside <| map tile l
tile x = case x of
  0 -> spacer 50 50 |> color darkGray
  i -> container 50 50 middle (plainText <| show i)
    |> color (if i % 2 == 0 then gray else lightGray)

swapLeft s = map swapLeft' s
swapLeft' l = case l of
  0::x::xs -> x::0::xs
  x::xs -> x::swapLeft' xs
  [] -> []

swapRight s = map swapRight' s
swapRight' l = case l of
  x::0::xs -> 0::x::xs
  x::xs -> x::swapRight' xs
  [] -> []
  
swapUp = transpose << swapLeft << transpose
swapDown = transpose << swapRight << transpose

transpose : [[a]] -> [[a]]
transpose l = case head l of
  [] -> []
  _  -> map head l :: transpose (map tail l)

update : {x:Int,y:Int} -> [[Int]] -> [[Int]]
update {x,y} s = case (x,y) of
  (1,0) -> swapRight s
  (-1,0) -> swapLeft s
  (0,1) -> swapUp s
  (0,-1) -> swapDown s
  _ -> s
