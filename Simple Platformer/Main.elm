import Window
import Keyboard

main = lift2 scene Window.dimensions state
state = foldp update initialState input
input = sampleOn delta <| lift2 (,) delta Keyboard.wasd
delta = fps 50

initialState =
  { p = xywh -10 0 20 20
  , c = (0,10)
  , v = (0,0)
  , bb =
    [ xywh -200 -20 400 20
    , xywh   30  50  70 20
    , xywh -100  50  70 20
    , xywh  -40 120  80 20
    ]
  }

scene (w,h) s = collage w h <|
  let (cx,cy) = s.c
      vw = move (-cx,-cy) in 
  [ box (filled red) s.p |> vw
  ] ++ map (vw << box (filled black)) s.bb

update (dt',{x,y}) s =
  let dt = dt'/1000
      (_,(vx',vy'),p',d) =
        foldr collide (s.p,s.v,translate (scale dt s.v) s.p,False) s.bb
      sx = 150 * toFloat x
      v'' = ( lerp 0.7 vx' sx
            , if y > 0 && d then 450 else vy')
  in  { s
  | p <- p'
  , v <- v'' !+ scale dt (0,-1200)
  , c <- lerp2 0.3 s.c <| center s.p
  }

collide (bx,by,bx',by')
        (((ox,oy,ox',oy') as o,(vx,vy) as v,(nx,ny,nx',ny'),d) as c) =
  if | oy' >= by && oy <= by' && ox' <= bx && nx' > bx ->
        (o,(0,vy),(bx+ox-ox',ny,bx,ny'),d)
     | oy' >= by && oy <= by' && ox >= bx' && nx < bx' ->
        (o,(0,vy),(bx',ny,bx'+ox'-ox,ny'),d)
     | ox' >= bx && ox <= bx' && oy' <= by && ny' > by ->
        (o,(vx,0),(nx,by+oy-oy',nx',by),d)
     | ox' >= bx && ox <= bx' && oy >= by' && ny < by' ->
        (o,(vx,0),(nx,by',nx',by'+oy'-oy),True)
     | otherwise -> c

box style (x,y,x',y') = rect (x'-x) (y'-y)
                     |> style
                     |> move ((x'+x)/2,(y'+y)/2)

lerp f x x' = (1-f)*x + f*x'
lerp2 f (x,y) (x',y') = (lerp f x x', lerp f y y')
scale f (x,y) = (f*x,f*y)
(x,y) !+ (x',y') = (x+x',y+y')
translate (a,b) (x,y,x',y') = (x+a,y+b,x'+a,y'+b)
center (x,y,x',y') = ((x+x')/2,(y+y')/2)
xywh x y w h = (x,y,x+w,y+h)
