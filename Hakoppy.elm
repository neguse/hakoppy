import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Time exposing (..)
import Window
import List
import Text
import Signal exposing (..)
import Random

{-

screen coordinate

(0,100)    (100,100)
------------
|          |
|          |
|          |
|          |
|          |
|          |
------------
(0,0)      (100,0)

-}


imagW = 100
imagH = 100


-- MODEL

type alias Object = 
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , color : Color
    }

type alias MovingObj o =
    { o
    | vx : Float
    , vy : Float
    }

type alias HakoObj o =
    { o | hopping : Bool, dead : Bool, deadColor : Color }

type alias Hako =
    HakoObj (MovingObj Object)

type alias ThroughObj o =
    { o | throughed : Bool }

type alias Pipe =
    ThroughObj (MovingObj Object)

type alias Area =
    Object

type alias Ground =
    Object

type State = Init | Running

type alias Model =
    { hako : Hako
    , pipes : List Pipe
    , gen : Float
    , state : State
    , area : Area
    , ground : Ground
    , score : Int
    , seed : Random.Seed
    }

type alias Inputs =
    { delta : Float
    , keys : Bool
    }


groundY = imagH * 0.1


initModel : Model
initModel =
    { hako = initHako
    , pipes = []
    , area = initArea
    , ground = initGround
    , gen = initGen
    , state = Init
    , score = 0
    , seed = Random.initialSeed 29305
    }


initHako : Hako
initHako =
  { x = 20
  , y = imagH / 2
  , vx = 0
  , vy = 0
  , w = 5
  , h = 5
  , color = (rgb 255 255 255)
  , deadColor = (rgb 127 127 127)
  , hopping = False
  , dead = False
  }


initPipe : Int -> List Pipe
initPipe dr =
    let p = { x = imagW
        , y = 0
        , vx = -0.8
        , vy = 0
        , w = 15
        , h = 0
        , color = (rgb 0 255 0)
        , throughed = False
        }
        centerY = (toFloat dr) * 2.0 + imagH / 2
        pipeD = 15
    in
        [
            {p | y <- 0, h <- centerY - pipeD, throughed <- True }, 
            {p | y <- centerY + pipeD, h <- imagH - (centerY + pipeD)  }
        ]


initArea : Area
initArea = 
  { x = 0
  , y = groundY
  , w = imagW
  , h = imagH - groundY
  , color = (rgb 174 238 238)
  }


initGround : Ground
initGround = 
  { x = 0
  , y = 0
  , w = imagW
  , h = imagH * 0.1
  , color = (rgb 74 167 43)
  }


initGen = 100


-- UPDATE

updateInit : Inputs -> Model -> Model
updateInit inputs model =
    if inputs.keys then
       { model | state <- Running, hako <- hop True model.hako }
   else model


updateRunning : Inputs -> Model -> Model
updateRunning inputs model =
    let
        gen' = model.gen - inputs.delta
        (rand, seed') = Random.generate (Random.int -10 10) model.seed
    in let
        hako' = updateHako inputs model.ground model.hako
        pipes' = if not model.hako.dead then
                   (updatePipes inputs.delta model.pipes) ++ (generatePipes gen' rand)
                   else model.pipes
    in let
       hako'' = collide pipes' model.area hako'
       (pipes'', scoreAcc) = (throughPipe pipes' hako')
   in let
       model' = { model |
            hako <- hako''
        ,   pipes <- pipes''
        ,   gen <- if gen' < 0 then initGen else gen'
        ,   score <- model.score + scoreAcc
        ,   seed <- seed'
        }
      in
         if model'.hako.dead && inputs.keys then initModel else model'



throughPipe : List Pipe -> Hako -> (List Pipe, Int)
throughPipe pipes hako =
    case pipes of
        [] ->
            ([], 0)

        p::xs ->
            let throughed = not p.throughed && hako.x > p.x
            in let
                (pipes', scoreAdd') = (throughPipe xs hako)
                p' = { p | throughed <- p.throughed || throughed }
            in
               (p' :: pipes', scoreAdd' + if throughed then 1 else 0)

update : Inputs -> Model -> Model
update inputs model =
    case model.state of
        Init ->
            updateInit inputs model

        Running ->
            updateRunning inputs model


updateHako : Inputs -> Ground -> Hako -> Hako
updateHako inputs ground hako =
  hako
    |> upperGround ground
    |> gravity inputs.delta
    |> hop inputs.keys
    |> physics inputs.delta


hop : Bool -> Hako -> Hako
hop keys hako =
  if keys && not hako.hopping && not hako.dead then
      { hako | vy <- 2, hopping <- True }

  else if not keys && hako.hopping then
      { hako | hopping <- False }

  else
      hako

gravity : Float -> Hako -> Hako
gravity dt hako =
  { hako |
      vy <- if hako.y > 0 then hako.vy - dt/9 else 0
  }


physics dt o =
  { o |
    x <- o.x + dt * o.vx,
    y <- o.y + dt * o.vy
  }


upperGround g o =
    { o | y <- if o.y < g.y + g.h then g.y + g.h else o.y }


isInside : Pipe -> Bool
isInside pipe =
    -pipe.w < pipe.x

updatePipe : Float -> Pipe -> Pipe
updatePipe dt pipe =
    pipe
      |> physics dt

updatePipes : Float -> List Pipe -> List Pipe
updatePipes dt pipes =
    List.filter isInside
    (List.map (updatePipe dt) pipes)


generatePipes : Float -> Int -> List Pipe
generatePipes gen rand =
    if gen < 0 then
       initPipe rand
    else
        []


collideObj o1 o2 =
    let
        o1l = o1.x
        o1r = o1.x + o1.w
        o1b = o1.y
        o1t = o1.y + o1.h
        o2l = o2.x
        o2r = o2.x + o2.w
        o2b = o2.y
        o2t = o2.y + o2.h
    in
       o2l < o1r
       && o1l < o2r
       && o1b < o2t
       && o2b < o1t


collide : List Pipe -> Area -> Hako -> Hako
collide pipes area hako =
    let
        collided = (List.any (collideObj hako) pipes) || not (collideObj hako area)
    in
        if not hako.dead && collided then
       { hako | dead <- True, color <- hako.deadColor, vy <- 0.0 }
       else hako

-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') { hako, pipes, area, ground, score, state } =
  let
    (w,h) = (toFloat w', toFloat h')
  in let
    (sw,sh) = (w/imagW, h/imagH)
  in let
    s = min sw sh
  in let

    viewObject o =
        rect o.w o.h
        |> filled o.color
        |> move (s * (o.x + o.w / 2 - imagW / 2), s * (o.y + o.h / 2 - imagH / 2))
        |> scale s

  in let

    viewHako hako =
        viewObject hako
          |> rotate (hakoRot hako.vy)

    spw = (w - s * imagW) / 2
    sph = (h - s * imagH) / 2

    msg =
        case state of 
            Init -> show "Click to Start"
            Running -> empty

    scoreT =
        case state of 
            Init -> empty
            Running -> show score

  in
    collage w' h'
      ([ viewObject area
      , viewObject ground
      ] ++ List.map viewObject pipes
      ++ [viewHako hako
      ,   toForm scoreT
      ,   toForm msg
            |> move (0, 30)
      ,   rect spw h
            |> filled black
            |> move ((spw/2-w/2), 0)
      ,   rect spw h
            |> filled black
            |> move ((-spw/2+w/2), 0)
      ,   rect w sph 
            |> filled black
            |> move (0, (sph/2-h/2))
      ,   rect w sph 
            |> filled black
            |> move (0, (-sph/2+h/2))
      ])


hakoRot : Float -> Float
hakoRot vy = vy * 0.1


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update initModel input)

type alias Inputs2 =
    { delta : Float
    , keys : Bool
    , prevKeys : Bool
    }

keyPressure : Inputs -> Inputs2 -> Inputs2
keyPressure a s =
    { delta = a.delta, keys = a.keys, prevKeys = s.keys }

keyPressured : Inputs2 -> Inputs
keyPressured s =
    { delta = s.delta, keys = s.keys }

initInputs : Inputs2
initInputs = 
    { delta = 0
    , keys = False
    , prevKeys = False
    }

input : Signal Inputs
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.map keyPressured (Signal.foldp keyPressure initInputs (Signal.sampleOn delta (Inputs <~ delta ~ Mouse.isDown)))

