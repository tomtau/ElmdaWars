module Main where

import Time
import Window
import Automaton (Automaton, hiddenState, step, pure)

-- rules
-- |Turn rate in degrees per tick
turnRate : Float
turnRate = 5

-- |Radar turn rate in degrees per tick
radarTurnRate : Float
radarTurnRate  = 15

-- |Turret turn rate in degrees per tick
turretTurnRate : Float
turretTurnRate = 10

-- |Max speed in pixles per tick
maxSpeed       = 3

-- |Max acceleration in pixels per tick per tick
maxAcceleration : Float
maxAcceleration = 0.4

-- |Max deceleration in pixels per tick per tick
maxDeceleration : Float
maxDeceleration = 1

-- core
type BoundingBox = {
  minX  : Int,
  minY  : Int,
  maxX  : Int,
  maxY  : Int
}

type World  = {
   worldBots    : [(Automaton DashBoard Command, BotState)],
   worldBullets : [Bullet],
   worldBox     : BoundingBox
}

defaultBox = {minX = 50, minY = 50, maxX = 500, maxY = 500}
defaultWorld = {worldBots = [(makeBot "Retardon" 150 150 circleBot),(makeBot "Rammer" 100 100 rammingBot),(makeBot "TurretTest" -50 -50 sittingDuck)], worldBullets = [], worldBox = defaultBox}

type Point = { x:Float, y:Float }
toPoint (x,y) = {x=x,y=y}
type Direction = Point
zeroPoint = {x=0, y=0}
type Degree = Float

data Command = NoAction
              | Turn Degree
              | Accelerate Float
              | Decelerate Float
              | MoveTurret Degree
              | MoveRadar Degree
              | Fire

type Bullet = {
  bulletPosition  : Point,
  bulletVelocity  : Point
}

type DashBoard = {
--    dashRadar     : ScanResult
  dashWallHit   : Bool,--dashWallHit   : Collision,
  dashVelocity  : Point
  }

type BotState = {
  botName     : String,
  botPosition : Point,
  botAngle : Float,
  botVelocity : Point,
  botTurret   : Direction,
  botRadar    : Direction,
  botLastCmd  : Command -- ^ useful for logging
}
-- hiddenState : s -> (i -> s -> (s,o)) -> Automaton i o
circleBotStep : DashBoard -> Bool -> (Command,Bool)
circleBotStep _ lastAcc = if lastAcc then (Turn 5,False) else (Accelerate 1,True)

circleBot : Automaton DashBoard Command
circleBot = hiddenState False circleBotStep

fireBot : Automaton DashBoard Command
fireBot = pure (\_ -> Fire)

sittingDuck : Automaton DashBoard Command
sittingDuck = pure (\_ -> MoveTurret 1)

rammingBot : Automaton DashBoard Command
rammingBot = pure (\_ -> Accelerate 0.01)

-- tick :: Signal Time
tick = fps 30
--Automaton DashBoard Command
makeBot : String -> Float -> Float -> Automaton DashBoard Command -> (Automaton DashBoard Command,BotState)
makeBot name x y program = (program,{botName = name,
  botPosition = {x=x,y=y},
  botAngle = 0,
  botVelocity = zeroPoint,
  botTurret = {x=1,y=0},
  botRadar = zeroPoint,
  botLastCmd = NoAction})

updateBotPosition : BoundingBox -> BotState -> BotState
updateBotPosition bbox b =
  let newX = clamp (-(toFloat bbox.maxX / 2) + toFloat bbox.minX) (toFloat bbox.maxX / 2 - toFloat bbox.minX) (b.botPosition.x + b.botVelocity.x)
      newY = clamp (-(toFloat bbox.maxY / 2) + toFloat bbox.minY) (toFloat bbox.maxY / 2 - toFloat bbox.minY) (b.botPosition.y + b.botVelocity.y)
  in {b | botPosition <- {x=newX,y=newY}}

updateBotSpeed : Float -> Float -> Float -> BotState -> BotState
updateBotSpeed acc maxAcc maxSpeed b =
  let newAcc = clamp (-maxAcc) maxAcc acc
      (r,theta) = toPolar (b.botVelocity.x,b.botVelocity.y)
      newSpeed = clamp 0 maxSpeed (r+newAcc)
      carts = fromPolar (newSpeed,theta) |> toPoint
  in
    {b | botVelocity <- carts}

updateBotDir : Float -> Float -> BotState -> BotState
updateBotDir degTurn maxDeg b =
  let newDeg = clamp (-maxDeg) maxDeg degTurn
      newTurn = radians newDeg
      (r,theta) = toPolar (b.botVelocity.x,b.botVelocity.y)
      carts = fromPolar (r,theta + newTurn) |> toPoint
  in
    {b | botVelocity <- carts, botAngle <- (theta + newTurn)}

updateTurrDir : Float -> Float -> BotState -> BotState
updateTurrDir degTurn maxDeg b =
  let newDeg = clamp (-maxDeg) maxDeg degTurn
      newTurn = radians newDeg
      (r,theta) = toPolar (b.botTurret.x,b.botTurret.y)
      carts = fromPolar (r,theta + newTurn) |> toPoint
  in
    {b | botTurret <- carts}

near k c n = n >= k-c && n <= k+c

stepBot : World -> (Automaton DashBoard Command,BotState) -> (Automaton DashBoard Command,BotState)
stepBot w (a,b) =
  let hitwall = False--(near b.botPosition.x w.worldBox.maxX 8) || (near b.botPosition.x w.worldBox.minX 8) || (near b.botPosition.y w.worldBox.maxY 8) || (near b.botPosition.y w.worldBox.minY 8)
      (newA, cmd) = step {dashVelocity=b.botVelocity,dashWallHit=hitwall} a
  in case cmd of NoAction -> (newA,updateBotPosition w.worldBox b)
                 Turn d -> (newA, updateBotDir d turnRate b |> updateBotPosition w.worldBox)
                 Accelerate x -> (newA, updateBotSpeed x maxAcceleration maxSpeed b |> updateBotPosition w.worldBox)
                 MoveTurret d -> (newA, updateTurrDir d turretTurnRate b |> updateBotPosition w.worldBox)
                 _ -> (newA,updateBotPosition w.worldBox b)

stepBullet : Bullet -> Bullet
stepBullet b = {b | bulletPosition <- {x=b.bulletPosition.x + b.bulletVelocity.x,y=b.bulletPosition.y + b.bulletVelocity.y}}

stepWorld : Float -> World -> World
stepWorld t w = {w | worldBots <- (map (stepBot w) w.worldBots)}

worldState : Signal World
worldState = foldp stepWorld defaultWorld tick

drawTank (a,b) = move (b.botPosition.x, b.botPosition.y) (rotate b.botAngle (filled black (rect 30 20)))
{-
(toForm (flow inward [ fittedImage 22 16 "res/radar.png", fittedImage 20 54 "res/turret.png"
             , fittedImage 36 38 "res/body.png" ]))
-}

turret =
  let c = 20
      wh = 2 * c + 1
      len = 12
      side = 8
      gauge = 2
      pill = filled grey (rect len side) --(c,c)
      barrel = filled grey (rect (2 * len) gauge) --(c+len, c)

  in collage wh wh [ pill, move (len, 0) barrel ]

drawTurret (a,b) =
  let coord = (b.botPosition.x, b.botPosition.y)
      (r,theta) = toPolar (b.botTurret.x,b.botTurret.y)
  in rotate (theta) (move coord (toForm turret))


display (w, h) (world) =
  let shiftX = world.worldBox.minX
      shiftY = world.worldBox.minY
      rw = world.worldBox.maxX - shiftX
      rh = world.worldBox.maxY - shiftY
  in
  collage w h ( (outlined (dashed grey) (rect (toFloat rw) (toFloat rh))) :: map drawTank (world.worldBots) ++ map drawTurret (world.worldBots)
              )

main = lift2 display Window.dimensions worldState
