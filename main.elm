module Main where

import Time
import Window
import Automaton (Automaton, hiddenState, step)

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
type World  = {
   worldBots    : [(Automaton DashBoard Command, BotState)],
   worldBullets : [Bullet]
   --_worldBox     :: BoundingBox
}

defaultWorld = {worldBots = [(makeBot "Retardon" 200 200 circleBot)], worldBullets = []}

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
--  , dashWallHit   : Collision,
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

-- tick :: Signal Time
tick = fps 30
--Automaton DashBoard Command
makeBot : String -> Float -> Float -> Automaton DashBoard Command -> (Automaton DashBoard Command,BotState)
makeBot name x y program = (program,{botName = name,
  botPosition = {x=x,y=y},
  botAngle = 0,
  botVelocity = zeroPoint,
  botTurret = zeroPoint,
  botRadar = zeroPoint,
  botLastCmd = NoAction})

updateBotPosition : BotState -> BotState
updateBotPosition b = {b | botPosition <- {x=b.botPosition.x + b.botVelocity.x,y=b.botPosition.y + b.botVelocity.y}}

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


stepBot : (Automaton DashBoard Command,BotState) -> (Automaton DashBoard Command,BotState)
stepBot (a,b) = let (newA, cmd) = step {dashVelocity=b.botVelocity} a
  in case cmd of NoAction -> (newA,updateBotPosition b)
                 Turn d -> (newA, updateBotDir d turnRate b |> updateBotPosition)
                 Accelerate x -> (newA, updateBotSpeed x maxAcceleration maxSpeed b |> updateBotPosition)
                 _ -> (newA,updateBotPosition b)

stepBullet : Bullet -> Bullet
stepBullet b = {b | bulletPosition <- {x=b.bulletPosition.x + b.bulletVelocity.x,y=b.bulletPosition.y + b.bulletVelocity.y}}

stepWorld : Float -> World -> World
stepWorld t w = {w | worldBots <- (map stepBot w.worldBots)}

worldState : Signal World
worldState = foldp stepWorld defaultWorld tick

drawTank (a,b) = move (b.botPosition.x, b.botPosition.y) (rotate b.botAngle (filled black (rect 30 20)))
{-
(toForm (flow inward [ fittedImage 22 16 "res/radar.png", fittedImage 20 54 "res/turret.png"
             , fittedImage 36 38 "res/body.png" ]))
-}

display (w, h) (world) =
  collage w h ( map drawTank (world.worldBots)
              )

main = lift2 display Window.dimensions worldState
