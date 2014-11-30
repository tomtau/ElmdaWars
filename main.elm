module Main where

import Time
import Window
import Automaton (Automaton, hiddenState, step, pure)
import Math.Vector2
import Math.Vector2 (..)
import Either
import Either (..)
import Debug
import Text

-- rules
-- |Turn rate in degrees per tick
turnRate : Degree
turnRate = 5

-- |Radar turn rate in degrees per tick
radarTurnRate : Degree
radarTurnRate  = 15

-- |Turret turn rate in degrees per tick
turretTurnRate : Degree
turretTurnRate = 10

-- |Max speed in pixles per tick
maxSpeed       = 3

-- |Bullet speed in pixles per tick
bulletSpeed       = 40

radarRange = 100 -- 1200?

-- |Max acceleration in pixels per tick per tick
maxAcceleration : Float
maxAcceleration = 0.4

-- |Max deceleration in pixels per tick per tick
maxDeceleration : Float
maxDeceleration = 1

-- core // model
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
defaultWorld = {worldBots = [(makeBot "Retardon" 150 150 circleBot),
        (makeBot "Rammer" 100 100 rammingBot),
        (makeBot "TurretTest" -50 -50 sittingDuck),
        (makeBot "FireTest" -50 0 fireBot),
        (makeBot "RadarTest" 0 50 searchAndFire)], worldBullets = [], worldBox = defaultBox}

type Point = Vec2
type Direction = Vec2
type Degree = Float
type Radian = Float

data Command = NoAction
              | Turn Degree
              | Accelerate Float
              | Decelerate Float
              | MoveTurret Degree
              | MoveRadar Degree
              | Fire

-- TODO: bot that fired it
type Bullet = {
  bulletPosition  : Point,
  bulletVelocity  : Direction
}

-- TODO: bot hit?
data BulletHit = BHWall | BHBot

type Collision = Bool

-- TODO: extra info: bearing, ... (as in RoboCode)
data ScanResult = BotFound --BotFound Double
                | WallFound --WallFound Double
                | NothingFound

type DashBoard = {
  dashRadar     : ScanResult,
  dashWallHit   : Collision,
  dashVelocity  : Direction,
  dashTurret    : Direction,
  dashRadarDir  : Direction
  }

-- TODO: score, HP, ...
type BotState = {
  botName     : String,
  botPosition : Point,
  botAngle    : Radian,
  botVelocity : Direction,
  botTurret   : Direction,
  botRadar    : Direction,
  botLastCmd  : Command -- ^ useful for logging
}

-- BOTS
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

searchAndFire : Automaton DashBoard Command
searchAndFire = pure (\d ->
  let res = d.dashRadar
      (_,ttheta) = toPolar (d.dashTurret |> toTuple) |> Debug.watch "ttheta"
      (_,rtheta) = toPolar (d.dashRadarDir |> toTuple) |> Debug.watch "rtheta"
  in case res of NothingFound -> MoveRadar 1
                 WallFound -> MoveRadar 1
                 BotFound -> if (abs (ttheta-rtheta) < 0.1) then Fire
                             else MoveTurret (degrees (rtheta-ttheta))
  )

-- tick :: Signal Time
tick = fps 30

-- steps / updates / creations
--Automaton DashBoard Command
makeBot : String -> Float -> Float -> Automaton DashBoard Command -> (Automaton DashBoard Command,BotState)
makeBot name x y program = (program,{botName = name,
  botPosition = vec2 x y,
  botAngle = 0,
  botVelocity = vec2 0 0,
  botTurret = vec2 1 0,
  botRadar = vec2 1 0,
  botLastCmd = NoAction})

makeBullet : Float -> Float -> Float -> Float -> Bullet
makeBullet x y rx ry = {bulletPosition = vec2 x y, bulletVelocity = vec2 rx ry}

hitBox : BoundingBox -> Float -> Point -> Bool
hitBox bbox d p =
  let minX = -(toFloat bbox.maxX / 2) + toFloat bbox.minX + d
      maxX = toFloat bbox.maxX / 2 - toFloat bbox.minX - d
      minY = -(toFloat bbox.maxY / 2) + toFloat bbox.minY + d
      maxY = toFloat bbox.maxY / 2 - toFloat bbox.minY - d
      x = getX p
      y = getY p
  in (x <= minX) || (x >= maxX) || (y <= minY) || (y >= maxY)

updateBotPosition : BoundingBox -> BotState -> BotState
updateBotPosition bbox b =
  let newPosition = add b.botPosition b.botVelocity
      clamped = vec2 (clamp (-(toFloat bbox.maxX / 2) + toFloat bbox.minX)
                      (toFloat bbox.maxX / 2 - toFloat bbox.minX)
                      (getX newPosition))
                      (clamp (-(toFloat bbox.maxY / 2) + toFloat bbox.minY)
                      (toFloat bbox.maxY / 2 - toFloat bbox.minY)
                      (getY newPosition))
  in {b | botPosition <- clamped}

updateBotSpeed : Float -> Float -> Float -> BotState -> BotState
updateBotSpeed acc maxAcc maxSpeed b =
  let newAcc = if (maxAcc > 0) then clamp 0 maxAcc acc else clamp maxAcc 0 acc
      (r,theta) = toPolar (b.botVelocity |> toTuple)
      newSpeed = clamp 0 maxSpeed (r+newAcc)
      carts = fromPolar (newSpeed,theta) |> fromTuple
  in
    {b | botVelocity <- carts}

--TODO: copy-paste -> refactor
updateBotDir : Degree -> Degree -> BotState -> BotState
updateBotDir degTurn maxDeg b =
  let newDeg = clamp (-maxDeg) maxDeg degTurn
      newTurn = radians newDeg
      (r,theta) = toPolar (b.botVelocity |> toTuple)
      carts = fromPolar (r,theta + newTurn) |> fromTuple
  in
    {b | botVelocity <- carts, botAngle <- (theta + newTurn)}

updateTurrDir : Degree -> Degree -> BotState -> BotState
updateTurrDir degTurn maxDeg b =
  let newDeg = clamp (-maxDeg) maxDeg degTurn
      newTurn = radians newDeg
      (r,theta) = toPolar (b.botTurret |> toTuple)
      carts = fromPolar (r,theta + newTurn) |> fromTuple
  in
    {b | botTurret <- carts}

updateRadarDir : Degree -> Degree -> BotState -> BotState
updateRadarDir degTurn maxDeg b =
  let newDeg = clamp (-maxDeg) maxDeg degTurn
      newTurn = radians newDeg
      (r,theta) = toPolar (b.botRadar |> toTuple)
      carts = fromPolar (r,theta + newTurn) |> fromTuple
  in
    {b | botRadar <- carts}

fireBullet : BotState -> Bullet
fireBullet b =
  let (r,theta) = toPolar (b.botTurret |> toTuple)
      bulVel = fromPolar (bulletSpeed, theta) |> fromTuple
      bulPos = add b.botPosition bulVel
  in {bulletPosition=bulPos, bulletVelocity=bulVel}

inB : Point -> Point -> Point -> Bool
inB start stop otherB =
  let (firstR,firstTh) = toPolar (stop |> toTuple)
      (otherPR,otherTh) = toPolar ((sub otherB start) |> toTuple)
  in (abs (firstTh - otherTh) < 0.1) && (otherPR <= firstR)

--TODO: wallscan
scan : BotState -> [BotState] -> ScanResult
scan b bs =
  let start = b.botPosition
      stop = (add b.botPosition (Math.Vector2.scale radarRange b.botRadar))
      scanned = map (\x -> inB start stop x.botPosition) bs |> any (\x -> x)
  in if scanned then BotFound else NothingFound

stepBot : World -> (Automaton DashBoard Command,BotState) -> ((Automaton DashBoard Command,BotState),Maybe Bullet)
stepBot w (a,b) =
  let hitwall = hitBox w.worldBox 1 b.botPosition
      scanRes = scan b (map snd w.worldBots |> filter (\x -> x /= b))
      (newA, cmd) = step {dashRadar=scanRes,dashVelocity=b.botVelocity,dashRadarDir=b.botRadar,dashTurret=b.botTurret,dashWallHit=hitwall} a
  in case cmd of NoAction -> ((newA,updateBotPosition w.worldBox b),Nothing)
                 Turn d -> ((newA, updateBotDir d turnRate b |> updateBotPosition w.worldBox),Nothing)
                 Accelerate x -> ((newA, updateBotSpeed (abs x) maxAcceleration maxSpeed b |> updateBotPosition w.worldBox),Nothing)
                 Decelerate x -> ((newA, updateBotSpeed (-(abs x)) maxDeceleration maxSpeed b |> updateBotPosition w.worldBox),Nothing)
                 MoveTurret d -> ((newA, updateTurrDir d turretTurnRate b |> updateBotPosition w.worldBox),Nothing)
                 MoveRadar d -> ((newA, updateRadarDir d radarTurnRate b |> updateBotPosition w.worldBox),Nothing)
                 Fire -> ((newA,updateBotPosition w.worldBox b),Just (fireBullet b))

stepBullet : World -> Bullet -> Either Bullet BulletHit
stepBullet w b =
  let newBP = add b.bulletPosition b.bulletVelocity
      hitwall = hitBox w.worldBox 1 newBP
  in if hitwall then Right BHWall else Left {b | bulletPosition <- newBP}

stepWorld : Float -> World -> World
stepWorld t w =
  let bullets = map (stepBullet w) w.worldBullets
      botsB = map (stepBot w) w.worldBots
      newBullets = map snd botsB |> concatMap (\x ->
        case x of Just y -> [y]
                  Nothing -> [])
  in {w | worldBots <- (map fst botsB), worldBullets <- (newBullets ++ lefts bullets)}

worldState : Signal World
worldState = foldp stepWorld defaultWorld tick

-- graphics / view

drawTank (a,b) = move (b.botPosition |> toTuple) (rotate b.botAngle (filled black (rect 30 20)))
{-
(toForm (flow inward [ fittedImage 22 16 "res/radar.png", fittedImage 20 54 "res/turret.png"
             , fittedImage 36 38 "res/body.png" ]))
-}

drawName (a,b) = move (b.botPosition |> toTuple) (move (0,-20) (centered (bold (toText b.botName)) |> toForm))

turret =
  let c = 20
      wh = 2 * c + 1
      len = 12
      side = 8
      gauge = 2
      pill = filled grey (rect len side)
      barrel = filled grey (rect (2 * len) gauge)

  in collage wh wh [ pill, move (len, 0) barrel ]

drawTurret (a,b) =
  let coord = b.botPosition |> toTuple
      (r,theta) = toPolar (b.botTurret |> toTuple)
  in rotate (theta) (move coord (toForm turret))

drawRadar (a,b) =
  let coord = b.botPosition |> toTuple
      endP = (add b.botPosition (Math.Vector2.scale radarRange b.botRadar)) |> toTuple
      l = segment coord endP
  in traced (solid red) l

drawBullet b =
  move (b.bulletPosition |> toTuple) (filled green (circle 3))

display (w, h) (world) =
  let shiftX = world.worldBox.minX
      shiftY = world.worldBox.minY
      rw = world.worldBox.maxX - shiftX
      rh = world.worldBox.maxY - shiftY
  in
  collage w h ( (outlined (dashed grey) (rect (toFloat rw) (toFloat rh)))
    :: map drawTank (world.worldBots)
    ++ map drawTurret (world.worldBots)
    ++ map drawRadar (world.worldBots)
    ++ map drawName (world.worldBots)
    ++ map drawBullet (world.worldBullets)
              )

main = lift2 display Window.dimensions worldState
