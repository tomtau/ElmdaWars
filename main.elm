module Main where

import Time
import Window
import Math.Vector2
import Math.Vector2 (..)
import Either
import Either (..)
import Text
import World (..)
import SimpleBots (..)
import Automaton (Automaton,step)
import Display (..)

defaultBox = {minX = 50, minY = 50, maxX = 500, maxY = 500}
defaultWorld = {worldBots = [(makeBot "Retardon" 150 150 circleBot),
        (makeBot "Rammer" 100 100 rammingBot),
        (makeBot "TurretTest" -50 -50 sittingDuck),
        (makeBot "FireTest" -50 0 fireBot),
        (makeBot "RadarTest" 0 50 searchAndFire)], worldBullets = [], worldBox = defaultBox}

-- tick :: Signal Time
tick = fps 30

-- steps / updates / creations
--Automaton DashBoard Command

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

stepBot : Arena -> (Automaton DashBoard Command,BotState) -> ((Automaton DashBoard Command,BotState),Maybe Bullet)
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

stepBullet : Arena -> Bullet -> Either Bullet BulletHit
stepBullet w b =
  let newBP = add b.bulletPosition b.bulletVelocity
      hitwall = hitBox w.worldBox 1 newBP
  in if hitwall then Right BHWall else Left {b | bulletPosition <- newBP}

stepWorld : Float -> Arena -> Arena
stepWorld t w =
  let bullets = map (stepBullet w) w.worldBullets
      botsB = map (stepBot w) w.worldBots
      newBullets = map snd botsB |> concatMap (\x ->
        case x of Just y -> [y]
                  Nothing -> [])
  in {w | worldBots <- (map fst botsB), worldBullets <- (newBullets ++ lefts bullets)}

worldState : Signal Arena
worldState = foldp stepWorld defaultWorld tick

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
