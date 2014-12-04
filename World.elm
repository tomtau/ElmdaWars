module World where

import Math.Vector2
import Math.Vector2 (..)
import Automaton (Automaton, hiddenState, step, pure)

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

type Arena  = {
   worldBots    : [(Automaton DashBoard Command, BotState)],
   worldBullets : [Bullet],
   worldBox     : BoundingBox
}

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
