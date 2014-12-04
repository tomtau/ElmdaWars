module SimpleBots where

import World (..)
import Debug
import Math.Vector2 (..)
import Automaton (Automaton, hiddenState, step, pure)

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
