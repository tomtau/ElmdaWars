module Display where

import World (..)
import Math.Vector2
import Math.Vector2 (..)

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
