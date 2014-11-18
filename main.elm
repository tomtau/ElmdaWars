import Keyboard
import Window

-- Game constants
(gameWidth, gameHeight) = (500, 500)
--

-- Type declarations
type Game = { player1:Player }

defaultGame : Game
defaultGame = { player1 = makePlayer 0 0 }

type Point = { x:Float, y:Float }
type Direction = Point

type BotState = {
  botName     : String,
  botPosition : Point,
  botVelocity : Point,
  botTurret   : Direction,
  botRadar    : Direction
  --botLastCmd  :: Command -- ^ useful for logging
}

type Player = { x:Float, y:Float, vx:Float, vy:Float }
type Input = { space: Bool, keyboard: {x: Int, y: Int}, delta: Time }

delta = inSeconds <~ fps 60
input = sampleOn delta (Input <~ Keyboard.space
                               ~ Keyboard.arrows
                               ~ delta)

displayObj obj shape =
    move (obj.x, obj.y) (shape)

makePlayer : Float -> Float -> Player
makePlayer x y = { x=x, y=y, vx=0, vy=0 }

display (w, h) { player1 }  =
  collage gameWidth gameHeight
    [displayObj player1 (toForm (flow inward [ fittedImage 22 16 "res/radar.png", fittedImage 20 54 "res/turret.png"
                 , fittedImage 36 38 "res/body.png" ])){-()-}]

stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx * t
          , y <- y + vy * t }

stepPlayer t dir player =
  stepObj t { player | vy <- toFloat dir.y * 100 , vx <- toFloat dir.x * 100}

stepGame : Input -> Game -> Game
stepGame { space, keyboard, delta } game =
  { game | player1 <- stepPlayer delta keyboard game.player1 }

gameState : Signal Game
gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
