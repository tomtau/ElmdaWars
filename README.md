ElmdaWars
=========
A (work-in-progress) port of LambdaWars to Elm.

[LambdaWars](https://github.com/andreyLevushkin/LambdaWars/wiki/Project-Overview)
are a Haskell clone of [RoboCode](http://robocode.sourceforge.net).

## How to run
    elm-reactor

... then visit [http://localhost:8000/Main.elm](http://localhost:8000/Main.elm).

## TODO
A lot of things are missing, check the [Issues list](https://github.com/tomtau/ElmdaWars/issues) and feel free to contribute.

## How to write an AI
The type of programs that control tanks is Automaton DashBoard Command.
DashBoard is a structure that contains a robot tank's view of the world.
Command is an instruction the AI wants its controlled tank to execute
in a given time step.

### Stateless AI
Write a step function of type DashBoard -> Command; create an Automaton from it
by passing this function as an input to the 'pure' function.

### Stateful AI
Write a step function of type DashBoard -> STATETYPE -> (Command,STATETYPE);
which returns the command to execute and the next state given the input DashBoard
and state in the current time step.
Create an Automaton from this function using the function hiddenState:
hiddenState initial_state your_step_function.
