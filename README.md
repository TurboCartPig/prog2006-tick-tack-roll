# Haskell assignment 1 - Tick tack roll

By default the program runs in human vs. cpu mode, were the program will print the board every time it prompts for user input. One oddity is that if the game is run in cpu vs. cpu mode the board is never printed, the game just finishes instantly.

The project is organized with the main program in `app`, the common functionality in `src`, and tests in `test`. Most of the tests are doctests, but there are also some hspec unit tests.

The command line arguments are defined and parsed via GetOpt which is part of the base package.
There were a bunch of libraries for doing argument parsing, but none of them seemed to offer any significant advantage over GetOpt, other than maybe being more succinct.

## Basics

Build:
```bash
stack build
```

Run tests:
```bash
stack test
```

Run program
```bash
stack run -- -- --help
```
```
Usage: tic-tack-roll [--cpu | --human | --help]
      --cpu    Run the game in cpu vs. cpu mode
      --human  Run the game in human vs. human mode
  -h  --help   Print this help message
```

Example run of the game:
```
# ___
# ___
# ___
4
# __O
# X__
# ___
5
# __O
# XX_
# __O
7
# __O
# XX_
# XOO
6
GAME OVER! X Won!
```

One with rotations:
```
# ___
# ___
# ___
1 left
# X_O
# ___
# ___
2 right
# __O
# __X
# _OX
5
# __O
# _XX
# OOX
4 left
GAME OVER! X Won!
```

## Time estimates

- Thinking about the problem: 4h
- Searching for stuff: 4h
- Coding/Documenting/Testing: 12h
- Total: 20h
