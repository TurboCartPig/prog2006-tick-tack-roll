# Haskell assignment 1 - Tick tack roll

## Basics

Build:
```bash
stack build
```

Run tests:
```bash
stack test
```

Run program in human vs. cpu mode:
```bash
stack run -- -- --help
```
```
Usage: tic-tack-roll [--cpu | --human | --help]
      --cpu    Run the game in cpu vs. cpu mode
      --human  Run the game in human vs. human mode
  -h  --help   Print this help message
```

The project is organized with the main program in `app`, the common functionality in `src`, and tests in `test`.

The command line arguments are defined and parsed via GetOpt which is part of the base package.
There were a bunch of libraries for doing argument parsing, but none of them seemed to offer any significant advantage over GetOpt, other than maybe being more succinct.

## Time estimates

- Thinking about the problem: 4h
- Searching for stuff: 4h
- Coding/Documenting/Testing: 12h
- Total: 20h
