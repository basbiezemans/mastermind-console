# Mastermind Console
Mastermind code-breaking game as a console application

[Mastermind (board game), Wikipedia](https://en.wikipedia.org/wiki/Mastermind_(board_game))

## Run the game in a docker container
```bash
make docker
```

## Play the game

The game has a minimal console user interface.
```
+------------------------------------+
| Mastermind, the code-breaking game |
+------------------------------------+
You have 10 turns to guess the code. Good luck!

Guess: _
```

As the codebreaker, you try to guess a 4-digit number within ten turns. After each guess, the program (codemaker) provides a hint of ones and zeros. A one for each digit from the guess which is correct and in the right position. A zero indicates the existence of a correct digit in the wrong position.

```
Guess: 1234
Turn: #1
Hint: 1,0

Guess: _
```

## Help / options

Common flags:

`--help`
`--version`

View all options:

```bash
$ mastermind-console --help
```

## Stack: build, test, and install

Build a docker image.
```bash
make build
```
Run the container with an interactive shell.
```bash
make shell
```

Build and test the application.
```bash
stack build --test
```

Install the application.
```bash
stack install
```

Make sure that `~/.local/bin` or an equivalent thereof is in the PATH environment variable.

Run the installed application.
```bash
mastermind-console
```