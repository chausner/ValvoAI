# ValvoAI
Minimax-based AI implementation for the board game [Valvo](https://familygames.com/extras) in F#

## Features
* Simulate the game for boards of arbitrary size
* Simulate and compare random, greedy and perfect (minimax) playing strategies
* Minimax can optimize either on expected game score or win probability
* Compute expected scores and win probabilities
* Draw visual representation of scores and win probabilities
* Trainer: reads game state from a running game instance, prints win probabilities and the next suggested move
* Generate random boards with interesting properties (e.g. minimize probability of a draw)
* Generate scatter plot of win probabilities
* Compute game statistics (win probabilities) for different playing strategies

Adapt the main entrypoint in Program.fs to choose what you want to run.

## Minimax AI
The AI implementation in the original game uses a minimax search where the depth limit is determined by the chosen AI level. The maximum level corresponds to a depth of 7. However, it seems the implementation of the scoring function in the original game is wrong and does not compute the scores correctly, thus it is expected to perform worse than a proper implementation.

This implementation takes advantage of the fact that the number of game states is very low (around 6000 for a 8x7 board) which makes it feasible to solve the game perfectly using Linear Programming. No full-blown LP solver is used here but a simpler fixed-point iteration approach.

## License
MIT, see [LICENSE](LICENSE)
