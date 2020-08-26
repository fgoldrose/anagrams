# anagrams
Realtime multiplayer implementation of the game anagrams

Written in Elm, using a Firebase Realtime Database

Play at https://fgoldrose.github.io/anagrams/


The game is played by any number of players. One player creates a room, and other players use the randomly generated room code to join that game. This means the application allows multiple games to happen simultaneously.

One action involved in the game is flipping a tile, which can be done with a button or space. This selects a tile randomly form the list of unflipped tiles. The game displays the pool of flipped tiles, and words that each player have taken. Players' scores are also kept track of and displayed through the game.
To take a word, a player types it and the game checks if that word can be constructed from combining one or more tiles from the pool with any word a player has previously taken.

There is also a history displayed of all moves that have been taken in the game. After a player steals a word but before another action is taken in the game, all players have the option to challenge the last move. If a majority of players challenge the word, that move is undone.
