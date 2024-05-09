README:

These files comprise a working connect 4 terminal game with an implemented AI player. In order to play the game one should call the referee file with necessary AI and human players at the bottom of the file. Once the board appears, you can select the move by typing a number into the terminal corresponding to the column number. If you are player 1 you will be playing with X’s and player 2 will play with O’s. You will win if you have 4 in a row on the board and lose if the other player gets 4 in a row on the board. If all the spaces on the board are filled and neither player has won yet then the game is a tie. 

This program functions by defining a game within the Game module that was provided. We define the players by defining the type whichPlayer, define states with state , status with status and moves with move. Initial state will create the initial board state of the game using a variety of helper functions including make width and make height. Several other helper functions take our defined types and then output a string version such as stringOfPlayer, stringOfMove and StringOfState. StringOfState will be printed by the referee code so that the players can see the state of the game. LegalMoves creates a list of all the legal moves. nextState takes in a move and then applies it to the state producing the state that occurs after the move is made. This is done by adding the piece to the respective position and then checking if the game is over via a draw or win. By chaining together these functions it allows for the game Connect4 to run.

We also have an estimateValue function that is defined in the Game module for use by the AIPlayer if one should choose to play against an AI. The AI utilizes this rough static board estimator in conjunction with a minimax algorithm defined in AIPlayer.re to choose a good move. 

At the moment no major bugs are reported. 

Programmers: Noah Kim, Sean Kim

No major extra features.