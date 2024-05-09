open CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      /*
       Gets the index of a list that the maximum value is at
       inputs: a list of floats lst, an int index, an int current and a float n
       outputs: an integer represnting the index with maximum value
       recursion: holds a current max recurs thorugh each eleemnt of the list to
                  see if the hd of the list is greater then the current max and
                  updates if it is.
       */
      let rec maxIndex: (list(float), int, int, float) => int =
        (lst, index, current, n) => {
          switch (lst) {
          | [] => index
          | [hd, ...tl] =>
            if (hd > n) {
              maxIndex(tl, current, current + 1, hd);
            } else {
              maxIndex(tl, index, current + 1, n);
            }
          };
        };
      /*
       Gets the index of a list that the min value is at
       inputs: a list of floats lst, an int index, an int current and a float n
       outputs: an integer represnting the index with min value
       recursion: holds a current min recurs thorugh each eleemnt of the list to
                  see if the hd of the list is less then the current min and
                  updates if it is.
       */
      let rec minIndex: (list(float), int, int, float) => int =
        (lst, index, current, n) => {
          switch (lst) {
          | [] => index
          | [hd, ...tl] =>
            if (hd < n) {
              minIndex(tl, current, current + 1, hd);
            } else {
              minIndex(tl, index, current + 1, n);
            }
          };
        };

      /*
       gets the value of an index
       input: List of moves lst and int index
       output: the element of lst at index
       recursion: recurs through list removing the hd and lowering index until
       index = 0
       */
      let rec indexMoveReturn: (list(PlayerGame.move), int) => PlayerGame.move =
        (lst, index) => {
          switch (lst, index) {
          | ([hd, ..._tl], 0) => hd
          | ([_hd, ...tl], _) => indexMoveReturn(tl, index - 1)
          | ([], _index) => failwith("invalid index")
          };
        };

      /*
       Get the maximum of a list
       input: list of floats lst and float n
       output: the maximum on the list
       recursion: recur through holding a max and checking the hd with max to
       update
       */
      let rec maxList: (list(float), float) => float =
        (lst, n) => {
          switch (lst) {
          | [] => n
          | [hd, ...tl] =>
            if (hd > n) {
              maxList(tl, hd);
            } else {
              maxList(tl, n);
            }
          };
        };

      /*
       Get the minimum of a list
       input: list of floats lst and float n
       output: the minimum on the list
       recursion: recur through holding a minimum and checking the hd with min to
       update
       */
      let rec minList: (list(float), float) => float =
        (lst, n) => {
          switch (lst) {
          | [] => n
          | [hd, ...tl] =>
            if (hd < n) {
              minList(tl, hd);
            } else {
              minList(tl, n);
            }
          };
        };

      /*
       get all the possible next states
       input: a state current
       output: a list of possible states
       */

      let nextStateLegals: PlayerGame.state => list(PlayerGame.state) =
        current => {
          let stateList = PlayerGame.nextState(current);
          List.map(stateList, PlayerGame.legalMoves(current));
        };

      /*
           get the value of node via minimax
           input: a state st, an int depth, a int player
           output: a float representing the estimated value of situation based on
           looking ahead moves
           recursion: keep looking at future states using nextStatelegals until
           depth = 0 where you use estimate value to calculate how good each of
           the states are then alternate choosing min and max from each of the
           available options until we get the value of the node.
       */
      let rec minimaxHelper: (PlayerGame.state, int, int) => float =
        (st, depth, player) =>
          if (depth == 0) {
            PlayerGame.estimateValue(st);
          } else if (player == 1) {
            maxList(
              List.map(minimaxHelper(_, depth - 1, 2), nextStateLegals(st)),
              -999999.0,
            );
          } else if (player == 2) {
            minList(
              List.map(minimaxHelper(_, depth - 1, 1), nextStateLegals(st)),
              999999.0,
            );
          } else {
            failwith("neither player 1 or 2");
          };

      /*
           get the minimax estimate values for all possible next states
           input: state st, int depth, int player
           output: gets a list of floats giving the estimate value of each of the
           next possible positions looking ahead using a minimax algorithim.
       */
      let minimaxLookup: (PlayerGame.state, int, int) => list(float) =
        (st, depth, player) => {
          List.map(minimaxHelper(_, depth, player), nextStateLegals(st));
        };

      /*
           get the best move
           input: state st and int depth
           output: a move that is best according to minimax algorithim
       */
      let minimax: (PlayerGame.state, int) => PlayerGame.move =
        (st, depth) => {
          switch (PlayerGame.gameStatus(st)) {
          | Ongoing(P1) =>
            indexMoveReturn(
              PlayerGame.legalMoves(st),
              maxIndex(minimaxLookup(st, depth, 2), 0, 0, -9999.0),
            )
          | Ongoing(P2) =>
            indexMoveReturn(
              PlayerGame.legalMoves(st),
              minIndex(minimaxLookup(st, depth, 1), 0, 0, 9999.0),
            )
          | _ => failwith("Needs ongoing")
          };
        };
      minimax(s, 5);
    };

  /* put your team name here! */
  let playerName = "";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer;

/* insert test cases for any procedures that don't take in
 * or return a state here */
