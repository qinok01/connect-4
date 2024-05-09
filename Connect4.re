open CS17SetupGame;
open Game;

type board = list(list(int));

module Connect4 = {
  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  type state =
    | State(status, board);

  type move =
    | Move(int);

  /* input: string
     ouput: the intial state */

  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);

      /* input: an int x and list lst
         ouput: a list of 0's x length long
         recursion: recurs by adding 0s onto lists until x is 0
         */
      let rec makeWidth: (int, list(int)) => list(int) =
        (x, lst) => {
          switch (x) {
          | 0 => lst
          | x => makeWidth(x - 1, [0, ...lst])
          };
        };

      /*input: a int w, list lst and board old
        output: a board made up of w lst's put into a list
        recursion: reucrs until x = 1 by cons lst onto old
        */

      let rec makeHeight: (int, list(int), board) => board =
        (w, lst, old) => {
          switch (w) {
          | 1 => old
          | x => makeHeight(x - 1, lst, [lst, ...old])
          };
        };

      State(
        Ongoing(P1),
        makeHeight(
          boardHeight,
          makeWidth(boardWidth, []),
          [makeWidth(boardWidth, [])],
        ),
      );
    };

  /*input: a player
    output: a string of that player
     */
  let stringOfPlayer: whichPlayer => string =
    x => {
      switch (x) {
      | P1 => "P1"
      | P2 => "P2"
      };
    };

  let stringOfState: state => string =
    x => {
      /*input: an int x
        output: a string "[ ]", " X " or " O " depending on x to make the game
         prettier*/
      let intToPiece: int => string =
        x => {
          switch (x) {
          | 0 => "[ ]"
          | 1 => " X "
          | 2 => " O "
          | _ => failwith("inputs should be 1 or 2")
          };
        };

      /*input: a list of integers lst and a string str
        output: a string where the integers use intToPiece to turn each element
              of lst into symbols and concatonated
        recursion: recurs through each elemnt of lst to change the data
        representation of the board into clean corresponding symbols.
         */

      let rec visline: (list(int), string) => string =
        (lst, str) => {
          switch (lst) {
          | [] => str
          | [hd, ...tl] => intToPiece(hd) ++ "" ++ visline(tl, str)
          };
        };

      /* input: a board
         // output: the visual represnetation of the board
           recursion: recurs through lines of board calling visline on each line
          */

      let rec visual: board => string =
        b => {
          switch (b) {
          | []
          | [[]] => ""
          | [hd, ...tl] => visline(hd, "") ++ "\n" ++ visual(tl)
          };
        };

      switch (x) {
      | State(Win(P1), s) => "Player 1 WINS!!\n\n" ++ visual(s)
      | State(Win(P2), s) => "Player 2 WINS!!\n\n" ++ visual(s)
      | State(Draw, s) => "ITS A DRAW\n\n" ++ visual(s)
      | State(Ongoing(P1), s) => "This is the board:\n\n" ++ visual(s)
      | State(Ongoing(P2), s) => "This is the board:\n\n" ++ visual(s)
      };
    };

  /*
   input: move mov
   output: string representing mov
   */
  let stringOfMove: move => string =
    mov => {
      switch (mov) {
      | Move(n) => string_of_int(n)
      };
    };

  /* Game Logic */

  /*
   input: state inState
   output: produces the list of legal moves at a state */

  let legalMoves: state => list(move) =
    inState => {
      /* input: list of integers top, list of integers active, int index
         // ouput: the list of indexs that correspond to element == 0
          recursion: recurs through the list and if element = 0 add it to a list
         */

      let rec returnIndexs: (list(int), list(int), int) => list(int) =
        (top, active, index) => {
          switch (top) {
          | [] => active
          | [hd, ...tl] =>
            if (hd == 0) {
              returnIndexs(tl, active @ [index], index + 1);
            } else {
              returnIndexs(tl, active, index + 1);
            }
          };
        };

      switch (inState) {
      | State(_, [])
      | State(_, [[]]) => failwith("empty board")
      | State(_, [hd, ..._tl]) =>
        List.map(value => Move(value), returnIndexs(hd, [], 1))
      };
    };

  /*
   input: state inState
   output: the status of the game at the given state */

  let gameStatus: state => status =
    inState =>
      switch (inState) {
      | State(p, _) => p
      };

  /*
   input: state inState and move mov
   Ouput: next state when move applied to state*/

  /*input: a board mat
    //output: the board transposed
    recursion:
     OI: [[a, b, c], [d, e, f]]
       RI: [[b,c][e,f]]
       RO: [[b,e][c,f]]
       ideation: cons on [a, d] to RO
     OO:[[a, d][b, e][c, f]]

     OI: [[a, b, c], [d, e, f], [g, h, i]]
       RI: [[b,c][e,f][h,i]]
       RO: [[b,e,h][c,f,i]
       ideation: cons on [a, d, g] to RO
     OO:[[a, d, g][b, e, h][c, f, i]]
     */
  let rec transpose: board => board =
    mat =>
      switch (mat) {
      | []
      | [[], ..._] => failwith("A matrix cannot be 0 - dimensional.")
      | [[_hd], ..._] => [List.flatten(mat)]
      | [[_hd, ..._tl], ..._] => [
          List.map(List.hd, mat),
          ...transpose(List.map(List.tl, mat)),
        ]
      };

  let vertFlip: board => board = mat => List.map(List.rev, mat);

  let nextState: (state, move) => state =
    (inState, mov) => {
      /*input: int x, list lst
          ouput: true if x is in list else false
          recursion: recur through the list to see if x is in list
        */
      let rec checkElement: (int, list(int)) => bool =
        (x, lst) => {
          switch (x, lst) {
          | (_, []) => false // note might need error instead
          | (1, [hd, ..._tl]) => hd == 0
          | (n, [_hd, ...tl]) => checkElement(n - 1, tl)
          };
        };

      /* input: int index, list lst, list addlist, z
         //ouput: a list with the element with index index switched to z
         recursion: recur through the list where index goes down by 1 and hd is removed
         after each recurrance. once index = 0 return the head
         */
      let rec switchElement: (int, list(int), list(int), int) => list(int) =
        (index, lst, addlist, z) => {
          switch (index, lst) {
          | (_, []) => failwith("empty list")
          | (1, [_hd, ...tl]) => addlist @ [z] @ tl
          | (n, [hd, ...tl]) => switchElement(n - 1, tl, addlist @ [hd], z)
          };
        };
      /* input: int index, list lst and int x
         // ouput: a list with the element with index index switched to z*/
      let switchE: (int, list(int), int) => list(int) =
        (index, lst, z) => {
          switchElement(index, lst, [], z);
        };

      /*input: a state inState, a move mov, a board br
          output: the new board after the move is done
          recursion: check to see if where the player wants to move is empty at the 2nd top row.
          If it is move down a row to check if the 3rd top row is empty, continue
          downwards until one is not empty. then switch the element in the row above
          to that of the players piece.
        */

      let rec nextStateHelper: (state, move, board) => board =
        (inState, mov, br) => {
          switch (inState, mov) {
          | (State(Draw, _), Move(_)) => failwith("Drawn game")
          | (State(Win(_p), _), Move(_)) => failwith("Finished game")
          | (State(Ongoing(_), []), _) => failwith("empty board")

          | (State(Ongoing(P1), [hd]), Move(n)) =>
            br @ [switchE(n, hd, 1)]
          | (State(Ongoing(P2), [hd]), Move(n)) =>
            br @ [switchE(n, hd, 2)]

          | (State(Ongoing(P1), [hd1, hd2, ...tl]), Move(n)) =>
            if (checkElement(n, hd2)) {
              nextStateHelper(
                State(Ongoing(P1), [hd2, ...tl]),
                mov,
                if (br == [[]]) {
                  [hd1];
                } else {
                  br @ [hd1];
                },
              );
            } else if (br == [[]]) {
              [switchE(n, hd1, 1), hd2, ...tl];
            } else {
              br @ [switchE(n, hd1, 1), hd2, ...tl];
            }

          | (State(Ongoing(P2), [hd1, hd2, ...tl]), Move(n)) =>
            if (checkElement(n, hd2)) {
              nextStateHelper(
                State(Ongoing(P2), [hd2, ...tl]),
                mov,
                if (br == [[]]) {
                  [hd1];
                } else {
                  br @ [hd1];
                },
              );
            } else if (br == [[]]) {
              [switchE(n, hd1, 2), hd2, ...tl];
            } else {
              br @ [switchE(n, hd1, 2), hd2, ...tl];
            }
          };
        };

      /*input: list of ints lst and int x
        //output: if there is at least four x's in a row in lst true, else false
          recursion: check if first 4 elements are equal to x
          if yes output true else remove first element and recur
        */
      let rec winHorzHelper: (list(int), int) => bool =
        (lst, x) => {
          switch (lst) {
          | [hd1, hd2, hd3, hd4, ..._tl] =>
            if (hd1 == hd2 && hd2 == hd3 && hd3 == hd4 && hd1 == x) {
              true;
            } else {
              winHorzHelper(List.tl(lst), x);
            }
          | _ => false
          };
        };

      /*input: a board br and int x
        //output: a bool true if there is a horizontal 4 in a row, else false
          recursion: recur through the row of the board by calling winHorzHelper
          on each row until resolution
        */
      let rec winHorz: (board, int) => bool =
        (br, x) => {
          switch (br) {
          | []
          | [[]] => false
          | [hd, ...tl] => winHorzHelper(hd, x) || winHorz(tl, x)
          };
        };

      /*input: a board br and int x
        //output: a bool true if there is a vertical 4 in a row, else false*/
      let winVert: (board, int) => bool =
        (lst, x) => {
          winHorz(transpose(lst), x);
        };

      /* input: a board br and int x
         output: if a left-right diagnol win on the first 4 rows true, else false
         recursion
         recursion: recur through the board by to see if there is a left to right
         downward diagnol. if no then remove the first column of the board and recur
         */
      let rec diagCheck: (board, int) => bool =
        (br, x) => {
          switch (br) {
          | [
              [hd1, ..._tl1],
              [_, hd2, ..._tl2],
              [_, _, hd3, ..._tl3],
              [_, _, _, hd4, ..._tl4],
              ..._tl,
            ] =>
            if (hd1 == hd2 && hd2 == hd3 && hd3 == hd4 && hd1 == x) {
              true;
            } else {
              diagCheck(List.map(List.tl, br), x);
            }
          | _ => false
          };
        };

      /* input: a board br and int x
         output: if there is a left-right diagnol win true, else false
         recursion: recur through the rows of br by calling diag check and if no then
         remove a row and recur.
          */
      let rec diagHelper: (list(list(int)), int) => bool =
        (br, x) => {
          switch (br) {
          | [_, _, _, _, ..._tl] =>
            if (diagCheck(br, x)) {
              true;
            } else {
              diagHelper(List.tl(br), x);
            }
          | _ => false
          };
        };

      /*input: a board br and int x
        output:  if there is a diagnol win true, else false */
      let winDiag: (board, int) => bool =
        (br, x) => {
          diagHelper(br, x) || diagHelper(vertFlip(br), x);
        };

      /*input: a board br and int x
        output:  if there is a win true, else false */
      let win: (board, int) => bool =
        (br, x) => {
          winHorz(br, x) || winVert(br, x) || winDiag(br, x);
        };

      switch (inState, mov) {
      | (State(Draw, _), Move(_)) => inState
      | (State(Win(_), _), Move(_)) => inState

      | (State(Ongoing(P1), _), Move(_)) =>
        if (win(nextStateHelper(inState, mov, [[]]), 1)) {
          State(Win(P1), nextStateHelper(inState, mov, [[]]));
        } else if (legalMoves(
                     State(
                       Ongoing(P2),
                       nextStateHelper(inState, mov, [[]]),
                     ),
                   )
                   == []) {
          State(Draw, nextStateHelper(inState, mov, [[]]));
        } else {
          State(Ongoing(P2), nextStateHelper(inState, mov, [[]]));
        }

      | (State(Ongoing(P2), _), Move(_)) =>
        if (win(nextStateHelper(inState, mov, [[]]), 2)) {
          State(Win(P2), nextStateHelper(inState, mov, [[]]));
        } else if (legalMoves(
                     State(
                       Ongoing(P1),
                       nextStateHelper(inState, mov, [[]]),
                     ),
                   )
                   == []) {
          State(Draw, nextStateHelper(inState, mov, [[]]));
        } else {
          State(Ongoing(P1), nextStateHelper(inState, mov, [[]]));
        }
      };
    };

  /* for transforming human player input into
     internal representation of move */
  let moveOfString: (string, state) => move =
    (str, isState) =>
      if (List.mem(
            try(Move(int_of_string(str))) {
            | _ => failwith("please insert an integer")
            },
            legalMoves(isState),
          )) {
        Move(int_of_string(str));
      } else {
        failwith("not legal");
      };

  /*
   estimateHorzOpenTwoHelp:
   input: lst, a single row of a board
         value, an integer that serves as an accumulator
         discount, an integer that decreases value based on specific Horziontal
                   Open Two Cases
   output:  For every instance of ( _ X X _ ) in the row, add 10.
           For every instance of ( _ O O _ ) in the row, subtract 10.
   Recursion explanation: You recur on List.tl(lst) in order to iterate by
                         1 column at a time horizontally.
   */

  let rec estimateHorzOpenTwoHelp: (list(int), int, int) => int =
    (lst, value, discount) => {
      switch (lst) {
      | [0, 1, 1, 0, ..._tl] =>
        estimateHorzOpenTwoHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [0, 2, 2, 0, ..._tl] =>
        estimateHorzOpenTwoHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )
      | [_, _, _, _, ..._tl] =>
        estimateHorzOpenTwoHelp(List.tl(lst), value, discount)
      | _ => value
      };
    };

  /*
   estimateHorzOpenTwo:
   input: br, a board
   output: sums estimateHorzOpenTwoHelp of every row of br
   Recursion explanation: You recur on br's tl in the form [hd, ...tl] so that
                           you iterate down by 1 row at a time.
   */

  let rec estimateHorzOpenTwo: (board, int, int) => int =
    (br, value, discount) => {
      switch (br) {
      | []
      | [[]] => value
      | [hd, ...tl] =>
        estimateHorzOpenTwo(
          tl,
          value + estimateHorzOpenTwoHelp(hd, 0, discount),
          discount + 1,
        )
      };
    };
  //Test cases for estimateHorzOpenTwo
  let board1 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0],
  ];
  let board2 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 0, 0, 0, 0],
    [0, 2, 2, 0, 0, 0, 0],
    [0, 2, 2, 0, 0, 0, 0],
  ];
  let board3 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0],
  ];
  checkExpect(estimateHorzOpenTwo(board1, 0, 0), 27, "case with P1");
  checkExpect(estimateHorzOpenTwo(board2, 0, 0), -39, "case with P2");
  checkExpect(estimateHorzOpenTwo(board3, 0, 0), 15, "case with P1 & P2");

  /*
   estimateDiagOpenTwoHelp:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses 4 rows horizontally to search for instances of open doubles
   Recursion explanation: You recur on List.map(List.tl, br) to horizontally shift
                         each of the 4 rows by 1 column on the recursive call
   */

  let rec estimateDiagOpenTwoHelp: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [
          [0, ..._tl1],
          [_, 1, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 0, ..._tl4],
          ..._tl,
        ] =>
        estimateDiagOpenTwoHelp(List.map(List.tl, br), x + 10)
      | [
          [0, ..._tl1],
          [_, 2, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 0, ..._tl4],
          ..._tl,
        ] =>
        estimateDiagOpenTwoHelp(List.map(List.tl, br), x - 10)
      | [
          [_, ..._tl1],
          [_, _, ..._tl2],
          [_, _, _, ..._tl3],
          [_, _, _, _, ..._tl4],
          ..._tl,
        ] =>
        estimateDiagOpenTwoHelp(List.map(List.tl, br), x)
      | _ => x
      };
    };

  /*
   estimateDiagOpenTwo:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses estimateDiagOpenTwoHelp vertically to search for instances of
           open doubles
   Recursion Explanation: you recur on List.tl(br) to vertically iterate by 1
                         row down on the recursive call
   */

  let rec estimateDiagOpenTwo: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [_, _, _, _, ..._tl] =>
        estimateDiagOpenTwo(List.tl(br), estimateDiagOpenTwoHelp(br, 0) + x)
      | _ => x
      };
    };
  //Test cases for estimateDiagOpenTwo
  let board4 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0],
    [0, 2, 1, 0, 0, 0, 0],
    [0, 2, 1, 0, 0, 0, 0],
  ];
  let board5 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0],
    [0, 1, 2, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0, 0],
  ];
  checkExpect(estimateDiagOpenTwo(board4, 0), 10, "case with P1");
  checkExpect(estimateDiagOpenTwo(board5, 0), -10, "case with P2");

  /* estimateOpenTwo:
       input: br, a board
                   x an int
         ouput: the sum of values of other estimates
     */

  let estimateOpenTwo: (board, int) => int =
    (br, x) => {
      estimateDiagOpenTwo(br, x)
      + estimateDiagOpenTwo(vertFlip(br), x)
      + estimateHorzOpenTwo(br, x, 0);
    };

  /*
   estimateHorzCloseTwoHelp:
   input: lst, a row of a board
           value, an integer that serves as an accumulator
           discount, an integer that decreases value based on specific Horziontal
           Close Two cases
   output: For every instance of ( _ X X O ) or ( O X X _ ) in the row, add 10.
           For every instance of ( _ O O X ) or ( X O O _ ) in the row,minus 10.
   Recursion explanation: You recur on List.tl(lst) in order to iterate by
                           1 column at a time horizontally.
   */

  let rec estimateHorzCloseTwoHelp: (list(int), int, int) => int =
    (lst, value, discount) => {
      switch (lst) {
      | [0, 0, 1, 1, 2, ..._tl] =>
        estimateHorzCloseTwoHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [2, 1, 1, 0, 0, ..._tl] =>
        estimateHorzCloseTwoHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [0, 0, 2, 2, 1, ..._tl] =>
        estimateHorzCloseTwoHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )
      | [1, 2, 2, 0, 0, ..._tl] =>
        estimateHorzCloseTwoHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )

      | [_, _, _, _, ..._tl] =>
        estimateHorzCloseTwoHelp(List.tl(lst), value, discount)
      | _ => value
      };
    };

  /*
   estimateHorzCloseTwo:
   input: br, a board
           value, an accumulator
           discount: an integer that decreases value based on specific Horziontal
                   close Two Cases
   output: sums estimateHorzCloseTwoHelp of every row of br
   Recursion explanation: You recur on br's tl in the form [hd, ...tl] so that
                           you iterate down by 1 row at a time.
   */
  let rec estimateHorzCloseTwo: (board, int, int) => int =
    (br, value, discount) => {
      switch (br) {
      | []
      | [[]] => value
      | [hd, ...tl] =>
        estimateHorzCloseTwo(
          tl,
          value + estimateHorzCloseTwoHelp(hd, 0, discount),
          discount + 1,
        )
      };
    };
  //Test cases for estimateHorzCloseTwo
  let board6 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [2, 1, 1, 0, 0, 0, 0],
    [2, 1, 1, 0, 0, 0, 0],
  ];
  let board7 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 2, 2, 0, 0, 0, 0],
    [1, 2, 2, 0, 0, 0, 0],
    [1, 2, 2, 0, 0, 0, 0],
  ];
  let board8 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 2, 2, 0, 0, 0, 0],
    [2, 1, 1, 0, 0, 0, 0],
    [2, 1, 1, 0, 0, 0, 0],
  ];
  checkExpect(estimateHorzCloseTwo(board6, 0, 0), 27, "case with P1");
  checkExpect(estimateHorzCloseTwo(board7, 0, 0), -39, "case with P2");
  checkExpect(estimateHorzCloseTwo(board8, 0, 0), 15, "case with P1 & P2");

  /*
   estimateVertCloseTwo:
   input: br, a board
         value, an integer that serves as an accumulator
   output: transposes br and utilizes estimateHorzCloseTwo to get the vertical
           cases
   */

  let estimateVertCloseTwo: (board, int) => int =
    (br, value) => {
      estimateHorzCloseTwo(transpose(br), value, 0);
    };
  //Test cases for estimateVertCloseTwo
  let board9 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 2, 0, 2, 0, 0],
  ];
  let board10 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 2, 0, 2],
    [0, 0, 2, 0, 2, 0, 2],
    [0, 0, 1, 0, 1, 0, 1],
  ];
  let board11 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 2, 0, 2],
    [0, 0, 1, 0, 2, 0, 2],
    [0, 0, 2, 0, 1, 0, 1],
  ];
  checkExpect(estimateVertCloseTwo(board9, 0), 26, "case with P1");
  checkExpect(estimateVertCloseTwo(board10, 0), -42, "case with P2");
  checkExpect(estimateVertCloseTwo(board11, 0), -18, "case with P1 & P2");

  /*
   estimateDiagCloseTwoHelp:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses 4 rows horizontally to search for instances of closed doubles
   Recursion Explanation: You recur on List.map(List.tl, br) to horizontally shift
                         each of the 4 rows by 1 column on the recursive call
   */

  let rec estimateDiagCloseTwoHelp: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [
          [0, ...__tl1],
          [_, 0, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 1, ..._tl4],
          [_, _, _, _, 2, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseTwoHelp(List.map(List.tl, br), x + 10)
      | [
          [2, ..._tl1],
          [_, 1, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 0, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseTwoHelp(List.map(List.tl, br), x + 10)
      | [
          [0, ..._tl1],
          [_, 0, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 2, ..._tl4],
          [_, _, _, _, 1, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseTwoHelp(List.map(List.tl, br), x - 10)
      | [
          [1, ..._tl1],
          [_, 2, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 0, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseTwoHelp(List.map(List.tl, br), x - 10)
      | [
          [_, ..._tl1],
          [_, _, ..._tl2],
          [_, _, _, ..._tl3],
          [_, _, _, _, ..._tl4],
          [_, _, _, _, _, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseTwoHelp(List.map(List.tl, br), x)
      | _ => x
      };
    };

  /*
   estimateDiagCloseTwo:
   input: br, a board
         x, an accumualtor that stores the output of the procedure
   output: traverses estimateDiagCloseTwoHelp vertically to search for instances of
           open doubles
   Recursion Explanation: you recur on List.tl(br) to vertically iterate by 1
                         row down on the recursive call
   */

  let rec estimateDiagCloseTwo: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [_, _, _, _, _, ..._tl] =>
        estimateDiagCloseTwo(
          List.tl(br),
          estimateDiagCloseTwoHelp(br, 0) + x,
        )
      | _ => x
      };
    };
  //Test cases for estimateDiagCloseTwo
  let board12 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 2, 0],
    [0, 0, 0, 1, 1, 2, 0],
    [0, 0, 1, 1, 1, 1, 0],
    [0, 2, 2, 1, 1, 2, 0],
  ];
  let board13 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 2, 2, 1, 0],
    [0, 0, 2, 2, 2, 2, 0],
    [0, 1, 2, 2, 2, 1, 0],
  ];
  checkExpect(estimateDiagCloseTwo(board12, 0), 10, "case with P1");
  checkExpect(estimateDiagCloseTwo(board13, 0), -10, "case with P2");

  /*
   estimateCloseTwo:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: sums up all close Two cases (horizontal, vertical, diagonal)
   */

  let estimateCloseTwo: (board, int) => int =
    (br, x) => {
      estimateDiagCloseTwo(br, x)
      + estimateDiagCloseTwo(vertFlip(br), x)
      + estimateHorzCloseTwo(br, x, 0)
      + estimateVertCloseTwo(br, x);
    };

  /*
   estimateHorzCloseThreeHelp:
   input: lst, a row of a board
           value, an integer that serves as an accumulator
           discount, an integer that decreases value based on specific Horziontal
           Close Three cases
   output: For every instance of ( _ X X O ) or ( O X X _ ) in the row, add 10.
           For every instance of ( _ O O X ) or ( X O O _ ) in the row,minus 10.
   Recursion explanation: You recur on List.tl(lst) in order to iterate by
                           1 column at a time horizontally.
   */

  let rec estimateHorzCloseThreeHelp: (list(int), int, int) => int =
    (lst, value, discount) => {
      switch (lst) {
      | [0, 1, 1, 1, 2, ..._tl] =>
        estimateHorzCloseThreeHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [2, 1, 1, 1, 0, ..._tl] =>
        estimateHorzCloseThreeHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [0, 2, 2, 2, 1, ..._tl] =>
        estimateHorzCloseThreeHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )
      | [1, 2, 2, 2, 0, ..._tl] =>
        estimateHorzCloseThreeHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )

      | [_, _, _, _, ..._tl] =>
        estimateHorzCloseThreeHelp(List.tl(lst), value, discount)
      | _ => value
      };
    };

  /*
   estimateHorzCloseThree:
   input: br, a board
           value, an accumulator
           discount: an integer that decreases value based on specific Horziontal
                   close Three Cases
   output: sums estimateHorzCloseThreeHelp of every row of br
   Recursion explanation: You recur on br's tl in the form [hd, ...tl] so that
                           you iterate down by 1 row at a time.
   */

  let rec estimateHorzCloseThree: (board, int, int) => int =
    (br, value, discount) => {
      switch (br) {
      | []
      | [[]] => value
      | [hd, ...tl] =>
        estimateHorzCloseThree(
          tl,
          value + estimateHorzCloseThreeHelp(hd, 0, discount),
          discount + 1,
        )
      };
    };
  //Test cases for estimateHorzCloseThree
  let board14 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [2, 1, 1, 1, 0, 0, 0],
    [2, 1, 1, 1, 0, 0, 0],
  ];
  let board15 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 2, 2, 2, 0, 0, 0],
    [1, 2, 2, 2, 0, 0, 0],
    [1, 2, 2, 2, 0, 0, 0],
  ];
  let board16 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 2, 2, 2, 0, 0, 0],
    [2, 1, 1, 1, 0, 0, 0],
    [2, 1, 1, 1, 0, 0, 0],
  ];
  checkExpect(estimateHorzCloseThree(board14, 0, 0), 27, "case with P1");
  checkExpect(estimateHorzCloseThree(board15, 0, 0), -39, "case with P2");
  checkExpect(
    estimateHorzCloseThree(board16, 0, 0),
    15,
    "case with P1 & P2",
  );

  /*
   estimateVertCloseThree:
   input: br, a board
         value, an integer that serves as an accumulator
   output: transposes br and utilizes estimateHorzCloseThree to get the vertical
           cases
   */

  let estimateVertCloseThree: (board, int) => int =
    (br, value) => {
      estimateHorzCloseThree(transpose(br), value, 0);
    };
  //Test cases for estimateVertCloseThree
  let board17 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 2, 0, 2, 0, 0],
  ];
  let board18 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 2, 0, 2],
    [0, 0, 2, 0, 2, 0, 2],
    [0, 0, 2, 0, 2, 0, 2],
    [0, 0, 1, 0, 1, 0, 1],
  ];
  let board19 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 2, 0, 2],
    [0, 0, 1, 0, 2, 0, 2],
    [0, 0, 1, 0, 2, 0, 2],
    [0, 0, 2, 0, 1, 0, 1],
  ];
  checkExpect(estimateVertCloseThree(board17, 0), 26, "case with P1");
  checkExpect(estimateVertCloseThree(board18, 0), -42, "case with P2");
  checkExpect(estimateVertCloseThree(board19, 0), -18, "case with P1 & P2");

  /*
   estimateDiagCloseThreeHelp:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses 4 rows horizontally to search for instances of closed doubles
   Recursion Explanation: You recur on List.map(List.tl, br) to horizontally shift
                         each of the 4 rows by 1 column on the recursive call
   */

  let rec estimateDiagCloseThreeHelp: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [
          [0, ..._tl1],
          [_, 1, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 1, ..._tl4],
          [_, _, _, _, 2, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseThreeHelp(List.map(List.tl, br), x + 10)
      | [
          [2, ..._tl1],
          [_, 1, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 1, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseThreeHelp(List.map(List.tl, br), x + 10)
      | [
          [0, ..._tl1],
          [_, 2, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 2, ..._tl4],
          [_, _, _, _, 1, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseThreeHelp(List.map(List.tl, br), x - 10)
      | [
          [1, ..._tl1],
          [_, 2, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 2, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseThreeHelp(List.map(List.tl, br), x - 10)
      | [
          [_, ..._tl1],
          [_, _, ..._tl2],
          [_, _, _, ..._tl3],
          [_, _, _, _, ..._tl4],
          [_, _, _, _, _, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagCloseThreeHelp(List.map(List.tl, br), x)
      | _ => x
      };
    };

  /*
   estimateDiagCloseThree:
   input: br, a board
         x, an accumualtor that stores the output of the procedure
   output: traverses estimateDiagCloseThreeHelp vertically to search for instances of
           open doubles
   Recursion Explanation: you recur on List.tl(br) to vertically iterate by 1
                         row down on the recursive call
   */

  let rec estimateDiagCloseThree: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [_, _, _, _, _, ..._tl] =>
        estimateDiagCloseThree(
          List.tl(br),
          estimateDiagCloseThreeHelp(br, 0) + x,
        )
      | _ => x
      };
    };
  //Test cases for estimateDiagCloseThree
  let board20 = [
    [0, 0, 0, 0, 0, 0, 0],
    [2, 1, 0, 0, 0, 0, 0],
    [1, 1, 1, 0, 0, 0, 0],
    [1, 2, 2, 1, 0, 0, 0],
    [1, 2, 1, 2, 2, 0, 0],
  ];
  let board21 = [
    [0, 0, 0, 0, 0, 0, 0],
    [1, 2, 0, 0, 0, 0, 0],
    [2, 2, 2, 0, 0, 0, 0],
    [2, 1, 1, 2, 0, 0, 0],
    [2, 1, 2, 1, 1, 0, 0],
  ];
  checkExpect(estimateDiagCloseThree(board20, 0), 10, "case with P1");
  checkExpect(estimateDiagCloseThree(board21, 0), -10, "case with P2");

  /*
   estimateCloseThree:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: sums up all close Three cases (horizontal, vertical, diagonal)
   */

  let estimateCloseThree: (board, int) => int =
    (br, x) => {
      estimateDiagCloseThree(br, x)
      + estimateDiagCloseThree(vertFlip(br), x)
      + estimateHorzCloseThree(br, x, 0)
      + estimateVertCloseThree(br, x);
    };

  /*
   estimateHorzOpenThreeHelp:
   input: lst, a single row of a board
         value, an integer that serves as an accumulator
         discount, an integer that decreases value based on specific Horziontal
                   Open Three Cases
   output:  For every instance of ( _ X X X _ ) in the row, add 10.
           For every instance of ( _ O O O _ ) in the row, subtract 10.
   Recursion explanation: You recur on List.tl(lst) in order to iterate by
                         1 column at a time horizontally.
   */

  let rec estimateHorzOpenThreeHelp: (list(int), int, int) => int =
    (lst, value, discount) => {
      switch (lst) {
      | [0, 1, 1, 1, 0, ..._tl] =>
        estimateHorzOpenThreeHelp(
          List.tl(lst),
          value + 10 + discount,
          discount,
        )
      | [0, 2, 2, 2, 0, ..._tl] =>
        estimateHorzOpenThreeHelp(
          List.tl(lst),
          value - 10 - discount,
          discount,
        )
      | [_, _, _, _, _, ..._] =>
        estimateHorzOpenThreeHelp(List.tl(lst), value, discount)
      | _ => value
      };
    };

  /*
   estimateHorzOpenThree:
   input: br, a board
   output: sums estimateHorzOpenThreeHelp of every row of br
   Recursion explanation: You recur on br's tl in the form [hd, ...tl] so that
                           you iterate down by 1 row at a time.
   */

  let rec estimateHorzOpenThree: (board, int, int) => int =
    (br, value, discount) => {
      switch (br) {
      | []
      | [[]] => value
      | [hd, ...tl] =>
        estimateHorzOpenThree(
          tl,
          value + estimateHorzOpenThreeHelp(hd, 0, discount),
          discount + 3,
        )
      };
    };
  //Test cases for estimateHorzOpenThree
  let board22 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 0, 0, 0],
    [0, 1, 1, 1, 0, 0, 0],
  ];
  let board23 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 0, 0, 0],
    [0, 2, 2, 2, 0, 0, 0],
    [0, 2, 2, 2, 0, 0, 0],
  ];
  let board24 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 0, 0, 0],
    [0, 1, 1, 1, 0, 0, 0],
    [0, 1, 1, 1, 0, 0, 0],
  ];
  checkExpect(estimateHorzOpenThree(board22, 0, 0), 41, "case with P1");
  checkExpect(estimateHorzOpenThree(board23, 0, 0), -57, "case with P2");
  checkExpect(estimateHorzOpenThree(board24, 0, 0), 25, "case with P1 & P2");

  /*
   estimateDiagOpenThreeHelp:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses 4 rows horizontally to search for instances of open doubles
   Recursion explanation: You recur on List.map(List.tl, br) to horizontally shift
                         each of the 4 rows by 1 column on the recursive call
   */

  let rec estimateDiagOpenThreeHelp: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [
          [0, ..._tl1],
          [_, 1, ..._tl2],
          [_, _, 1, ..._tl3],
          [_, _, _, 1, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagOpenThreeHelp(List.map(List.tl, br), x + 10)
      | [
          [0, ..._tl1],
          [_, 2, ..._tl2],
          [_, _, 2, ..._tl3],
          [_, _, _, 2, ..._tl4],
          [_, _, _, _, 0, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagOpenThreeHelp(List.map(List.tl, br), x - 10)
      | [
          [_, ..._tl1],
          [_, _, ..._tl2],
          [_, _, _, ..._tl3],
          [_, _, _, _, ..._tl4],
          [_, _, _, _, _, ..._tl5],
          ..._tl,
        ] =>
        estimateDiagOpenThreeHelp(List.map(List.tl, br), x)
      | _ => x
      };
    };

  /*
   estimateDiagOpenThree:
   input: br, a board
         x, an accumulator that stores the output of the procedure
   output: traverses estimateDiagOpenThreeHelp vertically to search for instances of
           open doubles
   Recursion Explanation: you recur on List.tl(br) to vertically iterate by 1
                         row down on the recursive call
   */

  let rec estimateDiagOpenThree: (board, int) => int =
    (br, x) => {
      switch (br) {
      | [_, _, _, _, ..._tl] =>
        estimateDiagOpenThree(
          List.tl(br),
          estimateDiagOpenThreeHelp(br, 0) + x,
        )
      | _ => x
      };
    };
  //Test cases for estimateDiagOpenThree
  let board25 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0],
    [0, 2, 1, 0, 0, 0, 0],
    [0, 1, 2, 1, 0, 0, 0],
    [0, 2, 1, 1, 0, 0, 0],
  ];
  let board26 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0],
    [0, 1, 2, 0, 0, 0, 0],
    [0, 2, 1, 2, 0, 0, 0],
    [0, 1, 2, 2, 0, 0, 0],
  ];
  checkExpect(estimateDiagOpenThree(board25, 0), 10, "case with P1");
  checkExpect(estimateDiagOpenThree(board26, 0), -10, "case with P2");

  /* estimateOpenThree:
       input: br, a board
                   x an int
         ouput: the sum of values of other estimates
     */

  let estimateOpenThree: (board, int) => int =
    (br, x) => {
      estimateDiagOpenThree(br, x)
      + estimateDiagOpenThree(vertFlip(br), x)
      + estimateHorzOpenThree(br, x, 0);
    };

  /*
   estimateHorzEdgeTwoHelper1:
   input: lst, a single row of a board
   output: if the particular row is in the form O O _ _ ..., returns 10
                                 is in the form X X _ _ ..., returns -10
   */
  let estimateHorzEdgeTwoHelper1: list(int) => int =
    lst => {
      switch (lst) {
      | [1, 1, 0, 0, ..._tl] => 10
      | [2, 2, 0, 0, ..._tl] => (-10)
      | _ => 0
      };
    };
  /*
   estimateHorzEdgeTwoHelper2:
   input: br, a board
   output: sums estimateHorzEdgeTwoHelper1 of every row of br
   */
  let rec estimateHorzEdgeTwoHelper2: board => int =
    br => {
      switch (br) {
      | []
      | [[]] => 0
      | [hd, ...tl] =>
        estimateHorzEdgeTwoHelper1(hd) + estimateHorzEdgeTwoHelper2(tl)
      };
    };

  /*
   estimateHorzEdgeTwo:
   input: br, a board
   output: applies estiamteHorzEdgeTwoHelper2 to both br and the vertFlip of br
   */
  let estimateHorzEdgeTwo: board => int =
    br =>
      estimateHorzEdgeTwoHelper2(br)
      + estimateHorzEdgeTwoHelper2(vertFlip(br));

  //Test cases for estimateHorzEdgeTwo
  let board27 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 1, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 1, 1],
  ];
  let board28 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [2, 2, 0, 0, 0, 2, 2],
    [2, 2, 0, 0, 0, 2, 2],
  ];
  let board29 = [
    [1, 1, 0, 0, 0, 0, 0],
    [1, 1, 0, 0, 0, 0, 0],
    [1, 1, 0, 0, 0, 0, 0],
    [2, 2, 0, 0, 0, 2, 2],
    [2, 2, 0, 0, 0, 2, 2],
  ];
  checkExpect(estimateHorzEdgeTwo(board27), 60, "case with P1");
  checkExpect(estimateHorzEdgeTwo(board28), -40, "case with P2");
  checkExpect(estimateHorzEdgeTwo(board29), -10, "case with P1 & P2");

  /*
   estimateVertEdgeTwo:
   input: br, a board
   output: gets the estimates of the cases in which there are two Os or Xs stacked
           vertically
   */
  let estimateVertEdgeTwo: board => int =
    br => estimateHorzEdgeTwoHelper2(vertFlip(transpose(br)));

  //Test cases for estsimateVertEdgeTwo:
  let board30 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 1, 0, 0, 0],
    [0, 1, 0, 1, 0, 0, 0],
  ];
  let board31 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 2, 0, 0, 0],
    [0, 2, 0, 2, 0, 0, 0],
  ];
  let board32 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 2, 1, 2, 2, 0],
    [0, 1, 2, 1, 2, 2, 0],
  ];
  checkExpect(estimateVertEdgeTwo(board30), 20, "case with P1");
  checkExpect(estimateVertEdgeTwo(board31), -20, "case with P2");
  checkExpect(estimateVertEdgeTwo(board32), -10, "case with P1 & P2");

  /*
   estimateHorzEdgeThreeHelper1:
   input: lst, a single row of a board
   output: if the particular row is in the form O O O _ ..., returns 10
                                 is in the form X X X _ ..., returns -10
   */
  let estimateHorzEdgeThreeHelper1: list(int) => int =
    lst => {
      switch (lst) {
      | [1, 1, 1, 0, ..._tl] => 10
      | [2, 2, 2, 0, ..._tl] => (-10)
      | _ => 0
      };
    };
  /*
   estimateHorzEdgeThreeHelper2:
   input: br, a board
   output: sums estimateHorzEdgeThreeHelper1 of every row of br
   */
  let rec estimateHorzEdgeThreeHelper2: board => int =
    br => {
      switch (br) {
      | []
      | [[]] => 0
      | [hd, ...tl] =>
        estimateHorzEdgeThreeHelper1(hd) + estimateHorzEdgeThreeHelper2(tl)
      };
    };
  /*
   estimateHorzEdgeThree:
   input: br, a board
   output: applies estiamteHorzEdgeThreeHelper2 to both br and the vertFlip of br
   */
  let estimateHorzEdgeThree: board => int =
    br => {
      estimateHorzEdgeThreeHelper2(br)
      + estimateHorzEdgeThreeHelper2(vertFlip(br));
    };

  //Test cases for estimateHorzEdgeThree
  let board33 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 0, 1, 1, 1],
    [1, 1, 1, 0, 1, 1, 1],
    [1, 1, 1, 0, 1, 1, 1],
  ];
  let board34 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [2, 2, 2, 0, 2, 2, 2],
    [2, 2, 2, 0, 2, 2, 2],
  ];
  let board35 = [
    [1, 1, 1, 0, 0, 0, 0],
    [1, 1, 1, 0, 0, 0, 0],
    [1, 1, 1, 0, 0, 0, 0],
    [2, 2, 2, 0, 2, 2, 2],
    [2, 2, 2, 0, 2, 2, 2],
  ];
  checkExpect(estimateHorzEdgeThree(board33), 60, "case with P1");
  checkExpect(estimateHorzEdgeThree(board34), -40, "case with P2");
  checkExpect(estimateHorzEdgeThree(board35), -10, "case with P1 & P2");

  /*
   estimateVertEdgeThree:
   input: br, a board
   output: gets the estimates of the cases in which there are two Os or Xs stacked
           vertically
   */
  let estimateVertEdgeThree: board => int =
    br => {
      estimateHorzEdgeThreeHelper2(vertFlip(transpose(br)));
    };
  //Test cases for estimateVertEdgeThree:
  let board36 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 1, 0, 0, 0],
    [0, 1, 0, 1, 0, 0, 0],
    [0, 1, 0, 1, 0, 0, 0],
  ];
  let board37 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 2, 0, 0, 0],
    [0, 2, 0, 2, 0, 0, 0],
    [0, 2, 0, 2, 0, 0, 0],
  ];
  let board38 = [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 1, 2, 1, 2, 2, 0],
    [0, 1, 2, 1, 2, 2, 0],
    [0, 1, 2, 1, 2, 2, 0],
  ];
  checkExpect(estimateVertEdgeThree(board36), 20, "case with P1");
  checkExpect(estimateVertEdgeThree(board37), -20, "case with P2");
  checkExpect(estimateVertEdgeThree(board38), -10, "case with P1 & P2");

  let estimateEdgeThree: board => int =
    br => {
      estimateVertEdgeThree(br) + estimateHorzEdgeThree(br);
    };

  let estimateEdgeTwo: board => int =
    br => {
      estimateVertEdgeTwo(br) + estimateHorzEdgeTwo(br);
    };

  /* estimates the value of a given state (static evaluation)
     input: a board br
     output: a float which is a static evaulation of the board state
     */
  let estimate: board => float =
    br => {
      10.
      *. float_of_int(estimateCloseThree(br, 0))
      +. 20.
      *. float_of_int(estimateOpenThree(br, 0))
      +. 3.
      *. float_of_int(estimateCloseTwo(br, 0))
      +. 5.
      *. float_of_int(estimateOpenTwo(br, 0))
      +. 3.
      *. float_of_int(estimateEdgeTwo(br))
      +. 5.
      *. float_of_int(estimateEdgeThree(br));
    };

  /*
   input: state inState
   output: float represeting estimate evaluation of static board state.
   */

  let estimateValue: state => float =
    inState => {
      switch (inState) {
      | State(Ongoing(P1), n) => estimate(n)
      | State(Win(P1), _) => 100000.
      | State(Draw, _) => 0.0
      | State(Ongoing(P2), n) => estimate(n)
      | State(Win(P2), _) => (-100000.)
      };
    };
};

module MyGame: Game = Connect4;
open Connect4;

/* test cases */

checkExpect(stringOfMove(Move(2)), "2", "stringOfMove works");
checkExpect(stringOfMove(Move(-2)), "-2", "stringOfMove -2 works");
checkExpect(stringOfPlayer(P1), "P1", "stringofplayer works");
checkExpect(stringOfPlayer(P2), "P2", "stringofplayer works");
checkExpect(
  moveOfString("4", State(Ongoing(P1), [[0, 0, 0, 0, 0, 0, 0]])),
  Connect4.Move(4),
  "moveOfString is functional",
);
checkError(
  () => moveOfString("10", State(Ongoing(P1), [[0, 0, 0, 0, 0, 0, 0]])),
  "not legal",
);
checkError(
  () => moveOfString("lol", State(Ongoing(P1), [[0, 0, 0, 0, 0, 0, 0]])),
  "please insert an integer",
);
checkExpect(
  nextState(
    State(
      Ongoing(P1),
      [
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
      ],
    ),
    Move(3),
  ),
  State(
    Ongoing(P2),
    [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
    ],
  ),
  "nextState on basic case",
);
checkExpect(
  nextState(
    State(
      Ongoing(P1),
      [
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 1, 1, 0, 0],
      ],
    ),
    Move(2),
  ),
  State(
    Win(P1),
    [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 1, 1, 1, 1, 0, 0],
    ],
  ),
  "nextState on Horz Win case",
);
checkExpect(
  nextState(
    State(
      Ongoing(P1),
      [
        [0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
      ],
    ),
    Move(1),
  ),
  State(
    Win(P1),
    [
      [0, 0, 0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
    ],
  ),
  "nextState on Vert Win case",
);
checkExpect(
  nextState(
    State(
      Ongoing(P2),
      [
        [0, 0, 0, 2, 0, 0, 0],
        [0, 0, 0, 0, 2, 0, 0],
        [1, 0, 0, 0, 0, 2, 0],
        [1, 0, 0, 0, 0, 2, 0],
        [1, 0, 0, 0, 0, 0, 2],
      ],
    ),
    Move(7),
  ),
  State(
    Win(P2),
    [
      [0, 0, 0, 2, 0, 0, 0],
      [0, 0, 0, 0, 2, 0, 0],
      [1, 0, 0, 0, 0, 2, 0],
      [1, 0, 0, 0, 0, 2, 2],
      [1, 0, 0, 0, 0, 0, 2],
    ],
  ),
  "nextState on Diag Win case",
);
checkExpect(
  legalMoves(
    State(
      Ongoing(P1),
      [
        [1, 1, 1, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0],
      ],
    ),
  ),
  [Move(7)],
  "legals moves :D",
);
checkError(() => legalMoves(State(Ongoing(P1), [[]])), "empty board");
checkExpect(
  legalMoves(State(Ongoing(P1), [[1, 1, 1, 1, 1, 1, 1]])),
  [],
  "empty list of legal moves if not legals",
);
checkExpect(
  initialState("5 7"),
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ],
  ),
  "intial state is created sucesfully",
);
checkExpect(
  initialState("3 3"),
  State(Ongoing(P1), [[0, 0, 0], [0, 0, 0], [0, 0, 0]]),
  "intial state 2 is created sucesfully",
);
checkExpect(stringOfMove(Move(3)), "3", "stringOfMove passes");

checkExpect(gameStatus(State(Draw, [[]])), Draw, "gameStatus pass");
