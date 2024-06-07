       IDENTIFICATION DIVISION.
       PROGRAM-ID. tictactoe.
       AUTHOR. Trey Bastian.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Player.
         02 Player1.
             03 Turn PIC 9 value 1.
         02 Player2.
             03 Turn PIC 9 VALUE ZEROS.
         02 PlayerMove PIC 99 VALUE ZEROS.
               88 ValidInput VALUE 1 THRU 9.
         02 Won PIC 9 VALUE ZERO.
               88 Player1Win VALUE 1.
               88 Player2Win VALUE 2.
               88 Stalemate VALUE 3.

       01 Board OCCURS 9 TIMES.
          02 State PIC X VALUE "N".
          02 Score PIC 9 VALUE ZEROS.

       01 TMP PIC 99 VALUE 0.
       01 TMP2 PIC 99 VALUE 0.
       01 TMP3 PIC 99 VALUE 0.
       01 TMP4 PIC 99 VALUE 0.
       01 BoardHasBlanks PIC 9 VALUE 0.

       PROCEDURE DIVISION.

         Intro SECTION.
         DISPLAY "Welcome to Trey's Dev Shed Tic Tac Toe Extravaganza!".

         Setup SECTION.
           MOVE 2 TO Score OF Board(1).
           MOVE 7 TO Score OF Board(2).
           MOVE 6 TO Score OF Board(3).
           MOVE 9 TO Score OF Board(4).
           MOVE 5 TO Score OF Board(5).
           MOVE 1 TO Score OF Board(6).
           Move 4 TO Score OF Board(7).
           MOVE 3 TO Score OF Board(8).
           MOVE 8 TO Score OF Board(9).

         Game SECTION.
             PERFORM UNTIL Player1Win OR Player2Win OR Stalemate

               PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP > 3
                 DISPLAY "----------------"
                 DISPLAY "|" WITH NO ADVANCING
                 PERFORM VARYING TMP2 FROM 1 BY 1 UNTIL TMP2 > 3

                   COMPUTE TMP3 = (TMP * 3) + TMP2 - 3

                   IF State OF Board(TMP3) = "N" THEN
                     DISPLAY " " TMP3 " |" WITH NO ADVANCING
                   ELSE
                     DISPLAY " " State OF Board(TMP3) " |" 
                     WITH NO ADVANCING
                   END-IF
                 END-PERFORM
                 DISPLAY SPACE
               END-PERFORM
               DISPLAY "---------------"

               PERFORM UNTIL ValidInput             
                 IF Turn OF Player1 = 1 THEN
                 DISPLAY "Player 1 " WITH NO ADVANCING
               ELSE
                 DISPLAY "Player 2 " WITH NO ADVANCING
               END-IF

               DISPLAY "make your move [1-9]: " WITH NO ADVANCING
               ACCEPT PlayerMove 

               IF ValidInput THEN
                 
                 IF State OF Board(PlayerMove) EQUAL "N"
                   EVALUATE 1 
                     WHEN Turn OF Player1
                       Move "X" TO State OF Board(PlayerMove) 

                       MOVE 0 TO Turn OF Player1
                       MOVE 1 TO Turn OF PLayer2

                     WHEN Turn OF Player2
                       MOVE "O" TO State OF Board(PlayerMove) 

                       MOVE 1 TO Turn OF Player1
                       MOVE 0 TO Turn OF Player2
                    END-EVALUATE
                 ELSE
                   MOVE 0 TO PlayerMove
                   DISPLAY "Invalid Move, Please Try Again"
               ELSE
                 DISPLAY "Invalid Move, Please Try Again"
               END-IF
             END-PERFORM
             MOVE 0 TO PlayerMove

      * Check Winners
             MOVE 0 TO BoardHasBlanks
             PERFORM VARYING TMP FROM 1 BY 1 UNTIL TMP = 9 OR
               Player1Win OR Player2Win 
               IF State of Board(TMP) = "N" THEN
                 MOVE 1 TO BoardHasBlanks
               END-IF
               PERFORM VARYING TMP2 FROM 1 BY 1 UNTIL TMP2 = 9 OR
                 Player1Win OR Player2Win
                 PERFORM VARYING TMP3 FROM 1 BY 1 UNTIL TMP3 = 9 OR 
                   Player1Win OR Player2Win
                       
                   IF TMP NOT EQUAL TMP2 AND TMP NOT EQUAL TMP3 AND
                     TMP2 NOT EQUAL TMP3 THEN

                     ADD Score OF Board(TMP) TO Score OF Board(TMP2)
                     GIVING TMP4
                     ADD Score OF Board(TMP3) To TMP4
                     IF TMP4 = 15 THEN 
                       EVALUATE 0 
                         WHEN Turn OF Player1
                           IF State OF Board(TMP) EQUAL "X" AND State 
                             OF Board(TMP2) EQUAL "X" 
                             AND STATE OF BOARD(TMP3) EQUAL "X" THEN
                               SET Player1Win TO TRUE 
                            END-IF
                         WHEN Turn OF Player2
                           IF State OF Board(TMP) EQUAL "O" AND State 
                             OF Board(TMP2) EQUAL "O" AND STATE 
                             OF BOARD(TMP3) EQUAL "O" THEN
                             SET Player2Win TO TRUE 
                           END-IF
                       END-EVALUATE
                    END-IF
                   END-IF
                  END-PERFORM
                END-PERFORM
              END-PERFORM

              IF BoardHasBlanks = 0 THEN
                Set Stalemate to TRUE
              END-IF
                

             END-PERFORM.
         Outro SECTION.
             EVALUATE TRUE
               WHEN Player1Win DISPLAY "CONGRATS PLAYER 1 Has Won!"
               WHEN Player2Win DISPLAY "CONGRATS PLAYER 2 Has Won!"
               WHEN Stalemate DISPLAY "No one won!" 
              END-EVALUATE
              STOP RUN.
