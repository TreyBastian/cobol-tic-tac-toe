       IDENTIFICATION DIVISION.
       PROGRAM-ID. tictactoe.
       AUTHOR. Trey Bastian.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Player1.
           02 Score PIC 99 VALUE ZEROS.
             88 Win VALUE 15.
           02 Turn PIC 9 value 1.
       01 Player2.
           02 Score PIC 99 VALUE ZEROES.
             88 Win VALUE 15.
           02 Turn PIC 9 value ZEROS.

       01 PlayerMove PIC 9 VALUE ZEROS.
       01 Board.
         02 Rows OCCURS 3 TIMES.
           03 Entries OCCURS 3 TIMES.
             04 EntryScore PIC 9 VALUE ZEROS.
             04 State PIC Z Value "_".

       01 InitRowIdx PIC S9 VALUE 1.
       01 AccessRowIdx PIC 9 VALUE 0.
       01 AccessEntryIdx PIC 9  VALUE 0.
       01 InitEntryIdx PIC S9 VALUE 2.
       01 InitScoreValue PIC 99 VALUE 1.
       01 InitDivideResult PIC S99 VALUE ZEROS. 
       01 InitDivideRemainder PIC 99 VALUE ZEROS.
       01 IdxRow PIC 9 VALUE 1.
       01 IdxCol PIC 9 VALUE 1.
       01 InitAdd3 PIC 99 VALUE 0.
       PROCEDURE DIVISION.

       Intro SECTION.
       DISPLAY "Welcome to Trey's Dev Shed Tic Tac Toe Extravaganza!".

       Setup SECTION.
              PERFORM VARYING InitScoreValue FROM 1 BY 1 UNTIL
                InitScoreValue > 9 
                COMPUTE AccessRowIdx = InitRowIdx + 1
                COMPUTE AccessEntryIdx = InitEntryIdx + 1
                MOVE InitScoreValue TO 
                   EntryScore(AccessRowIdx AccessEntryIdx)                

                DIVIDE InitScoreValue BY 3 GIVING InitDivideResult
                REMAINDER InitDivideRemainder 

                IF InitDivideRemainder = 0 THEN
                  COMPUTE InitEntryIdx = InitEntryIdx - 1
                ELSE
                   COMPUTE InitRowIdx = InitRowIdx - 1
                   COMPUTE InitEntryIdx = InitEntryIdx + 1
                END-IF

                COMPUTE InitAdd3 = InitRowIdx + 3
                DIVIDE InitAdd3 BY 3 GIVING InitDivideResult REMAINDER
                InitDivideRemainder 
                COMPUTE InitRowIdx = InitDivideRemainder

                COMPUTE InitAdd3 = InitEntryIdx + 3
                DIVIDE InitAdd3 BY 3 GIVING InitDivideResult REMAINDER
                InitDivideRemainder
                COMPUTE InitEntryIdx = InitDivideRemainder
              END-PERFORM.
       Game SECTION.
           PERFORM UNTIL Win OF Score OF Player1 OR Win OF Score OF
             Player2
             PERFORM VARYING IdxRow FROM 1 BY 1 UNTIL IdxRow > 3
               PERFORM VARYING IdxCol FROM 1 BY 1 UNTIL IdxCol > 3
                 Display EntryScore(IdxRow, IdxCol)
               END-PERFORM
             END-PERFORM
             SET Win of Score of Player1 TO true
      * Accept Input
      * Validate Input
      * Make Move
      * Add Score
           END-PERFORM.

       Outro SECTION.
           IF Win OF Score OF Player1 THEN
               DISPLAY "CONGRATS PLAYER 1 Has Won!"
             ELSE
               DISPLAY "CONGRATS PLAYER 2 Has Won!"
            END-IF
        STOP RUN.
