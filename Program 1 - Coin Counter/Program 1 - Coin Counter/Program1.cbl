       identification division.
       program-id. Program1.
       AUTHOR. JOSHUA COLE.
               KELTON ESSIG.
               JOHN SCHLANGER.



       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COIN-FILE    ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COUNTER-FILE ASSIGN TO 'output.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       data division.
       FILE SECTION.
       FD  COIN-FILE
           RECORD CONTAINS 12 CHARACTERS
           DATA RECORD IS COIN-IN.
       01  COIN-IN.
           05  PENNIES         PIC 9(3).
           05  NICKELS         PIC 9(3).
           05  DIMES           PIC 9(3).
           05  QUARTERS        PIC 9(3).

       FD  COUNTER-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS PRINT-LINE.
       01  PRINT-LINE          PIC X(80).
       
       working-storage section.
       01  TITLE-HEADER.
           05  FILLER          PIC X(30)   VALUE SPACES.
           05  R-HEADER        PIC X(19)   VALUE 'Coin Counter Report'.
           05  FILLER          PIC X(31)   VALUE SPACES.

      *header for columns of output file
       01  DENOM-HEADER.
           05  FILLER          PIC X(20)   VALUE SPACES.
           05  P-HEADER        PIC X(8)    VALUE 'PENNIES:'.
           05  FILLER          PIC X(7)    VALUE SPACES.
           05  N-HEADER        PIC X(8)    VALUE 'NICKELS:'.
           05  FILLER          PIC X(7)    VALUE SPACES.
           05  D-HEADER        PIC X(6)    VALUE 'DIMES:'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  Q-HEADER        PIC X(9)    VALUE 'QUARTERS:'.
           05  FILLER          PIC X(6)    VALUE SPACES.


      *beginnings of individual rows of the output files
       01  INIT-LINE-HDR
           05  INIT            PIC X(15)   VALUE 'INITIAL AMOUNT:'.
           05  FILLER          PIC X(5)    VALUE SPACES.

       01  ROLL-LINE-HDR
           05  ROLL            PIC X(16)   VALUE 'NUMBER OF ROLLS:'.
           05  FILLER          PIC X(4)    VALUE SPACES.

       01  REM-LINE-HDR
           05  REM             PIC X(15)   VALUE 'REMAINING AMOUNT:'.
           05  FILLER          PIC X(5)    VALUE SPACES.


      * Units for the output lines
       01  AMOUNTS
           05  P-AMT           PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  N-AMT           PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  D-AMT           PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  Q-AMT           PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE SPACES.

      * Offset used for spacing the numbers in the output file
       01  AMT-OFFSET          PIC X(12)   VALUE SPACES.
      * Generic variable for numbers if needed
       01  NUM                 PIC X(3)    VALUE SPACES.
      * Flag for signaling EoF
       01  DATA-REMAINS        PIC X       VALUE 'Y'.

       procedure division.
       PROCESS-COIN-FILES.
           OPEN INPUT  COIN-FILE
                OUTPUT COUNTER-FILE.
           READ    COIN-FILE
               AT END MOVE 'N' TO DATA-REMAINS
           END-READ.
      * TO-DO: Consider a case for an empty file output shows error
           PERFORM WRITE-HEADERS.
           PERFORM PROCESS-COINS
               UNTIL DATA-REMAINS = 'N'.
           CLOSE   COIN-FILE
                   COUNTER-FILE.
           STOP RUN.

       WRITE-HEADERS.
           MOVE TITLE-HEADER TO PRINT-LINE.
           WRITE PRINT-LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.
           MOVE DENOM-HEADER TO PRINT-LINE.
           WRITE PRINT-LINE.
       
       PROCESS-COINS.
      * TO DO: Math goes here :D ... until then, for testing files
           MOVE 'N' TO DATA-REMAINS.

       end program Program1.