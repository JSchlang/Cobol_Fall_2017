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
           05  REM             PIC X(17)   VALUE 'REMAINING AMOUNT:'.
           05  FILLER          PIC X(3)    VALUE SPACES.

       01  BAD-RECORD-LINE
           05  BAD-REC         PIC X(52)   VALUE
               'ERROR: The given input record contains invalid data.'.
           05  FILLER          PIC X(28)   VALUE SPACES.


      * Units for the output lines
       01  ROLL-AMOUNTS
           05  P-ROLL          PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  N-ROLL          PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  D-ROLL          PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  Q-ROLL          PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
       01  REM-AMOUNTS
           05  P-REM           PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  N-REM           PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  D-REM           PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.
           05  Q-REM           PIC 999     VALUE ZEROES.
           05  FILLER          PIC X(12)   VALUE SPACES.

      * Offset used for spacing the numbers in the output file
       01  AMT-OFFSET          PIC X(12)   VALUE SPACES.
      * Generic variable for numbers if needed
       01  NUM                 PIC 9(3)    VALUE ZEROES.
      * Flag for signaling EoF
       01  DATA-REMAINS        PIC X       VALUE 'Y'.
       01  DATA-FLAG           PIC 9       VALUE 0

       procedure division.
       PROCESS-COIN-FILES.
           OPEN INPUT  COIN-FILE
                OUTPUT COUNTER-FILE.
           READ    COIN-FILE
               AT END MOVE 'N' TO DATA-REMAINS
           END-READ.

           DISPLAY 'Processing Input File...'.
           MOVE TITLE-HEADER TO PRINT-LINE.
           WRITE PRINT-LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.

      * TO-DO: Consider a case for an empty file output shows error
           PERFORM PROCESS-COINS
               UNTIL DATA-REMAINS = 'N'.
           CLOSE   COIN-FILE
                   COUNTER-FILE.

           DISPLAY
             'Processing complete. Please check the output file.'.
           STOP RUN.

       WRITE-HEADERS.
           MOVE DENOM-HEADER TO PRINT-LINE.
           WRITE PRINT-LINE.
       
       PROCESS-COINS.
           DISPLAY '.'.
           MOVE 1 TO DATA-FLAG.

           PERFORM PROCESS-PENNIES.
           PERFORM PROCESS-NICKELS.
           PERFORM PROCESS-DIMES.
           PERFORM PROCESS-QUARTERS.

           PERFORM WRITE-RECORD.

           READ    COIN-FILE
               AT END MOVE 'N' TO DATA-REMAINS
           END-READ.

       PROCESS-PENNIES.
           IF PENNIES IS NUMERIC
               DIVIDE PENNIES BY 50 GIVING P-ROLL REMAINDER P-REM
           ELSE
               MOVE 0 TO DATA-FLAG
           END-IF.

       PROCESS-NICKELS.
           IF DATA-FLAG = 1
               IF NICKELS IS NUMERIC
                   DIVIDE NICKELS BY 40 GIVING N-ROLL REMAINDER N-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       PROCESS-DIMES.
           IF DATA-FLAG = 1
               IF DIMES IS NUMERIC
                   DIVIDE DIMES BY 50 GIVING D-ROLL REMAINDER D-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       PROCESS-QUARTERS.
           IF DATA-FLAG = 1
               IF QUARTERS IS NUMERIC
                   DIVIDE QUARTERS BY 40 GIVING Q-ROLL REMAINDER Q-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       WRITE-RECORD.
           IF DATA-FLAG = 1
               PERFORM WRITE-HEADERS
               MOVE SPACES TO PRINT-LINE
               STRING  INIT-LINE-HDR DELIMITED BY SIZE
                       PENNIES DELIMITED BY SIZE
                       AMT-OFFSET DELIMITED BY SIZE
                       NICKELS DELIMITED BY SIZE
                       AMT-OFFSET DELIMITED BY SIZE
                       DIMES DELIMITED BY SIZE
                       AMT-OFFSET DELIMITED BY SIZE
                       QUARTERS DELIMITED BY SIZE
                       AMT-OFFSET DELIMITED BY SIZE
               INTO PRINT-LINE
               WRITE PRINT-LINE
               STRING  ROLL-LINE-HDR DELIMITED BY SIZE
                       ROLL-AMOUNTS DELIMITED BY SIZE
               INTO PRINT-LINE
               WRITE PRINT-LINE
               STRING  REM-LINE-HDR DELIMITED BY SIZE
                       REM-AMOUNTS DELIMITED BY SIZE
               INTO PRINT-LINE
               WRITE PRINT-LINE
           ELSE
               MOVE BAD-RECORD-LINE TO PRINT-LINE
               WRITE PRINT-LINE
           END-IF.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.


       end program Program1.