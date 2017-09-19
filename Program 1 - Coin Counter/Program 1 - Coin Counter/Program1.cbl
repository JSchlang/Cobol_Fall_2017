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
      * Record to hold the data read from the input file
       01  COIN-IN.
           05  PENNIES         PIC 9(3).
           05  NICKELS         PIC 9(3).
           05  DIMES           PIC 9(3).
           05  QUARTERS        PIC 9(3).

       FD  COUNTER-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS PRINT-LINE.
      * Record to hold data being written to the output file
       01  PRINT-LINE          PIC X(80).
       
       working-storage section.
      * Output file title header
       01  TITLE-HEADER.
           05  FILLER          PIC X(30)   VALUE SPACES.
           05  R-HEADER        PIC X(19)   VALUE 'Coin Counter Report'.
           05  FILLER          PIC X(31)   VALUE SPACES.

      *header for denomination columns of output file
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

      * Line to indicate bad input for thw output file
       01  BAD-RECORD-LINE
           05  BAD-REC         PIC X(52)   VALUE
               'ERROR: The given input record contains invalid data.'.
           05  FILLER          PIC X(28)   VALUE SPACES.

      * Units for # of rolls and # of remaining coins for output lines
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
      * Flag for signaling EoF and good/bad data
       01  DATA-REMAINS        PIC X       VALUE 'Y'.
       01  DATA-FLAG           PIC 9       VALUE 0

       procedure division.
       PROCESS-COIN-FILES.
      * Open the files for processing
           OPEN INPUT  COIN-FILE
                OUTPUT COUNTER-FILE. 
      * Perform a priming read of the input file
           READ    COIN-FILE
               AT END MOVE 'N' TO DATA-REMAINS
           END-READ.
      *    Check to see if the input file is empty to begin with
           IF DATA-REMAINS = 'N'
               MOVE 'ERROR: Input file is empty. Nothing to process'
                 TO PRINT-LINE
               WRITE PRINT-LINE
           ELSE
      *        Write the title header to the output file
               MOVE TITLE-HEADER TO PRINT-LINE
               WRITE PRINT-LINE
               MOVE SPACES TO PRINT-LINE
               WRITE PRINT-LINE
           END-IF.
      * Console splash text to let the user know it's working on it
           DISPLAY 'Processing Input File...'.
      * Process record lines of the input file until there are no more
           PERFORM PROCESS-COINS
               UNTIL DATA-REMAINS = 'N'.
           CLOSE   COIN-FILE
                   COUNTER-FILE.
      * Console splash text to show completion of processing
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
      * Read next record from input file
           READ    COIN-FILE
               AT END MOVE 'N' TO DATA-REMAINS
           END-READ.

       PROCESS-PENNIES.
      *    Test for bad data from input, calculate # rolls if good
           IF PENNIES IS NUMERIC
               DIVIDE PENNIES BY 50 GIVING P-ROLL REMAINDER P-REM
           ELSE
               MOVE 0 TO DATA-FLAG
           END-IF.

       PROCESS-NICKELS.
      *    If data from previous process was good,
      *    Test for bad data from input, calculate # rolls if good 
           IF DATA-FLAG = 1
               IF NICKELS IS NUMERIC
                   DIVIDE NICKELS BY 40 GIVING N-ROLL REMAINDER N-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       PROCESS-DIMES.
      *    If data from previous process was good,
      *    Test for bad data from input, calculate # rolls if good 
           IF DATA-FLAG = 1
               IF DIMES IS NUMERIC
                   DIVIDE DIMES BY 50 GIVING D-ROLL REMAINDER D-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       PROCESS-QUARTERS.
      *    If data from previous process was good,
      *    Test for bad data from input, calculate # rolls if good 
           IF DATA-FLAG = 1
               IF QUARTERS IS NUMERIC
                   DIVIDE QUARTERS BY 40 GIVING Q-ROLL REMAINDER Q-REM
               ELSE
                   MOVE 0 TO DATA-FLAG
               END-IF
           END-IF.

       WRITE-RECORD.
           IF DATA-FLAG = 1
      *        Write the denomination headers to the output
               PERFORM WRITE-HEADERS
               MOVE SPACES TO PRINT-LINE
      *        Fill output line with initial coin data and write it
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
      *        Fill output line with coin roll data and write it
               STRING  ROLL-LINE-HDR DELIMITED BY SIZE
                       ROLL-AMOUNTS DELIMITED BY SIZE
               INTO PRINT-LINE
               WRITE PRINT-LINE
      *        Fill output line with remaining coin data and write it
               STRING  REM-LINE-HDR DELIMITED BY SIZE
                       REM-AMOUNTS DELIMITED BY SIZE
               INTO PRINT-LINE
               WRITE PRINT-LINE
           ELSE
      *        if any data was bad, default to writing bad record line
               MOVE BAD-RECORD-LINE TO PRINT-LINE
               WRITE PRINT-LINE
           END-IF.
      *    Add blank line to output file for neatness of records
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.


       end program Program1.