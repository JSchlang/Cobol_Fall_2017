       IDENTIFICATION DIVISION.
       PROGRAM-ID. ColeEssigSchlanger.
       AUTHOR. JOHN SCHLANGER.
               KELTON ESSIG.
               JOSH COLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILEIN
            RECORD CONTAINS 33 CHARACTERS
            DATA RECORD IS EMPLOYEE-IN.
       01 EMPLOYEE-IN.
            05 SOCSEC     PIC 9(9).
            05 LASTNAME   PIC X(13).
            05 INITIALS   PIC XX.
            05 HOURRATE   PIC 999V99.
            05 HOURWORK   PIC 99V99.



       WORKING-STORAGE SECTION.
      * VARIABLES FOR THE PROGRAM
       01 EMPLOYEEINFO.
            05 GROSSPAY   PIC 9999V99 VALUE ZEROES.
            05 FEDTAX     PIC 999V99 VALUE ZEROES.
            05 NETPAY     PIC 9999V99 VALUE ZEROES.
            05 TIMENHALF  PIC 999V99 VALUE ZEROES.
            05 DTIME      PIC 999V99 VALUE ZEROES.

       01 COMPANYTOTALS.
            05 CGROSSPAY  PIC 99999V99 VALUE ZEROES.
            05 CFEDTAX    PIC 99999V99 VALUE ZEROES.
            05 CNETPAY    PIC 99999V99 VALUE ZEROES.
      * FLAG FOR MORE DATA IN FILE
       01 DATA-REMAINS      PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       PROCESS-EMPLOYEEFILE.
           DISPLAY 'HELLO'.
              DISPLAY 'COMPANY GROSS: ' CGROSSPAY
              DISPLAY 'COMPANY TAX: ' CFEDTAX
              DISPLAY 'COMPANY NETPAY: ' CNETPAY
      * OPEN AND PRIME A READ OF INPUT FILE FILEIN
           OPEN INPUT FILEIN.
           READ FILEIN
              AT END MOVE 'N' TO DATA-REMAINS
           END-READ.

           IF DATA-REMAINS = 'N'
              DISPLAY 'ERROR: INPUT FILE IS EMPTY'
           ELSE
              DISPLAY 'WE ARE READING'
              PERFORM CALCULATE
                 UNTIL DATA-REMAINS = 'N'
           END-IF.
      *CLOSE INPUT FILE FILEIN
           CLOSE FILEIN.

           STOP RUN.

       CALCULATE. 
      *DISPLAYS TO TEST WHAT I AM READING TO BE COMMENTED OUT LATER
      *     DISPLAY 'EMPLOYEE: ' EMPLOYEE-IN.
      *     DISPLAY 'SOCIAL: ' SOCSEC.
           DISPLAY '************************'
           DISPLAY 'NAME: ' LASTNAME.
           DISPLAY 'INITIAL: ' INITIALS.
           DISPLAY 'HOUR RATE: ' HOURRATE.
           DISPLAY 'HOUR WORKED: ' HOURWORK.
           PERFORM CALCULATE-GROSSPAY.
           DISPLAY 'GROSS PAY: ' GROSSPAY.
           PERFORM CALCULATE-TAX.
           DISPLAY 'TAX: ' FEDTAX.
           PERFORM CALCULATE-NETPAY.
           DISPLAY 'NETPAY: ' NETPAY.
           DISPLAY '************************'
              DISPLAY 'COMPANY GROSS: ' CGROSSPAY
              DISPLAY 'COMPANY TAX: ' CFEDTAX
              DISPLAY 'COMPANY NETPAY: ' CNETPAY
           READ FILEIN
              AT END MOVE 'N' TO DATA-REMAINS
           END-READ.

       CALCULATE-GROSSPAY.
           IF HOURWORK <= 40
              MULTIPLY HOURWORK BY HOURRATE GIVING GROSSPAY
           ELSE
              IF HOURWORK > 48
                 COMPUTE  GROSSPAY= (HOURRATE * 40) + 
                    ((HOURWORK - 48) * (HOURRATE * 2)) 
                    + ( 8 * (HOURRATE * 1.5))
              ELSE
                 SUBTRACT 40 FROM HOURWORK GIVING TIMENHALF
                 COMPUTE GROSSPAY= (HOURRATE * 40) + 
                    (TIMENHALF * (HOURRATE * 1.5))
              END-IF
           END-IF.
           DISPLAY 'TEST'.
           ADD GROSSPAY TO CGROSSPAY.

       CALCULATE-TAX.
           DISPLAY 'GROSS PAY IN TAX: ' GROSSPAY
           IF GROSSPAY <= 200
              COMPUTE FEDTAX= (GROSSPAY * .18)
           END-IF.
           IF GROSSPAY > 200
              IF GROSSPAY < 240
                 COMPUTE FEDTAX= (GROSSPAY * .2)
              END-IF
           END-IF.
           IF GROSSPAY > 240
              IF GROSSPAY < 280
                 COMPUTE FEDTAX= (GROSSPAY * .22)
              ELSE
                 COMPUTE FEDTAX= (GROSSPAY * .24)
              END-IF
           END-IF.
           ADD FEDTAX TO CFEDTAX.

       CALCULATE-NETPAY.
           COMPUTE NETPAY= GROSSPAY - FEDTAX.
           ADD NETPAY TO CNETPAY.







