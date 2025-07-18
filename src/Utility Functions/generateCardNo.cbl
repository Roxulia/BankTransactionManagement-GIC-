      ******************************************************************
      * Author: Sat Paing Thu
      * Date: 18.07.2025
      * Purpose: 16 Digit Random Generated Card Number.
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
       PROGRAM-ID. generateCardNo.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RANDOM          PIC 9(16).
       01 WS-TIME            PIC 9(8).
       01 WS-SEED            PIC 9(8).
       01 WS-FORMATTED-NUM.
          05 PART1           PIC 9(4).
          05 FILLER          PIC X VALUE '-'.
          05 PART2           PIC 9(4).
          05 FILLER          PIC X VALUE '-'.
          05 PART3           PIC 9(4).
          05 FILLER          PIC X VALUE '-'.
          05 PART4           PIC 9(4).

       LINKAGE SECTION.
       01 LS-RETURN-NUM      PIC X(19).

       PROCEDURE DIVISION USING LS-RETURN-NUM.
       MAIN-PARA.
           ACCEPT WS-TIME FROM TIME
           MOVE FUNCTION CURRENT-DATE(9:8) TO WS-SEED

           COMPUTE WS-RANDOM = FUNCTION
           RANDOM (WS-SEED) * 9000000000000000
           ADD 1000000000000000 TO WS-RANDOM

           MOVE WS-RANDOM(1:4)  TO PART1
           MOVE WS-RANDOM(5:4)  TO PART2
           MOVE WS-RANDOM(9:4)  TO PART3
           MOVE WS-RANDOM(13:4) TO PART4

           MOVE WS-FORMATTED-NUM TO LS-RETURN-NUM

           DISPLAY 'Generated Number: ' WS-FORMATTED-NUM

           EXIT PROGRAM.

       END PROGRAM generateCardNo.
