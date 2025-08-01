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
       01 WS-CURRENT-DATE.
          05 WS-YEAR         PIC 9(4).
          05 WS-MONTH        PIC 9(2).
          05 WS-DAY          PIC 9(2).
          05 WS-HOUR         PIC 9(2).
          05 WS-MINUTE       PIC 9(2).
          05 WS-SECOND       PIC 9(2).
       01 WS-PREV-DATE       PIC 9(8) VALUE ZEROS.
       01 WS-SEQUENCE        PIC 9(4) VALUE 0.
       01 WS-FORMATTED-NUM.
          05 WS-DATE-TIME    PIC 9(10).
          05 WS-SEQ-NUM      PIC 9(6).

       LINKAGE SECTION.

       01  UID               PIC 9(5).

       01  LS-RETURN-NUM     PIC 9(16).

       PROCEDURE DIVISION USING UID,LS-RETURN-NUM.
       MAIN-PARA.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-DATE

           COMPUTE WS-DATE-TIME =
               (WS-YEAR * 1000000) +
               (WS-MONTH * 10000) +
               (WS-DAY * 100) +
                WS-HOUR

           MOVE UID TO WS-SEQ-NUM
           MOVE WS-FORMATTED-NUM TO LS-RETURN-NUM

           DISPLAY 'Generated Number: ' WS-FORMATTED-NUM

           EXIT PROGRAM.

       END PROGRAM generateCardNo.
