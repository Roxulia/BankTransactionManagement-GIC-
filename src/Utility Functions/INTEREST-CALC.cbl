       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INTEREST-RATE      PIC V9999 VALUE 0.0002.

       *> Current Date Fields
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURR-DATE.
               10  WS-CURR-YEAR    PIC 9(4).
               10  WS-CURR-MONTH   PIC 9(2).
               10  WS-CURR-DAY     PIC 9(2).
           05  WS-CURR-TIME.
               10  WS-CURR-HOUR    PIC 9(2).
               10  WS-CURR-MINUTES PIC 9(2).
               10  WS-CURR-SECONDS PIC 9(2).
               10  WS-CURR-HUND    PIC 9(2).

       *> User Date Fields
       01 WS-USER-DATE.
           05 WS-USER-YY         PIC 9(2).
           05 WS-USER-MM         PIC 9(2).
           05 WS-USER-DD         PIC 9(2).

       *> Calculated Fields
       01 WS-USER-FULL-YEAR     PIC 9(4).
       01 WS-ADJ-USER-MM        PIC 9(2).
       01 WS-ADJ-USER-YEAR      PIC 9(4).

       LINKAGE SECTION.
       01 LS-INPUT-DATA.
           05 LS-INPUT-DATE      PIC 9(6).
           05 LS-INITIAL-AMOUNT  PIC 9(10).
           05 LS-UPDATE-TIME     PIC 9(6).  *> Input time (HHMMSS format)
       01 LS-OUTPUT-DATA.
           05 LS-UPDATED-DATE.
               10 LS-UPD-YEAR    PIC 9(2).
               10 LS-UPD-MONTH   PIC 9(2).
               10 LS-UPD-DAY     PIC 9(2).
           05 LS-FINAL-AMOUNT    PIC 9(10)V99.
           05 LS-INTEREST-AMOUNT PIC 9(6)V99.
           05 LS-TOTAL-MONTHS    PIC 9(4).
           05 LS-UPDATED-TIME    PIC 9(6).  *> Output time (HHMMSS format)

       PROCEDURE DIVISION USING LS-INPUT-DATA, LS-OUTPUT-DATA.
       MAIN-LOGIC.
           *> Get current date and time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS

           *> Update the time to current time (HHMMSS format)
           STRING WS-CURR-HOUR WS-CURR-MINUTES WS-CURR-SECONDS
                  DELIMITED BY SIZE INTO LS-UPDATED-TIME

           *> Process input date
           MOVE LS-INPUT-DATE TO WS-USER-DATE

           *> Convert 2-digit year to 4-digit year
           IF WS-USER-YY >= 50
               COMPUTE WS-USER-FULL-YEAR = 1900 + WS-USER-YY
           ELSE
               COMPUTE WS-USER-FULL-YEAR = 2000 + WS-USER-YY
           END-IF

           *> Adjust user date to next payday
           MOVE WS-USER-MM TO WS-ADJ-USER-MM
           MOVE WS-USER-FULL-YEAR TO WS-ADJ-USER-YEAR

           IF WS-USER-DD > 1
               IF WS-ADJ-USER-MM = 12
                   ADD 1 TO WS-ADJ-USER-YEAR
                   MOVE 1 TO WS-ADJ-USER-MM
               ELSE
                   ADD 1 TO WS-ADJ-USER-MM
               END-IF
           END-IF

           *> Calculate months difference
           COMPUTE LS-TOTAL-MONTHS = 1 +
               ((WS-CURR-YEAR * 12 + WS-CURR-MONTH) -
                (WS-ADJ-USER-YEAR * 12 + WS-ADJ-USER-MM))

           *> Ensure non-negative months
           IF LS-TOTAL-MONTHS < 0
               MOVE 0 TO LS-TOTAL-MONTHS
           END-IF

           *> Calculate interest and final amount
           COMPUTE LS-INTEREST-AMOUNT ROUNDED =
               LS-TOTAL-MONTHS * LS-INITIAL-AMOUNT * WS-INTEREST-RATE
           COMPUTE LS-FINAL-AMOUNT ROUNDED =
               LS-INITIAL-AMOUNT + LS-INTEREST-AMOUNT

           *> Update date to current date (last 2 digits of year)
           MOVE WS-CURR-YEAR(3:2) TO LS-UPD-YEAR
           MOVE WS-CURR-MONTH TO LS-UPD-MONTH
           MOVE WS-CURR-DAY   TO LS-UPD-DAY

           EXIT PROGRAM.
       END PROGRAM INTEREST-CALC.
