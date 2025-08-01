       IDENTIFICATION DIVISION.
       PROGRAM-ID. userPassVal.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ERROR-FLAG    PIC X VALUE 'N'.
           88 NO-ERROR      VALUE 'N'.
           88 HAS-ERROR     VALUE 'Y'.

       01  WS-PASSWORD-CHECK.
           05 WS-HAS-CAPITAL    PIC X VALUE 'N'.
           05 WS-HAS-SMALL      PIC X VALUE 'N'.
           05 WS-HAS-NUMBER     PIC X VALUE 'N'.
           05 WS-HAS-SPECIAL    PIC X VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-IDX           PIC 99 VALUE 1.
           05 WS-CHAR          PIC X.
           05 WS-CHAR-ASCII    PIC 999.

       LINKAGE SECTION.
       01  LS-PASSWORD     PIC X(20).
       01  LS-RETURN-CODE  PIC X(2).
           88 VALID-INPUT    VALUE 0.
           88 EMPTY-PASS     VALUE 2.
           88 LENGTH-ERROR   VALUE 3.
           88 PASS-NO-UPPER  VALUE 4.
           88 PASS-NO-LOWER  VALUE 5.
           88 PASS-NO-NUMBER VALUE 6.
           88 PASS-NO-SPECIAL VALUE 7.
           88 PASS-TOO-SHORT  VALUE 8.

       PROCEDURE DIVISION USING LS-PASSWORD,
       LS-RETURN-CODE.
       MAIN-PARA.
           PERFORM INIT-VALIDATION
           PERFORM CHECK-EMPTY-PASSWORD
           PERFORM CHECK-LENGTH-CONSTRAINTS
           PERFORM CHECK-PASSWORD-STRENGTH
           GOBACK.

       INIT-VALIDATION.
           MOVE "00" TO LS-RETURN-CODE
           SET NO-ERROR TO TRUE
           MOVE 'N' TO WS-HAS-CAPITAL
           MOVE 'N' TO WS-HAS-SMALL
           MOVE 'N' TO WS-HAS-NUMBER
           MOVE 'N' TO WS-HAS-SPECIAL.



       CHECK-EMPTY-PASSWORD.
           IF NOT HAS-ERROR
              IF LS-PASSWORD = SPACES
                 MOVE "02" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF
           END-IF.

        CHECK-LENGTH-CONSTRAINTS.
           IF NOT HAS-ERROR
               IF LS-PASSWORD(1:1) = SPACES AND NOT HAS-ERROR
                 MOVE "03" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
               END-IF
           END-IF.

       CHECK-PASSWORD-STRENGTH.
           IF NOT HAS-ERROR
              IF FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD)) < 9
                 MOVE "08" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF
           END-IF.

           IF NOT HAS-ERROR
              PERFORM VARYING WS-IDX FROM 1 BY 1
                      UNTIL WS-IDX > FUNCTION LENGTH(LS-PASSWORD)
                 MOVE LS-PASSWORD(WS-IDX:1) TO WS-CHAR
                 COMPUTE WS-CHAR-ASCII = FUNCTION ORD(WS-CHAR)

      *          Check for uppercase letter (ASCII 65-90)
                 IF WS-CHAR-ASCII >= 65 AND WS-CHAR-ASCII <= 90
                    MOVE 'Y' TO WS-HAS-CAPITAL
                 END-IF

      *          Check for lowercase letter (ASCII 97-122)
                 IF WS-CHAR-ASCII >= 97 AND WS-CHAR-ASCII <= 122
                    MOVE 'Y' TO WS-HAS-SMALL
                 END-IF

      *          Check for number (ASCII 48-57)
                 IF WS-CHAR-ASCII >= 48 AND WS-CHAR-ASCII <= 57
                    MOVE 'Y' TO WS-HAS-NUMBER
                 END-IF

      *          Check for special characters
                 IF (WS-CHAR-ASCII >= 33 AND WS-CHAR-ASCII <= 47) OR
                    (WS-CHAR-ASCII >= 58 AND WS-CHAR-ASCII <= 64) OR
                    (WS-CHAR-ASCII >= 91 AND WS-CHAR-ASCII <= 96) OR
                    (WS-CHAR-ASCII >= 123 AND WS-CHAR-ASCII <= 126)
                    MOVE 'Y' TO WS-HAS-SPECIAL
                 END-IF
              END-PERFORM

              IF WS-HAS-CAPITAL = 'N'
                 MOVE "04" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF

              IF WS-HAS-SMALL = 'N' AND NOT HAS-ERROR
                 MOVE "05" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF

              IF WS-HAS-NUMBER = 'N' AND NOT HAS-ERROR
                 MOVE "06" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF

              IF WS-HAS-SPECIAL = 'N' AND NOT HAS-ERROR
                 MOVE "07" TO LS-RETURN-CODE
                 SET HAS-ERROR TO TRUE
              END-IF
           END-IF.
