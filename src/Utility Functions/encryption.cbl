      ******************************************************************
      * Author: Sat Paing Thu
      * Date: 04.07.2025
      * Update Date: 17.07.2025
      * Purpose: Password Encryption
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. encryption.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS HEX-CHARS IS "0" THRU "9", "A" THRU "F".

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * Constants
       01  CONSTANTS.
           05 HASH-SIZE       PIC 99 VALUE 32.
           05 SALT-SIZE       PIC 99 VALUE 32.
           05 ROUNDS          PIC 999 VALUE 100.

      * Salt for pseudo-hashing
       01  SALT-VALUE         PIC X(32)
           VALUE "Kj#9$mP2@nQ5*vR8&wS4^xT7%yU3!zV6".

      * Work areas
       01  HASH-WORK-AREA.
           05 WS-I            PIC 99.
           05 WS-J            PIC 99.
           05 WS-CHAR-CODE    PIC 999.
           05 WS-SALT-CODE    PIC 999.
           05 WS-PREV-CODE    PIC 999.
           05 WS-TEMP         PIC 999.
           05 WS-ROUND        PIC 999.
           05 WS-POSITION     PIC 999.

      * Hash buffer
       01  HASH-BUFFER.
           05 WS-HASH-TMP     PIC X(32).
           05 WS-PREV-HASH    PIC X(32).

       LINKAGE SECTION.
       01  LS-INPUT-PW        PIC X(20).
       01  LS-HASHED-PW       PIC X(32).

       PROCEDURE DIVISION USING LS-INPUT-PW, LS-HASHED-PW.

       MAIN-PROCESS.

           INITIALIZE WS-HASH-TMP WS-PREV-HASH LS-HASHED-PW

           PERFORM VARYING WS-ROUND FROM 1 BY 1 UNTIL WS-ROUND > ROUNDS
               PERFORM INITIAL-HASH
               PERFORM MIX-WITH-PREVIOUS
               MOVE WS-HASH-TMP TO WS-PREV-HASH
           END-PERFORM

           MOVE WS-HASH-TMP TO LS-HASHED-PW
           *>DISPLAY LS-HASHED-PW

           GOBACK.

      ******************************************************************
       INITIAL-HASH.
      * Basic hashing pass with input + salt + round mixing

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > HASH-SIZE
               COMPUTE WS-J = FUNCTION MOD(WS-I, 20) + 1

               IF LS-INPUT-PW(WS-J:1) = SPACE
                   MOVE "0" TO WS-HASH-TMP(WS-I:1)
               ELSE
                   COMPUTE WS-CHAR-CODE =
                       FUNCTION ORD(LS-INPUT-PW(WS-J:1))
                   COMPUTE WS-SALT-CODE =
                       FUNCTION ORD(SALT-VALUE(WS-I:1))
                   COMPUTE WS-POSITION = WS-J * 13

      * Enhanced multipliers based on character types and position
                   EVALUATE TRUE
      * Uppercase letters (A-Z)
                   WHEN WS-CHAR-CODE >= 65 AND <= 90
                       COMPUTE WS-CHAR-CODE =
                           WS-CHAR-CODE * 3 + WS-POSITION
      * Lowercase letters (a-z)
                   WHEN WS-CHAR-CODE >= 97 AND <= 122
                       COMPUTE WS-CHAR-CODE =
                           WS-CHAR-CODE * 5 + WS-POSITION
      * Numbers (0-9)
                   WHEN WS-CHAR-CODE >= 48 AND <= 57
                       COMPUTE WS-CHAR-CODE =
                           WS-CHAR-CODE * 7 + WS-POSITION
      * Special characters - enhanced handling
                   WHEN OTHER
                       COMPUTE WS-CHAR-CODE =
                           WS-CHAR-CODE * 11 + WS-POSITION * 2
                   END-EVALUATE

                   COMPUTE WS-TEMP = FUNCTION MOD(
                       (WS-CHAR-CODE * 17 + WS-I *
                       WS-ROUND * 13 + WS-SALT-CODE ** 2 +
                       WS-POSITION), 16) + 48

                   IF WS-TEMP > 57
                       ADD 7 TO WS-TEMP
                   END-IF

                   MOVE FUNCTION CHAR(WS-TEMP) TO WS-HASH-TMP(WS-I:1)
               END-IF
           END-PERFORM.

      ******************************************************************
       MIX-WITH-PREVIOUS.
      * Enhanced mixing algorithm with position sensitivity

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > HASH-SIZE
               COMPUTE WS-CHAR-CODE = FUNCTION ORD(WS-HASH-TMP(WS-I:1))
               COMPUTE WS-PREV-CODE = FUNCTION ORD(WS-PREV-HASH(WS-I:1))
               COMPUTE WS-SALT-CODE = FUNCTION ORD(SALT-VALUE(WS-I:1))

      * Enhanced mixing formula with better distribution
               COMPUTE WS-TEMP = FUNCTION MOD(
                   (WS-CHAR-CODE * 7 + WS-PREV-CODE * 5 +
                   WS-SALT-CODE * 3 + WS-ROUND * 11 +
                   WS-I * 13), 16) + 48

               IF WS-TEMP > 57
                   ADD 7 TO WS-TEMP
               END-IF

               MOVE FUNCTION CHAR(WS-TEMP) TO WS-HASH-TMP(WS-I:1)
           END-PERFORM.
       END PROGRAM encryption.
