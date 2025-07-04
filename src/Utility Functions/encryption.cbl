      ******************************************************************
      * Author: Sat Paing Thu
      * Date: 04.07.2025
      * Purpose: Password Encryption
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. encryption.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS HEX-CHARS IS "0" THRU "9", "A" THRU "F".

       INPUT-OUTPUT section.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CONSTANTS.
           05 HASH-SIZE       PIC 99 VALUE 32.
           05 SALT-SIZE       PIC 99 VALUE 32.
           05 ROUNDS          PIC 999 VALUE 100.
       01  SALT-VALUE         PIC X(32)
           VALUE "Kj#9$mP2@nQ5*vR8&wS4^xT7%yU3!zV6".
       01  HASH-WORK-AREA.
           05 WS-I            PIC 99.
           05 WS-J            PIC 99.
           05 WS-CHAR-CODE    PIC 999.
           05 WS-SALT-CODE    PIC 999.
           05 WS-PREV-CODE    PIC 999.
           05 WS-TEMP         PIC 999.
           05 WS-ROUND        PIC 999.
       01  HASH-BUFFER.
           05 WS-HASH-TMP     PIC X(32).
           05 WS-PREV-HASH    PIC X(32).


       LINKAGE SECTION.
       01  LS-INPUT-PW        PIC X(20).
       01  LS-HASHED-PW       PIC X(32).
       PROCEDURE DIVISION using LS-INPUT-PW, LS-HASHED-PW.

       MAIN-PROCESS.
           INITIALIZE WS-HASH-TMP WS-PREV-HASH LS-HASHED-PW

           PERFORM VARYING WS-ROUND FROM 1 BY 1 UNTIL WS-ROUND > ROUNDS
               PERFORM INITIAL-HASH
               PERFORM MIX-WITH-PREVIOUS
               MOVE WS-HASH-TMP TO WS-PREV-HASH
           END-PERFORM

           MOVE WS-HASH-TMP TO LS-HASHED-PW

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
                   COMPUTE WS-TEMP =
                       FUNCTION
           MOD((WS-CHAR-CODE + WS-I * WS-ROUND + WS-SALT-CODE ** 2), 16) + 48

                   IF WS-TEMP > 57
                       ADD 7 TO WS-TEMP
                   END-IF

                   MOVE FUNCTION CHAR(WS-TEMP) TO WS-HASH-TMP(WS-I:1)
               END-IF
           END-PERFORM.

      ******************************************************************
       MIX-WITH-PREVIOUS.
      * Strengthens hash by chaining result of prior round

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > HASH-SIZE
               COMPUTE WS-CHAR-CODE = FUNCTION ORD(WS-HASH-TMP(WS-I:1))
               COMPUTE WS-PREV-CODE = FUNCTION ORD(WS-PREV-HASH(WS-I:1))
               COMPUTE WS-SALT-CODE = FUNCTION ORD(SALT-VALUE(WS-I:1))

               COMPUTE WS-TEMP = FUNCTION MOD(
                   (WS-CHAR-CODE * 3 + WS-PREV-CODE
                   * 2 + WS-SALT-CODE + WS-ROUND), 16) + 48

               IF WS-TEMP > 57
                   ADD 7 TO WS-TEMP
               END-IF

               MOVE FUNCTION CHAR(WS-TEMP) TO WS-HASH-TMP(WS-I:1)
           END-PERFORM.

       END PROGRAM encryption.
