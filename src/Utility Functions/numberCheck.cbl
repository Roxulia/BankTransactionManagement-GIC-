      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. numberCheck.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  ws-indx pic 9.
       01  ws-char pic x.
       01  ws-char-ascii pic 9(3).
       01  ws-text pic x(16).
       01  containText pic x.
       LINKAGE SECTION.
       01  text-input pic x(16).
       01  statusCode pic x(2).
       PROCEDURE DIVISION using text-input , statusCode.
           INITIALIZE statusCode
           move 'n' to containText
           move SPACE to ws-text
           move text-input to ws-text
           perform varying ws-indx from 1 by 1
           until ws-indx >
           FUNCTION LENGTH(function trim(ws-text))
               move ws-text(ws-indx:1) to ws-char
               COMPUTE ws-char-ascii = FUNCTION ORD(ws-char)
               *>DISPLAY ws-char-ascii
               IF WS-CHAR-ASCII < 48 or WS-CHAR-ASCII > 58
                    move "y" to containText

               END-IF
           END-PERFORM
           if containText EQUAL "y"
               move "10" to statusCode
           ELSE
               move "00" to statusCode
           end-if
           exit PROGRAM.

       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM numberCheck.
