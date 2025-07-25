       IDENTIFICATION DIVISION.
       PROGRAM-ID. balanceInfo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserFile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD UserFile.
       01 userdata.
          COPY "../../Utility Functions/userFile.cpy".

       WORKING-STORAGE SECTION.
       01 WS-UID            PIC 9(5).
       01 WS-FOUND          PIC X VALUE "N".
       01 END-FILE          PIC X VALUE "N".
       01  ws-fs pic x(2).
       01 ws-balance pic zzzzzzzzz9.99 USAGE DISPLAY.

       COPY "../../Utility Functions/colorCodes.cpy".

       linkage section.
       01  LS-UID pic 9(5).

       PROCEDURE DIVISION using LS-UID.
       MAIN-LOGIC.
           move "N" to END-FILE
           OPEN INPUT UserFile

           if ws-fs not EQUAL "00" THEN
               display "File openning error"
               exit PROGRAM
           end-if

           PERFORM UNTIL END-FILE = "Y"
               READ UserFile
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       IF UID = LS-UID
                           MOVE "Y" TO WS-FOUND
                           DISPLAY "-----------------------------------"
                           DISPLAY "User Name : " UName
                           DISPLAY "Account No     : " UAccno
                           DISPLAY "Balance   : " WITH NO ADVANCING
                           MOVE balance to ws-balance
                           DISPLAY  ws-balance
                           DISPLAY "-----------------------------------"
                           MOVE "Y" TO END-FILE
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND NOT = "Y"
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display "!      Account NOT Found     !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
           END-IF

           CLOSE UserFile

           exit program.
