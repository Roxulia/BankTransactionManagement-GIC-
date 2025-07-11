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
           05 UID           PIC 9(5).
           05 UName         PIC X(20).
           05 ULoginName    PIC X(25).
           05 UEncPsw       PIC X(32).
           05 UAddress      PIC X(20).
           05 Phone         PIC x(9).
           05 Balance       PIC 9(10)V99.
           05 UDate         PIC 9(6).
           05 UTime         PIC 9(6).

       WORKING-STORAGE SECTION.
       01 WS-UID            PIC 9(5).
       01 WS-FOUND          PIC X VALUE "N".
       01 END-FILE          PIC X VALUE "N".
       01  ws-fs pic x(2).

       linkage section.
       01  LS-UID pic 9(5).

       PROCEDURE DIVISION using LS-UID.
       MAIN-LOGIC.

           OPEN INPUT UserFile.

           if ws-fs not EQUAL "00" THEN
               display "File openning error"
               exit PROGRAM
           end-if.

           PERFORM UNTIL END-FILE = "Y"
               READ UserFile
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       IF UID = LS-UID
                           MOVE "Y" TO WS-FOUND
                           DISPLAY "-----------------------------------"
                           DISPLAY "User Name : " UName
                           DISPLAY "Phone     : " Phone
                           DISPLAY "Balance   : " Balance
                           DISPLAY "-----------------------------------"
                           MOVE "Y" TO END-FILE
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND NOT = "Y"
               DISPLAY "User ID not found."
           END-IF.

           CLOSE UserFile.

           exit program.
