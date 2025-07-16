      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 15/7/2025
      * Purpose: Read and Generate all admain details.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adminList.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AdminAccounts
           ASSIGN TO "../../../../data/AdminAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD AdminAccounts.
       01 AdminRecord.
           05 AID         PIC 9(5).
           05 AName       PIC X(20).
           05 ALoginName  PIC X(25).
           05 AEncPsw     PIC X(32).
           05 ARole       PIC 9(1).
       WORKING-STORAGE SECTION.
       01 EOF-FLAG      PIC X VALUE 'N'.
       01 ws-fs pic x(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "**************************************************".
           DISPLAY " Admin List Report ".
           DISPLAY "**************************************************".
           DISPLAY "AID         AName       ALoginName            Role".
           DISPLAY "------------ -------------------------------------".
             OPEN INPUT AdminAccounts.

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ AdminAccounts INTO AdminRecord
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                   DISPLAY AID SPACE AName SPACE ALoginName SPACE ARole
               END-READ
           END-PERFORM.
           CLOSE AdminAccounts.
           DISPLAY "--------------------------------------------------".
           DISPLAY "End of Admin List."
           STOP RUN.
       END PROGRAM adminList.
