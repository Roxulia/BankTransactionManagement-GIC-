      ******************************************************************
      * Author: Nyan Ye Thu
      * Date:4.7.2025
      * Purpose:Bank Transaction Management
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userInfo.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT testfile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD testfile.
       01 userdata.
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(20).
           05 UEncPsw  PIC X(255).
           05 UAddress PIC X(20).
           05 Phone    PIC 9(9).
           05 Balance  PIC 9(10)V99.
           05 UDate    PIC 9(8).
           05 UTime    PIC 9(6).
       WORKING-STORAGE SECTION.
       01 InputUID PIC 9(5).
       01 Found    PIC X(1) VALUE 'N'.
       01 EOF-Flag PIC X(1) VALUE 'N'.
       01  ws-fs pic x(2).
       linkage SECTION.
       01 InputUID1 PIC 9(5).

       PROCEDURE DIVISION using InputUID1.
       MAIN-PROCEDURE.
            DISPLAY "Enter User ID(UID):"
            ACCEPT InputUID.

            OPEN INPUT testfile.
            PERFORM UNTIL EOF-Flag = 'Y'
               READ testfile INTO userdata
               AT END
               MOVE 'Y' TO EOF-Flag
               NOT AT END
               IF  UID = InputUID THEN
                   MOVE 'Y' TO Found
                   MOVE 'Y' TO EOF-Flag
               END-IF
               END-READ
               END-PERFORM
               CLOSE testfile.

               IF Found = 'Y'
                   DISPLAY "Username: " UName
                   DISPLAY "UAddress: " UAddress
                   DISPLAY "Phone: "    Phone
                   DISPLAY "Balance: "  Balance
               ELSE
                   DISPLAY "User not found."
               END-IF.
            STOP RUN.
       END PROGRAM userInfo.
