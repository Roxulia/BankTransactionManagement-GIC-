******************************************************************
      * Author: Sat Paing Thu
      * Date:10.7.2025
      * Purpose: Pyaw Pya Woo - Get All Users
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. getAllUsers.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT testfile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD testfile.
       01 userdata.
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(25).
           05 UEncPsw  PIC X(32).
           05 UAddress PIC X(20).
           05 Phone    PIC 9(9).
           05 Balance  PIC 9(10)V99.
           05 UDate    PIC 9(8).
           05 UTime    PIC 9(6).
       WORKING-STORAGE SECTION.
       01 EOF-Flag PIC X(1) VALUE 'N'.
       01 ws-fs pic x(2).
       01 WS-Count PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "HELLO"
           OPEN INPUT testfile
           IF WS-FS NOT = "00"
               DISPLAY "File Status: " WS-FS
               DISPLAY "Error opening file!"
               GOBACK
           END-IF

           DISPLAY "ENTERING TEST FILE"

           PERFORM UNTIL EOF-Flag = 'Y'
               READ testfile INTO userdata
                   AT END
                       MOVE 'Y' TO EOF-Flag
                   NOT AT END
                       IF WS-FS = "00"
                           ADD 1 TO WS-Count
                           DISPLAY "================="
                           DISPLAY "Record #: " WS-Count
                           DISPLAY "UID: " UID
                           DISPLAY "Name: " UName
                           DISPLAY "Login Name: " ULoginName
                           DISPLAY "Encoded Password: "UEncPsw
                           DISPLAY "Address: " UAddress
                           DISPLAY "Phone: " Phone
                           DISPLAY "Balance: " Balance
                           DISPLAY "Date: " UDate
                           DISPLAY "Time: " UTime
                       ELSE
                           DISPLAY "Error reading record: " WS-FS
                           MOVE 'Y' TO EOF-Flag
                       END-IF
               END-READ
           END-PERFORM

           CLOSE testfile
           IF WS-FS NOT = "00"
               DISPLAY "Error closing file: " WS-FS
           END-IF

           DISPLAY "ENDING TEST FILE"
           DISPLAY "================="
           DISPLAY "Total Records: " WS-Count

           GOBACK.
       END PROGRAM getAllUsers.
