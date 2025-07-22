      ******************************************************************
      * Author: Nyan Ye Thu, Myo Thein Chit
      * Date: 15/7/2025
      * Purpose: Read and page through user account details (10 records per page).
      * Tectonics: OpenCOBOL (GnuCOBOL)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userList.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserAccounts
               ASSIGN TO "../../../data/UserAccounts.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  UserAccounts.
       01  UserRecord.

       COPY "../../Utility Functions/userFile.cpy".

       WORKING-STORAGE SECTION.
       77  WS-FS         PIC XX.
       77  WS-PAGE       PIC 9(3) VALUE 1.
       77  WS-CHOICE     PIC 9 VALUE 0.
       77  WS-SKIP-COUNT PIC 9(4).
       77  WS-REC-COUNT  PIC 9(2).
       77  WS-TEXT       PIC X(50).
       77  WS-EOF        PIC X value 'N'.
       77  WS-LAST-PAGE  PIC 9(3) value 0.
       01  WS-DISPLAY-LINE.
           05 WS-UID     PIC X(6).
           05 WS-UNAME   PIC X(20).
           05 WS-ADDRESS PIC X(20).
           05 WS-PHONE   PIC X(12).
           05 ws-Uaccno  Pic 9(16).
           05 ws-trxcount pic 9(5).
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           move 1 to  ws-page
           INITIALIZE ws-last-page
           move 'N' to ws-eof
           INITIALIZE ws-choice
           move 0 to ws-choice
           OPEN INPUT UserAccounts
           PERFORM DISPLAY-PAGE
           CLOSE UserAccounts
           exit program.

      *-------------------------------------------------------------------*
       MENU-LOOP.
           DISPLAY "--------------------------------------------------"
           if ws-page = 1 and ws-eof = 'Y'
           DISPLAY "Options:                            3=Exit"
           DISPLAY "--------------------------------------------------"
           else
               if ws-eof = 'N' and ws-page = 1
           DISPLAY "Options:               2=Next Page, 3=Exit"
           DISPLAY "--------------------------------------------------"
           else
               if ws-eof = 'N' and ws-page not EQUAL 1
           DISPLAY "Options:  1=Prev Page, 2=Next Page, 3=Exit"
           DISPLAY "--------------------------------------------------"
           else
               if ws-eof = 'Y' and ws-page not EQUAL 1
           DISPLAY "Options:  1=Prev Page,            , 3=Exit"
           DISPLAY "--------------------------------------------------"
           ELSE
           DISPLAY "Options:  1=Prev Page, 2=Next Page, 3=Exit"
           DISPLAY "--------------------------------------------------"
           end-if
           END-IF
           END-IF
           END-IF
           Display "Choice : "
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1
               IF WS-PAGE > 1
                  SUBTRACT 1 FROM WS-PAGE
                  PERFORM DISPLAY-PAGE
               ELSE IF WS-PAGE = 1
                  PERFORM DISPLAY-PAGE
               END-IF
             WHEN 2
               IF WS-EOF = 'N'
                   ADD 1 TO WS-PAGE
                   PERFORM DISPLAY-PAGE
               ELSE
                   MOVE WS-LAST-PAGE TO WS-PAGE
                   PERFORM DISPLAY-PAGE
               END-IF
             WHEN 3
               CLOSE UserAccounts
               exit program
             WHEN OTHER
               DISPLAY "Invalid choice."
               perform DISPLAY-PAGE
           END-EVALUATE.

      *-------------------------------------------------------------------*
       DISPLAY-PAGE.
           *> Reposition by closing/re-opening
           CLOSE UserAccounts
           OPEN INPUT UserAccounts
           move 'N' to ws-eof
           IF WS-FS NOT = "00"
               DISPLAY "ERROR: Unable to OPEN UserAccounts," WS-FS
               STOP RUN
           END-IF

           *> Skip records from previous pages
           COMPUTE WS-SKIP-COUNT = (WS-PAGE - 1) * 5
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > WS-SKIP-COUNT
               READ UserAccounts
                   AT END EXIT PERFORM
               END-READ
           END-PERFORM

           *> Display header
           DISPLAY "***************************************************"
           STRING  " Page " WS-PAGE " "
             DELIMITED BY SIZE
             INTO WS-TEXT
           END-STRING
           DISPLAY WS-TEXT
           DISPLAY "***************************************************"
                   "***************************************************"
           DISPLAY "UID   UName              Address              Phone"
                   "       Account Number             Transaction Count"
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
           *> Read and display up to 10 records
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > 5
               READ UserAccounts
                   AT END
                     DISPLAY "-- End of file reached --"
                     MOVE WS-PAGE TO WS-LAST-PAGE
                     MOVE 'Y' TO WS-EOF
                     EXIT PERFORM
               NOT AT END
                     MOVE UID TO WS-UID
                     MOVE UName TO WS-UNAME
                     MOVE UAddress TO WS-ADDRESS
                     MOVE uph TO WS-PHONE
                     move uaccno to ws-Uaccno
                     move trxcount to ws-trxcount
                     DISPLAY WS-DISPLAY-LINE
               END-READ
           END-PERFORM
           perform MENU-LOOP.
       END PROGRAM userList.
