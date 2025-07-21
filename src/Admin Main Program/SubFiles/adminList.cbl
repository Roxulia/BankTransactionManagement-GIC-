      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 15/7/2025
      * Purpose: Read and page through admin account details (4 records per page).
      * Tectonics: OpenCOBOL (GnuCOBOL)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adminList.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AdminAccounts
               ASSIGN TO "../../../data/AdminAccounts.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  AdminAccounts.
       01  AdminRecord.
           05 AID         PIC 9(5).          *> Admin ID
           05 AName       PIC X(20).         *> Admin Name
           05 ALoginName  PIC X(25).         *> Login Username
           05 AEncPsw     PIC X(32).         *> Encrypted Password
           05 ARole       PIC 9(1).


       WORKING-STORAGE SECTION.
       77  WS-FS         PIC XX.
       77  WS-PAGE       PIC 9(3) VALUE 1.
       77  WS-CHOICE     PIC 9 VALUE 0.
       77  WS-SKIP-COUNT PIC 9(4).
       77  WS-REC-COUNT  PIC 9(2).
       77  WS-TEXT       PIC X(50).
       77  WS-EOF        PIC X VALUE 'N'.
       77  WS-LAST-PAGE  PIC 9(3) VALUE 0.

       01  WS-DISPLAY-LINE.
           05 WS-AID     PIC X(5).
           05 WS-AName   PIC X(20).
           05 WS-ALoginName PIC X(25).
           05 WS-ARole   PIC 9(1).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           move 1 to  ws-page
           INITIALIZE ws-last-page
           move 'N' to ws-eof
           INITIALIZE ws-choice
           OPEN INPUT AdminAccounts
           PERFORM DISPLAY-PAGE
           CLOSE AdminAccounts
           exit program.

      *-------------------------------------------------------------------*
       MENU-LOOP.
           DISPLAY "---------------------------------------------"
           if ws-page = 1 and ws-eof = 'Y'
           DISPLAY "Options:                            3=Exit"
           DISPLAY "--------------------------------------------------"
           else if ws-eof = 'N' and ws-page = 1
           DISPLAY "Options:               2=Next Page, 3=Exit"
           DISPLAY "--------------------------------------------------"
           else if ws-eof = 'N' and ws-page not EQUAL 1
           DISPLAY "Options:  1=Prev Page, 2=Next Page, 3=Exit"
           DISPLAY "--------------------------------------------------"
           else if ws-eof = 'Y' and ws-page not EQUAL 1
           DISPLAY "Options:  1=Prev Page,            , 3=Exit"
           DISPLAY "--------------------------------------------------"
           end-if
           END-IF
           END-IF
           END-IF
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1
               IF WS-PAGE > 1
                  SUBTRACT 1 FROM WS-PAGE
                  PERFORM DISPLAY-PAGE
               ELSE
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
               CLOSE AdminAccounts
               exit program
             WHEN OTHER
               DISPLAY "Invalid choice."
               perform DISPLAY-PAGE
           END-EVALUATE.

      *-------------------------------------------------------------------*
       DISPLAY-PAGE.
           *> Reposition by closing/re-opening
           CLOSE AdminAccounts
           OPEN INPUT AdminAccounts

           IF WS-FS NOT = "00"
               DISPLAY "ERROR: Unable to OPEN AdminAccounts," WS-FS
               STOP RUN
           END-IF
           move 'N' to ws-eof
           *> Skip records from previous pages
           COMPUTE WS-SKIP-COUNT = (WS-PAGE - 1) * 5
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > WS-SKIP-COUNT
               READ AdminAccounts
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
           DISPLAY "AID   AName              AdminLoginName       Role"
           DISPLAY "---------------------------------------------------"

           *> Read and display up to 4 records
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > 5
               READ AdminAccounts
                   AT END
                     DISPLAY "-- End of file reached --"
                     MOVE WS-PAGE TO WS-LAST-PAGE
                     MOVE 'Y' TO WS-EOF
                     EXIT PERFORM
                   NOT AT END
                     MOVE AID TO WS-AID
                     MOVE AName TO WS-AName
                     MOVE ALoginName TO WS-ALoginName
                     MOVE ARole TO WS-ARole
                     DISPLAY WS-DISPLAY-LINE
               END-READ
           END-PERFORM
           perform MENU-LOOP
               .
       END PROGRAM adminList.
