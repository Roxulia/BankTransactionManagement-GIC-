      ******************************************************************
      * Author: Nyan Ye Thu, Myo Thein Chit
      * Date: 15/7/2025
      * Purpose: Read and page through user account details (10 records per page).
      * Tectonics: OpenCOBOL (GnuCOBOL)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userListPaging.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserAccounts
               ASSIGN TO "/../../../data/UserAccounts.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  UserAccounts.
       01  UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UEncPsw     PIC X(32).
           05 UAddress    PIC X(20).
           05 Phone       PIC X(9).
           05 Balance     PIC 9(10)V99.
           05 UDate       PIC 9(6).
           05 UTime       PIC 9(6).

       WORKING-STORAGE SECTION.
       77  WS-FS         PIC XX.
       77  WS-PAGE       PIC 9(3) VALUE 1.
       77  WS-CHOICE     PIC 9 VALUE 0.
       77  WS-SKIP-COUNT PIC 9(4).
       77  WS-REC-COUNT  PIC 9(2).
       77  WS-TEXT       PIC X(50).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT UserAccounts
           PERFORM DISPLAY-PAGE
           PERFORM MENU-LOOP
           CLOSE UserAccounts
           STOP RUN.

      *-------------------------------------------------------------------*
       MENU-LOOP.
           DISPLAY "---------------------------------------------"
           DISPLAY "Options: 1=Prev page, 2=Next page, 3=Exit"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1
               IF WS-PAGE > 1
                  SUBTRACT 1 FROM WS-PAGE
                  PERFORM DISPLAY-PAGE
               ELSE
                  DISPLAY "Already at first page."
               END-IF
             WHEN 2
               ADD 1 TO WS-PAGE
               PERFORM DISPLAY-PAGE
             WHEN 3
               EXIT PROGRAM
             WHEN OTHER
               DISPLAY "Invalid choice."
           END-EVALUATE
           GO TO MENU-LOOP.

      *-------------------------------------------------------------------*
       DISPLAY-PAGE.
           *> Reposition by closing/re-opening
           CLOSE UserAccounts
           OPEN INPUT UserAccounts
           IF WS-FS NOT = "00"
               DISPLAY "ERROR: Unable to OPEN UserAccounts," WS-FS
               STOP RUN
           END-IF

           *> Skip records from previous pages
           COMPUTE WS-SKIP-COUNT = (WS-PAGE - 1) * 10
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > WS-SKIP-COUNT
               READ UserAccounts
                   AT END EXIT PERFORM
               END-READ
           END-PERFORM

           *> Display header
           DISPLAY "*********************************************"
           STRING  " Page " WS-PAGE " "
             DELIMITED BY SIZE
             INTO WS-TEXT
           END-STRING
           DISPLAY WS-TEXT
           DISPLAY "***************************************************"
           DISPLAY "UID  UName      Address       Phone    UDate  UTime"
           DISPLAY "---------------------------------------------------"

           *> Read and display up to 10 records
           PERFORM VARYING WS-REC-COUNT FROM 1 BY 1
                   UNTIL WS-REC-COUNT > 10
               READ UserAccounts
                   AT END
                     DISPLAY "-- End of file reached --"
                     EXIT PERFORM
               NOT AT END
                     DISPLAY UID " " UName " " UAddress " " Phone
               END-READ
           END-PERFORM

           DISPLAY "(Showing page " WS-PAGE ")"
           .
       END PROGRAM userListPaging.
