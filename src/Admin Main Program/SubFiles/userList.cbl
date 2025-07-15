      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 15/7/2025
      * Purpose: Read and show all user account details.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userList.
        ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserAccounts
           ASSIGN TO "../../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD UserAccounts.
       01 UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UEncPsw     PIC X(32).
           05 UAddress     PIC X(20).
           05 Phone       PIC X(9).
           05 Balance     PIC 9(10)V99.
           05 UDate       PIC 9(6).
           05 UTime       PIC 9(6).
       WORKING-STORAGE SECTION.
       01 EOF-FLAG        PIC X VALUE 'N'.
       01 Choice          PIC 9(1) VALUE 0.
       01 More-Data       PIC X Value 'Y'.
       01 I               PIC 9(2) value 0.
       01 J               PIC 9(2) value 0.
       01 ws-fs pic x(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
             MAIN-PROCEDURE.



           OPEN INPUT UserAccounts.



               DISPLAY "*********************************************"
               DISPLAY " User List Report "
               DISPLAY "*********************************************"
               DISPLAY "UID  UName Address Phone UDate UTime"
                   DISPLAY "------------------------------------------"
              PERFORM UNTIL Choice = 'Exit'

               IF Choice = '1'
              PERFORM UNTIL I >= 2
                READ UserAccounts INTO UserRecord
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                       DISPLAY "End of Files"
                       CONTINUE
                   NOT AT END
                       DISPLAY UID SPACE
                               UName SPACE
                               *>ULoginName SPACE
                               uAddress SPACE
                               Phone SPACE
                               UDate SPACE
                               UTime
                      Add 1 to I

                   END-READ

               END-PERFORM

               ELSE IF Choice ='2'
                   PERFORM Until J >= 2
                READ UserAccounts INTO UserRecord
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                       DISPLAY "End of Files"
                       CONTINUE
                   NOT AT END
                       DISPLAY UID SPACE
                               UName SPACE
                               *>ULoginName SPACE
                               uAddress SPACE
                               Phone SPACE
                               UDate SPACE
                               UTime
                      Add 1 to J

                   END-READ

               END-PERFORM
               ELSE IF Choice = '3'
                   EXIT PROGRAM
                END-IF

               DISPLAY "Enter Options to see User Data."
               Display "1. Show Previous ten User's Data."
               Display "2. Show Next ten User's Data."
               Display "3. Exit"
               ACCEPT Choice
             END-PERFORM.
           CLOSE UserAccounts.

          STOP RUN.
       END PROGRAM userList.
