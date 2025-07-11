      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 3.7.2025
      * Purpose: Bank Transaction Management
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userLogin.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserAccounts
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD UserAccounts.
       01 UserRecord.
           05 UID PIC x(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(20).
           05 UEncPsw  PIC X(32).
           05 UAddress PIC X(20).
           05 Phone    PIC 9(9).
           05 Balance  PIC 9(10)V99.
           05 UDate    PIC 9(8).
           05 UTime    PIC 9(6).
       WORKING-STORAGE SECTION.
       01 UserName     PIC X(20).
       01 Password     PIC X(20).
       01 Encpassword  PIC X(255).
       01 StoredPassword PIC X(255).
       01 UserID       PIC x(5).
       01 StatusCode   PIC 9(1) VALUE ZERO.
       01 EOF-FLAG     PIC X(1) VALUE 'N'.
       01  WS-FS pic x(2).
       LINKAGE SECTION.
       01 Return-Values.
           05 Return-UID PIC 9(5).
           05 Retrun-Cd  PIC X(2).
       PROCEDURE DIVISION USING Return-Values.
           perform MAIN-PROCEDURE
           exit program.
       MAIN-PROCEDURE.
            DISPLAY "Enter Username:"
            ACCEPT UserName.

            DISPLAY "Enter Password:"
            ACCEPT Password.

      *      CALL 'encryption' USING Password Encpassword.

            OPEN INPUT UserAccounts.

            PERFORM UNTIL EOF-FLAG = 'Y'
               READ UserAccounts INTO UserRecord
               AT END MOVE 'Y' TO EOF-FLAG
               NOT AT END
               display ULoginName
               IF ULoginName = UserName
                   THEN
                       MOVE UEncPsw TO StoredPassword
                       MOVE UID TO UserID
                END-IF
                END-READ
            END-PERFORM.
            CLOSE UserAccounts.

      *      CALL 'checkValidation' USING UserID Encpassword
      *      StoredPassword StatusCode.

      *         IF StatusCode = "1"
                    DISPLAY Password
                    DISPLAY StoredPassword
                    DISPLAY UserID
                IF Password = StoredPassword
                   DISPLAY "Login Successful."
                   MOVE UserID TO Return-UID
                   MOVE "00" TO Retrun-Cd
               ELSE
                   DISPLAY "Invalid Credentials."
                   MOVE 0 TO Return-UID
                   MOVE "01" TO Retrun-Cd
               END-IF.
            STOP RUN.
       END PROGRAM userLogin.
