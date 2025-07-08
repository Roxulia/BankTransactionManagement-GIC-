      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 8/7/2025
      * Purpose: Bank Transaction System
      * Tectonics: cobc
      ******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. updatePassword.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserAccounts ASSIGN TO "UserAccounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempFile ASSIGN TO "TempUser.tmp"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD UserAccounts.
       01 UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UEncPsw     PIC X(255).
           05 UAddress     PIC X(20).
           05 Phone       PIC 9(9).
           05 Balance     PIC 9(10)V99.
           05 UDate       PIC 9(8).
           05 UTime       PIC 9(6).

       FD TempFile.
       01 TempRecord     PIC X(335).

       WORKING-STORAGE SECTION.
       01 UserID         PIC 9(5) VALUE ZERO.
       01 OldPass        PIC X(20).
       01 NewPass1       PIC X(20).
       01 NewPass2       PIC X(20).
       01 EncOldPass     PIC X(255).
       01 EncNewPass     PIC X(255).
       01 StoredPass     PIC X(255).
       01 FoundUser      PIC X VALUE 'N'.
       01 EOF-FLAG       PIC X VALUE 'N'.
       01 StatusCode     PIC XX VALUE "00".
       01 UpperCount     PIC 9(2) VALUE 0.
       01 LowerCount     PIC 9(2) VALUE 0.
       01 DigitCount     PIC 9(2) VALUE 0.
       01 SpecialCount   PIC 9(2) VALUE 0.

       LINKAGE SECTION.
       01 LS-UserID      PIC 9(5).

       PROCEDURE DIVISION USING LS-UserID.
       MAIN-PROCEDURE.
           MOVE LS-UserID TO UserID

           DISPLAY "Enter Current Password:".
           ACCEPT OldPass.

           DISPLAY "Enter New Password:".
           ACCEPT NewPass1.

           DISPLAY "Confirm New Password:".
           ACCEPT NewPass2.

           IF NewPass1 NOT = NewPass2
               DISPLAY "Passwords do not match."
               MOVE "08" TO StatusCode
               GOBACK
           END-IF

           PERFORM VALIDATE-PASSWORD-RULES

           OPEN INPUT UserAccounts
           OPEN OUTPUT TempFile

           PERFORM READ-USER-RECORD UNTIL EOF-FLAG = 'Y'

           CLOSE UserAccounts
           CLOSE TempFile

           IF FoundUser = 'Y'
               CALL "SYSTEM" USING "rm -f UserAccounts.dat"
               CALL "SYSTEM" USING "mv TempUser.tmp UserAccounts.dat"
               DISPLAY "Password updated successfully."
               MOVE "00" TO StatusCode
           ELSE
               DISPLAY "User not found or incorrect old password."
               MOVE "99" TO StatusCode
               CALL "SYSTEM" USING "rm -f TempUser.tmp"
           END-IF

           GOBACK.

           
     >*      * Validate password complexity rules                       *
     >*      *----------------------------------------------------------*
       VALIDATE-PASSWORD-RULES.
            DISPLAY "Validating password rules..."
             *> Rule 1: Minimum length check
           IF FUNCTION LENGTH(NewPass1) < 8
               DISPLAY "Error: Password must be at least 8 characters."
               MOVE "08" TO StatusCode
               GOBACK
           END-IF

           
           *> Rule 2: Uppercase letter check
           INSPECT NewPass1 TALLYING
               UpperCount FOR ALL "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           IF UpperCount = 0
           DISPLAY "Error:At least one uppercase letter."
               MOVE "04" TO StatusCode
               GOBACK
           END-IF

            *> Rule 3: Lowercase letter check
           INSPECT NewPass1 TALLYING
               LowerCount FOR ALL "abcdefghijklmnopqrstuvwxyz"
           IF LowerCount = 0
               DISPLAY "Error: At least one lowercase letter."
               MOVE "05" TO StatusCode
               GOBACK
           END-IF

           *> Rule 4: Digit check
           INSPECT NewPass1 TALLYING
               DigitCount FOR ALL "0123456789"
           IF DigitCount = 0
               DISPLAY "Error: Password must contain one digit."
               MOVE "06" TO StatusCode
               GOBACK
           END-IF

           *> Rule 5: Special character check
           INSPECT NewPass1 TALLYING
               SpecialCount FOR ALL "!@#$%^&*()-_=+[]{}|;:,.<>?/"
           IF SpecialCount = 0
               DISPLAY "Error:At least one special character."
               MOVE "07" TO StatusCode
               GOBACK
           END-IF
           .

     >*  *----------------------------------------------------------------*
     >*  * Read each record and update password if match found            *
     >*  *----------------------------------------------------------------*
       READ-USER-RECORD.
           READ UserAccounts INTO UserRecord
               AT END MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   IF UID = UserID
                       MOVE UEncPsw TO StoredPass
                       CALL 'encryption' USING OldPass EncOldPass
                       IF EncOldPass = StoredPass
                           THEN
                            CALL 'encryption' USING NewPass1 EncNewPass
                               MOVE EncNewPass TO UEncPsw
                               MOVE 'Y' TO FoundUser
                       END-IF
                   END-IF
                   WRITE TempRecord FROM UserRecord
           END-READ
           .

       END PROGRAM updatePassword.
