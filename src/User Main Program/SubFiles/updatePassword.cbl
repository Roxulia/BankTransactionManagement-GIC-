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
           SELECT UserFile ASSIGN TO '../../../data/UserAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

           DATA DIVISION.
       FILE SECTION.
       FD UserFile.
       01 UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UEncPsw     PIC X(255).
           05 UAddress    PIC X(20).
           05 Phone       PIC X(9).
           05 Balance     PIC 9(10)V99.
           05 UDate       PIC 9(8).
           05 UTime       PIC 9(6).

       WORKING-STORAGE SECTION.
       01  WS-FS         PIC XX.
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
       LINKAGE SECTION.
       01 LS-UserID      PIC 9(5).
       01  LNK-Status        PIC XX.
       01 LS-EncryptedPassword   PIC X(32).

       PROCEDURE DIVISION USING LS-UserID.
       MAIN-PROCEDURE.
           MOVE LS-UserID TO UserID
          
           OPEN INPUT UserFile
           IF WS-FS  = '35'
               DISPLAY "No file with name UserAccounts.DAT , creating"
               OPEN OUTPUT UserFile
               CLOSE UserFile
           END-IF
           CLOSE UserFile.
           
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE LS-UserID,UserRecord,statusCode

           DISPLAY "Enter Current Password:".
           ACCEPT OldPass.

            *>Call encryption submodule
                   CALL '../../Utility Functions/bin/userPassVal'
                       using by REFERENCE OldPass STatuscode
                   perform until statusCode equal "00"
                   evaluate statusCode
                   WHEN "01"
                       DISPLAY "Error: Username cannot be empty"
                   WHEN "02"
                       DISPLAY "Error: Password cannot be empty"
                   WHEN "03"
                       DISPLAY "Error: Invalid length or format"
                   WHEN "04"
                       DISPLAY "Error: Password must contain at least "
                        WITH NO ADVANCING
                       DISPLAY "one uppercase letter"
                   WHEN "05"
                       DISPLAY "Error: Password must contain at least "
                        WITH NO ADVANCING
                       DISPLAY "one lowercase letter"
                   WHEN "06"
                       DISPLAY "Error: Password must contain at least "
                        WITH NO ADVANCING
                       DISPLAY "one number"
                   WHEN "07"
                       DISPLAY "Error: Password must contain at least "
                        WITH NO ADVANCING
                       DISPLAY "one special character"
                   WHEN "08"
                       DISPLAY "Error: Password must be at least 9"
                       WITH NO ADVANCING
                       DISPLAY "characters long"
                   END-EVALUATE
                   DISPLAY "Enter new Password: "
                   ACCEPT NewPass1

                   *>Call encryption submodule
                   CALL '../../Utility Functions/bin/userPassVal'
                       using by REFERENCE NewPass1 STatuscode
                   END-PERFORM
                   CALL '../../Utility Functions/bin/encryption'
                       USING BY REFERENCE NewPass1 LS-EncryptedPassword
                   IF RETURN-CODE NOT = 0
                       DISPLAY "Error encrypting password. Aborting"
                       MOVE '04' TO LNK-Status
                       CONTINUE
                   END-IF

                   *>remove the line following if encryption.cbl is ready
                   *>MOVE NewPsw TO AEncPsw

                   MOVE LS-EncryptedPassword TO UEncPsw
           IF NewPass1 NOT = NewPass2
               DISPLAY "Passwords do not match."
               MOVE "08" TO StatusCode
               GOBACK
           END-IF  .    

       END PROGRAM updatePassword.
