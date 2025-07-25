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
           COPY "../../Utility Functions/userFile.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FS         PIC XX.
       01 UserID         PIC 9(5) VALUE ZERO.
       01 OldPass        PIC X(20).
       01 NewPass1       PIC X(20).
       01 NewPass2       PIC X(20).
       01 EncOldPass     PIC X(32).
       01 EncNewPass     PIC X(32).
       01 StoredPass     PIC X(32).
       01 FoundUser      PIC X VALUE 'N'.
       01 EOF-FLAG       PIC X VALUE 'N'.
       01 StatusCode     PIC XX VALUE "00".

       copy '../../Utility Functions/colorCodes.cpy'.

       LINKAGE SECTION.
       01  LS-UserID      PIC 9(5).
       01  LNK-Status        PIC XX.

       PROCEDURE DIVISION USING LS-UserID,LNK-Status.
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
           if StatusCode not EQUAL "00"
               move StatusCode to LNK-Status
               exit PROGRAM
           end-if
           DISPLAY "Enter Current Password:".
           ACCEPT OldPass.

           call '../../Utility Functions/bin/encryption'
           using by REFERENCE OldPass EncOldPass

           if UEncPsw not equal EncOldPass
               move "95" to LNK-Status
               exit PROGRAM
           end-if

           DISPLAY "Enter New Password : "
           ACCEPT NewPass1

            *>Call encryption submodule
           CALL '../../Utility Functions/bin/userPassVal'
               using by REFERENCE OldPass STatuscode
           perform until statusCode equal "00"
               DISPLAY color-pink
               display "+++++++++++++++++++++++++++++++++++++++++++++++"
               evaluate statusCode
               WHEN "01"
                   DISPLAY "+ Error: Username cannot be empty +"
               WHEN "02"
                   DISPLAY "+ Error: Password cannot be empty +"
               WHEN "03"
                   DISPLAY "+ Error: Invalid length or format +"
               WHEN "04"
                   DISPLAY "+ Error: Password must contain at least "
                           "one uppercase letter +"
               WHEN "05"
                   DISPLAY "+ Error: Password must contain at least "
                           "one lowercase letter +"
               WHEN "06"
                   DISPLAY "+ Error: Password must contain at least "
                           "one number +"
               WHEN "07"
                   DISPLAY "+ Error: Password must contain at least "
                           "one special character +"
               WHEN "08"
                   DISPLAY "+ Error: Password must be at least 9 +"
                           "characters long +"
               END-EVALUATE
               display "+++++++++++++++++++++++++++++++++++++++++++++++"
               display esc resetx
               DISPLAY "Enter new Password: "
               ACCEPT NewPass1

               *>Call encryption submodule
               CALL '../../Utility Functions/bin/userPassVal'
                   using by REFERENCE NewPass1 STatuscode
           END-PERFORM

           DISPLAY "Enter new password again : "
           ACCEPT NewPass2

           perform until NewPass1 = NewPass2
               Display esc redx
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! Password Do not match !"
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!"
               Display esc resetx
               display "Enter New Password Again : "
               accept NewPass2
           END-PERFORM

           CALL '../../Utility Functions/bin/encryption'
               USING BY REFERENCE NewPass1 EncNewPass
           IF RETURN-CODE NOT = 0
               Display esc redx
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! Error in Encrypting password !"
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               Display esc resetx
               MOVE '04' TO LNK-Status
               CONTINUE
           END-IF

               *>remove the line following if encryption.cbl is ready
           *>MOVE NewPsw TO AEncPsw

           MOVE EncNewPass TO UEncPsw
           open i-o UserFile
           REWRITE UserRecord
               INVALID KEY
                   MOVE "99" TO LNK-Status
               NOT INVALID KEY
                   MOVE "00" TO LNK-Status
           END-REWRITE
           close UserFile.

       END PROGRAM updatePassword.
