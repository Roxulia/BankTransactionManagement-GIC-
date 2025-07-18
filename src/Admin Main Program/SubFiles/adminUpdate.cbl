      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-5-2025
      * Purpose:Updating Admin Info
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adminUpdate.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AdminFile ASSIGN TO '../../../data/AdminAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AID
               FILE STATUS IS WS-FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  AdminFile.
       01  AdminRecord.
           05  AID        PIC 9(5).
           05  AName      PIC X(20).
           05  ALoginName PIC X(25).
           05  AEncPsw    PIC X(32).
           05  role       PIC 9.

       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       77  WS-FileStatus     PIC XX.
       77  OptCode           PIC 9(1).
       77  NewName           PIC X(20).
       77  NewPsw            PIC X(255).
       77  EncryptedPassword PIC X(32).
       77  NewRole           PIC 9(1).
       77  RoleStr           PIC X(10).
       77  statuscode pic xx.
       77  ws-aid pic 9(5).


       LINKAGE SECTION.
       01  LNK-AID           PIC 9(5).
       01  LNK-Status        PIC XX.

       PROCEDURE DIVISION USING LNK-AID LNK-Status.

       Main-Section.
           INITIALIZE optcode
           INITIALIZE ws-aid
           move lnk-aid to ws-aid
           PERFORM Record-pointer
           INITIALIZE Optcode
           PERFORM UNTIL OptCode = 4
               PERFORM Update-Menu
               PERFORM Process-option
               IF OptCode >= 1 AND OptCode <= 3
                   PERFORM Update-record
               END-If
           END-PERFORM

           CLOSE AdminFile

           GOBACK.

       *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> opening AdminAccounts data file and retrieving the RECORD
       Record-pointer.

           MOVE ws-AID TO AID
           OPEN I-O AdminFile
           IF WS-FileStatus NOT = '00'
               MOVE '99' TO LNK-Status
               GOBACK
           END-IF

           READ AdminFile KEY IS AID
               INVALID KEY
                   DISPLAY "Admin ID not found"
                   MOVE '96' TO LNK-Status
                   CLOSE AdminFile
                   GOBACK
           END-READ

           EVALUATE role
               WHEN 1 MOVE "Manager" TO RoleStr
               WHEN 2 MOVE "Staff"   TO RoleStr
               WHEN OTHER MOVE "Unknown" TO RoleStr
           END-EVALUATE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> Display current values prompting edit options
       Update-Menu.

           DISPLAY "========================================"
           DISPLAY "=  Current Name : " ESC GREENX AName ESC RESETX
           DISPLAY "=  Current Role : " ESC GREENX RoleStr ESC RESETX
           DISPLAY "=  Current Password : " ESC GREENX "????" ESC RESETX
           DISPLAY "========================================"
           DISPLAY "=  Which field to update?"
           DISPLAY "=  1. Name"
           DISPLAY "=  2. Password"
           DISPLAY "=  3. Role"
           DISPLAY "=  4. Exit"
           DISPLAY "========================================"
           DISPLAY "=  Enter option code: "
           ACCEPT OptCode
           DISPLAY "========================================".

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>evaluating the option code, 1,2,3,4
       Process-option.

           EVALUATE OptCode
               WHEN 1
                   DISPLAY "=  Enter Full Name (max 20 chars):"
                   ACCEPT NewName
                   call '../../Utility Functions/bin/userNameVal'
                   using by REFERENCE newName , statusCode
                   
                   perform until statusCode equal "00"
                       DISPLAY esc redx "Invalid Name" esc resetx
                       DISPLAY "=  Enter Full Name (max 20 chars):"
                       ACCEPT newName
                       call '../../Utility Functions/bin/userNameVal'
                       using by REFERENCE newName , statusCode
                   END-PERFORM
                   MOVE NewName TO AName

               WHEN 2
                   DISPLAY "Enter new Password: "
                   ACCEPT NewPsw

                   *>Call encryption submodule
                   CALL '../../Utility Functions/bin/userPassVal'
                       using by REFERENCE newpsw STatuscode
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
                   ACCEPT NewPsw

                   *>Call encryption submodule
                   CALL '../../Utility Functions/bin/userPassVal'
                       using by REFERENCE newpsw STatuscode
                   END-PERFORM

                   CALL '../../UtilityFunctions/bin/encryption'
                       USING BY REFERENCE NewPsw EncryptedPassword
                   IF RETURN-CODE NOT = 0
                       DISPLAY "Error encrypting password. Aborting."
                       MOVE '04' TO LNK-Status
                   END-IF

                   *>remove the line following if encryption.cbl is ready
                   *>MOVE NewPsw TO AEncPsw

                   MOVE EncryptedPassword TO AEncPsw

               WHEN 3
                   DISPLAY "Enter new Role(1 for Manager, 2 for staff):"
                   ACCEPT NewRole
                   MOVE NewRole TO role

               WHEN 4
                   CLOSE AdminFile
                   CONTINUE

               WHEN OTHER
                   DISPLAY "Invalid option, please choose 1 to 4 :"
                   MOVE '99' TO LNK-Status
                   CONTINUE
           END-EVALUATE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> Rewrite the updated record
       Update-record.
           REWRITE AdminRecord
               INVALID KEY
                   DISPLAY "Error updating record " WS-FileStatus
                   MOVE "99" TO LNK-Status
               NOT INVALID KEY
                   DISPLAY "========================================"
                   DISPLAY "=      Record updated successfully     ="
                   DISPLAY "========================================"
                   MOVE "00" TO LNK-Status
           END-REWRITE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
