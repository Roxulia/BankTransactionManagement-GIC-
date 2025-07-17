      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-10-2025
      * Purpose:Updating User Info
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userUpdate.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserFile ASSIGN TO '../../../data/UserAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  UserFile.
       01  UserRecord.
           05  UID        PIC 9(5).
           05  UName      PIC X(20).
           05  ULoginName PIC X(25).
           05  UEncPsw    PIC X(32).
           05  UAddress   PIC X(20).
           05  UPh        PIC x(9).
           05  Balance    PIC 9(10)V99.
           05  TrxCount   PIC 9(5).
           05  UDate      PIC 9(8).
           05  UTime      PIC 9(6).

       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       77  WS-FileStatus     PIC XX.
       77  OptCode           PIC 9(1).
       77  NewName           PIC X(20).
       77  NewPsw            PIC X(20).
       77  EncryptedPassword PIC X(32).
       77  statusCode pic xx.
       77  ws-uid pic 9(5).

       LINKAGE SECTION.
       01  LNK-UID           PIC 9(5).
       01  LNK-Status        PIC XX.

       PROCEDURE DIVISION USING LNK-UID LNK-Status.

       Main-Section.
           INITIALIZE optcode
           INITIALIZE ws-uid
           move LNK-UID to ws-uid
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE ws-UID,UserRecord,statusCode

           if statusCode not EQUAL "00"
               move statuscode to LNK-Status
               go to exit-process
           ELSE
           open i-o UserFile
           if Ws-filestatus not EQUAL "00"
               move "99" to LNK-Status
               go to exit-process
           end-if
           PERFORM UNTIL OptCode = 5
               PERFORM Update-Menu
               PERFORM Process-option
               IF OptCode >= 1 AND OptCode <= 4
                   PERFORM Update-record
               END-IF
           END-PERFORM

           CLOSE UserFile

           GO to exit-process.

       *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> opening UserAccounts data file and retrieving the RECORD
       Record-pointer.

           MOVE LNK-UID TO UID
           OPEN I-O UserFile
           IF WS-FileStatus NOT = '00'
               MOVE '99' TO LNK-Status
               GOBACK
           END-IF

           READ UserFile KEY IS UID
               INVALID KEY
                   DISPLAY "User ID not found"
                   MOVE '99' TO LNK-Status
                   CLOSE UserFile
                   GOBACK
           END-READ.

       *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> Display current values prompting edit options
       Update-Menu.

           DISPLAY "========================================"
           DISPLAY "=  Current Name : " ESC GREENX UName ESC RESETX
           DISPLAY "=  Current Password : "ESC GREENX "????" ESC RESETX
           DISPLAY "=  Current Address : "ESC GREENX UAddress ESC RESETX
           DISPLAY "=  Current Phone : " ESC GREENX UPh ESC RESETX
           DISPLAY "========================================"
           DISPLAY "=  Which field to update?"
           DISPLAY "=  1. Name"
           DISPLAY "=  2. Password"
           DISPLAY "=  3. Address"
           DISPLAY "=  4. Phone"
           DISPLAY "=  5. Exit"
           DISPLAY "========================================"
           DISPLAY "=  Enter option code: "
           ACCEPT OptCode
           DISPLAY "========================================".

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>evaluating the option code, 1,2,3,4
       Process-option.

           EVALUATE OptCode
               WHEN 1
                   DISPLAY "Enter new Name (20 chars): "
                   ACCEPT NewName
                   MOVE NewName TO UName

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
                   CALL '../../Utility Functions/bin/encryption'
                       USING BY REFERENCE NewPsw EncryptedPassword
                   IF RETURN-CODE NOT = 0
                       DISPLAY "Error encrypting password. Aborting"
                       MOVE '04' TO LNK-Status
                       CONTINUE
                   END-IF

                   *>remove the line following if encryption.cbl is ready
                   *>MOVE NewPsw TO AEncPsw

                   MOVE EncryptedPassword TO UEncPsw

               WHEN 3
                   DISPLAY "==========================================="
                   DISPLAY "=  Enter new Address: "
                   ACCEPT UAddress

               WHEN 4
                   CALL '../../Utility Functions/bin/phoneValidCheck'
                   USING BY REFERENCE UPh

               WHEN 5
                   CLOSE UserFile
                   CONTINUE

               WHEN OTHER
                   DISPLAY "Invalid option, please choose 1 to 5 :"
                   MOVE '99' TO LNK-Status
                   CONTINUE
           END-EVALUATE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> Rewrite the updated record
       Update-record.
           REWRITE UserRecord
               INVALID KEY
                   DISPLAY "Error updating record " WS-FileStatus
                   MOVE "99" TO LNK-Status
               NOT INVALID KEY
                   DISPLAY "========================================"
                   DISPLAY "=      Record updated successfully     ="
                   DISPLAY "========================================"
                   MOVE "00" TO LNK-Status
           END-REWRITE.

       exit-process.
           exit program.
       end PROGRAM userUpdate.
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
