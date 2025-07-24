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
       COPY "../../Utility Functions/userFile.cpy".


       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       77  WS-FileStatus     PIC XX.
       77  OptCode           PIC 9(1).
       77  NewName           PIC X(20).
       77  NewPsw            PIC X(20).
       77  newph             pic x(11).
       77  new_add           pic x(20).
       77  EncryptedPassword PIC X(32).
       77  statusCode pic xx.
       77  ws-nrc pic x(30).
       77  is_exit pic x.

       LINKAGE SECTION.
       01  LNK-NRC           PIC x(30).
       01  LNK-Status        PIC XX.

       PROCEDURE DIVISION USING LNK-NRC LNK-Status.

       Main-Section.
           INITIALIZE optcode
           INITIALIZE ws-nrc
           move 'n' to is_exit
           move LNK-nrc to ws-nrc
           call '../../Utility Functions/bin/getUserByNRC'
           using by REFERENCE ws-nrc,UserRecord,statusCode

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
                   DISPLAY "=  Enter Full Name (max 20 chars):"
                   ACCEPT NewName
                   if newName = "EXIT" or newname = "exit"
                       DISPLAY "CANCEL NAME UPDATE"
                       move 'y' to is_exit
                       NEXT SENTENCE
                   END-IF
                   call '../../Utility Functions/bin/userNameVal'
                   using by REFERENCE newName , statusCode

                   perform until statusCode equal "00"
                       DISPLAY esc redx "Invalid Name" esc resetx
                       DISPLAY "=  Enter Full Name (max 20 chars):"
                       ACCEPT newName
                       if newName = "EXIT" or newname = "exit"
                           DISPLAY "CANCEL NAME UPDATE"
                           move 'y' to is_exit
                           Next SENTENCE
                       END-IF
                       call '../../Utility Functions/bin/userNameVal'
                       using by REFERENCE newName , statusCode
                   END-PERFORM
                   MOVE NewName TO UName
                   move 'n' to is_exit

               WHEN 2
                   DISPLAY "Enter new Password: "
                   ACCEPT NewPsw
                   if newpsw = "EXIT" or newpsw = "exit"
                       DISPLAY "Cancel Password Update"
                       move 'y' to is_exit
                       next SENTENCE
                   END-IF
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
                   if newpsw = "EXIT" or newpsw = "exit"
                       DISPLAY "Cancel Password Update"
                       move 'y' to is_exit
                       NEXT SENTENCE
                   END-IF
                   *>Call encryption submodule
                   CALL '../../Utility Functions/bin/userPassVal'
                       using by REFERENCE newpsw STatuscode
                   END-PERFORM
                   CALL '../../Utility Functions/bin/encryption'
                       USING BY REFERENCE NewPsw EncryptedPassword
                   IF RETURN-CODE NOT = 0
                       DISPLAY "Error encrypting password. Aborting"
                       MOVE '04' TO LNK-Status
                       move 'y' to is_exit
                       next SENTENCE
                   END-IF

                   *>remove the line following if encryption.cbl is ready
                   *>MOVE NewPsw TO AEncPsw

                   MOVE EncryptedPassword TO UEncPsw
                   move 'n' to is_exit

               WHEN 3
                   DISPLAY "==========================================="
                   DISPLAY "=  Enter new Address: "
                   ACCEPT new_add
                   if new_add = "EXIT" or new_add = "exit"
                       DISPLAY "CANCEL ADDRESS UPDATE"
                       move 'y' to is_exit
                       NEXT SENTENCE
                   END-IF
                   move 'n' to is_exit
                   move new_add to Uaddress
               WHEN 4
                   CALL '../../Utility Functions/bin/phoneValidCheck'
                   USING BY REFERENCE newPh
                   if newph = "EXIT" or newph = "exit"
                       DISPLAY "CANCEL PHONE UPDATE"
                       move 'y' to is_exit
                       next SENTENCE
                   END-IF
                   move 'n' to is_exit
                   move newph to uph
               WHEN 5
                   CLOSE UserFile
                   move 'y' to is_exit
                   CONTINUE

               WHEN OTHER
                   DISPLAY "Invalid option, please choose 1 to 5 :"
                   MOVE '99' TO LNK-Status
                   CONTINUE
           END-EVALUATE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *> Rewrite the updated record
       Update-record.
           if is_exit = 'n'
           REWRITE UserRecord
               INVALID KEY
                   DISPLAY "Error updating record " WS-FileStatus
                   MOVE "99" TO LNK-Status
               NOT INVALID KEY
                   DISPLAY "========================================"
                   DISPLAY "=      Record updated successfully     ="
                   DISPLAY "========================================"
                   MOVE "00" TO LNK-Status
           END-REWRITE
           END-IF.

       exit-process.
           exit program.
       end PROGRAM userUpdate.
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
