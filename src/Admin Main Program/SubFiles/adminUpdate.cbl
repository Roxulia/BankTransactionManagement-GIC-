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
           05  ALoginName PIC X(20).
           05  AEncPsw    PIC X(255).
           05  ARole      pic X.

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


       LINKAGE SECTION.
       01  LNK-AID           PIC 9(5).
       01  LNK-Status        PIC XX.

       PROCEDURE DIVISION USING LNK-AID LNK-Status.

       Main-Section.

           *>DISPLAY "Enter Admin Id to update"
           *>ACCEPT LNK-AID

          *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          *> opening AdminAccounts data file and retrieving the RECORD

           MOVE LNK-AID TO AID
           OPEN I-O AdminFile
           IF WS-FileStatus NOT = '00'
               MOVE '99' TO LNK-Status
               GOBACK
           END-IF

           READ AdminFile KEY IS AID
               INVALID KEY
                   DISPLAY "Admin ID not found"
                   MOVE '99' TO LNK-Status
                   CLOSE AdminFile
                   GOBACK
           END-READ

           EVALUATE ARole
               WHEN 1 MOVE "Manager" TO RoleStr
               WHEN 2 MOVE "Staff"   TO RoleStr
               WHEN OTHER MOVE "Unknown" TO RoleStr
           END-EVALUATE

          *>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          *> Display current values prompting edit options

           DISPLAY "========================================"
           DISPLAY "Current Name : " ESC GREENX AName ESC RESETX
           DISPLAY "Current Role : " ESC GREENX RoleStr ESC RESETX
           DISPLAY "========================================"
           DISPLAY "Which field to update?"
           DISPLAY "1. Name"
           DISPLAY "2. Password"
           DISPLAY "3. Role"
           DISPLAY "4. Exit"
           DISPLAY "========================================"
           DISPLAY "Enter option code: "
           ACCEPT OptCode
           DISPLAY "========================================"

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<
          *>evaluating the option code, 1,2,3,4

           EVALUATE OptCode
               WHEN 1
                   DISPLAY "Enter new Name (20 chars): "
                   ACCEPT NewName
                   MOVE NewName TO AName

               WHEN 2
                   DISPLAY "Enter new Password: "
                   ACCEPT NewPsw

                   *>Call encryption submodule

                   CALL '../../UtilityFunctions/bin/encryption'
                       USING BY CONTENT NewPsw EncryptedPassword
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
                   MOVE NewRole TO ARole

               WHEN 4
                   CLOSE AdminFile
                   GOBACK

               WHEN OTHER
                   DISPLAY "Invalid option"
                   MOVE '99' TO LNK-Status
                   CLOSE AdminFile
                   GOBACK
           END-EVALUATE

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           *> Rewrite the updated record

           REWRITE AdminRecord
               INVALID KEY
                   DISPLAY "Error updating record " WS-FileStatus
                   MOVE "99" TO LNK-Status
               NOT INVALID KEY
                   DISPLAY "========================================"
                   DISPLAY "Record updated successfully"
                   DISPLAY "========================================"
                   MOVE "00" TO LNK-Status
           END-REWRITE

           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

           CLOSE AdminFile

           GOBACK.
