      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-4-2025
      * Purpose:Creating Admin Account from admin
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adminCreate.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          *>fix this file directory plsss <3
           SELECT AdminFile
           ASSIGN TO '../../../data/AdminAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  AdminFile.
       01  AdminRecord.
           05  AID        PIC 9(5).
           05  AName      PIC X(20).
           05  ALoginName PIC X(20).
           05  AEncPsw    PIC X(255).
           05  ARole      Pic X.

       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       01  WS-FS            PIC XX.
       01  Dup-Flag         PIC X VALUE 'N'.
       01  RPSW               PIC 9(6).
       01  PlainPassword      PIC X(20).
       01  EncryptedPassword  PIC X(32).
       01  PrevAID            PIC 9(5) value 00000.
       01  EOF-Flag           PIC X value 'N'.
       01  PTR                 PIC 9(4)  COMP-5.
       01  I                   PIC 9(4)  COMP-5.

       LINKAGE SECTION.
       01  WS-ReturnCode       PIC 9(4) VALUE 0.

       PROCEDURE DIVISION USING WS-ReturnCode.

       Main-Section.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Creating a new file to store data if not already exist

           OPEN INPUT AdminFile
           IF WS-FS  = '35'
               DISPLAY "No file with name AdminAccounts.DAT , creating"
               OPEN OUTPUT AdminFile
               CLOSE AdminFile
           END-IF
           CLOSE AdminFile

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Opening the file for generating AID

           OPEN INPUT AdminFile
           IF WS-FS NOT = '00'
              DISPLAY "Error opening AdminAccounts.dat (Status="WS-FS")"
              MOVE 1 TO WS-ReturnCode
              GO TO End-Program
           END-IF

           MOVE 0       TO PrevAID
           MOVE 'N'      TO EOF-Flag

           PERFORM UNTIL EOF-Flag = 'Y'
               READ AdminFile NEXT
                   AT END
                       MOVE PrevAID TO AID
                       ADD 1 TO AID
                       *>DISPLAY 'current UID :' UID
                       MOVE 'Y'    TO EOF-Flag
                   NOT AT END
                       *> Update PrevUID to the UID just read
                       ADD 1 TO PrevAID
                       *>DISPLAY 'NOT at end ' PrevUID
               END-READ
           END-PERFORM
           CLOSE AdminFile

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Prompt display for user input and generate a rnd password

           DISPLAY "====== Create New Admin Account ======"
           DISPLAY "Generated AID: " AID

           DISPLAY "Enter Full Name (max 20 chars):"
           ACCEPT AName

           COMPUTE RPSW = FUNCTION RANDOM() * 1000000.
           MOVE RPSW TO PlainPassword.

           MOVE 2 TO ARole.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Generating Login name ( full name + ID )

           MOVE FUNCTION LOWER-CASE(AName) to ALoginName

           MOVE 1 TO PTR
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF ALoginName
               IF ALoginName(I:1) NOT = SPACE
                  MOVE ALoginName(I:1) TO ALoginName(PTR:1)
                  ADD 1 TO PTR
               END-IF
           END-PERFORM

           STRING
               ALoginName(1:PTR - 1) DELIMITED BY SIZE
               AID               DELIMITED BY SIZE
           INTO ALoginName

           DISPLAY ESC REDX"======================================"
           DISPLAY "!! REMEMBER YOUR LOGIN INFOS !! "
           DISPLAY "~  LoginName : "ESC GREENX WITH NO ADVANCING
           DISPLAY ALoginName ESC REDX
           DISPLAY "~  Password  : "ESC GREENX WITH NO ADVANCING
           DISPLAY PlainPassword
           DISPLAY ESC REDX "!! DON'T FORGET TO" WITH NO ADVANCING
           DISPLAY " CHANGE YOUR PASSWORD !!"
           DISPLAY "======================================="

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
           *> Call encryption submodule

           CALL '../../UtilityFunctions/bin/encryption'
           USING BY CONTENT PlainPassword
                                              EncryptedPassword
           IF RETURN-CODE NOT = 0
               DISPLAY "Error encrypting password. Aborting."
               MOVE 4 TO WS-ReturnCode
               GO TO End-Program
           END-IF

           *>remove the line following if encryption.cbl is ready
           *>MOVE PlainPassword TO AEncPsw

           MOVE EncryptedPassword TO AEncPsw

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>wRITING A NEW RECORD TO THE AdminAccounts.DAT

           OPEN I-O AdminFile
           WRITE AdminRecord
               INVALID KEY
                   DISPLAY "Error writing to file (Status=" WS-FS ")"
                   MOVE 2 TO WS-ReturnCode
               NOT INVALID KEY
                   DISPLAY ESC GREENX "Admin account created"
                       WITH NO ADVANCING
                   DISPLAY "successfully."
                   MOVE 0 TO WS-ReturnCode
                   display esc RESETX
           END-WRITE

           CLOSE AdminFile.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Sub routine to end the program if something happened
           End-Program.
           exit program.

       END PROGRAM adminCreate.
