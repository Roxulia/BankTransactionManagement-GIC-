      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-3-2025
      * Purpose:Creating User Account from admin
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userCreate.
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
       FD  UserFile.
       01  UserRecord.
       COPY "../../Utility Functions/userFile.cpy".

       WORKING-STORAGE SECTION.
       01  WS-FS               PIC XX.
       01  CurrentDate         PIC x(8).
       01  CurrentTime         PIC x(6).
       01  Dup-Flag            PIC X VALUE 'N'.
       01  RPSW                PIC 9(6).
       01  PlainPassword       PIC X(20).
       01  EncryptedPassword   PIC X(32).
       01  PrevUID             PIC 9(5) value 00000.
       01  EOF-Flag            PIC X value 'N'.
       01  PTR                 PIC 9(4)  COMP-5.
       01  I                   PIC 9(4)  COMP-5.
       01  statusCode          pic xx.
       01  temp-nrc pic x(30).
       01  UserData.
           05  ws-UID        PIC 9(5).
           05  ws-UName      PIC X(20).
           05  ws-ULoginName PIC X(25).
           05  ws-UAccNo     PIC 9(16).
           05  ws-UEncPsw    PIC X(32).
           05  ws-UNrc       PIC X(30).
           05  ws-UAddress   PIC X(20).
           05  ws-UPh        PIC X(11).
           05  ws-Balance    PIC s9(10)V99.
           05  ws-TrxCount   PIC 9(5).
           05  ws-UDate      PIC 9(8).
           05  ws-UTime      PIC 9(6).

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       LINKAGE SECTION.
       01  WS-ReturnCode       PIC 9(4) VALUE 0.

       PROCEDURE DIVISION USING WS-ReturnCode.

       Main-Section.
           PERFORM File-Check
           PERFORM Generate-UID
           PERFORM Generate-CardNo
           PERFORM Prompt-NRC
           call '../../Utility Functions/bin/getUserByNRC'
           using by REFERENCE temp-nrc UserData statusCode
           if statusCode equal "00"
               DISPLAY esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! User Already Existed !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY esc greenx
               display "************************"
               DISPLAY "* Name : " ws-UName
               DISPLAY "* Account Number : " ws-UAccNo
               DISPLAY "************************"
               DISPLAY esc resetx
               exit PROGRAM
           end-if
           move temp-nrc to unrc
           PERFORM Prompt-Box
           PERFORM ValidCheck-IniPsw
           PERFORM Generate-Login
           PERFORM Encryption-Call
           PERFORM Write-Record

           GOBACK.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Creating a new file to store data if not already exist
       File-Check.

           OPEN INPUT UserFile
           IF WS-FS  = '35'
               DISPLAY "No file with name UserAccounts.DAT , creating"
               OPEN OUTPUT UserFile
               DISPLAY "Created..."
               CLOSE UserFile
           END-IF
           CLOSE UserFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Opening the file for generating UID
       Generate-UID.

           OPEN INPUT UserFile
           IF WS-FS NOT = '00'
              display esc redx
              display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              DISPLAY "! Error opening UserAccounts.dat ("WS-FS") !"
              display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
              display esc redx
              MOVE 1 TO WS-ReturnCode
              GO TO End-Program
           END-IF

           MOVE 0       TO PrevUID
           MOVE 'N'      TO EOF-Flag

           PERFORM UNTIL EOF-Flag = 'Y'
               READ UserFile NEXT
                   AT END
                       MOVE PrevUID TO UID
                       ADD 1 TO UID
                       MOVE 'Y'    TO EOF-Flag
                   NOT AT END
                       ADD 1 TO PrevUID
               END-READ
           END-PERFORM
           CLOSE UserFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Generating user bank account Number
       Generate-CardNo.

           CALL '../../Utility Functions/bin/generateCardNo'
               USING BY REFERENCE UID,UAccNo.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>NRC prompt box
       Prompt-NRC.

           CALL '../../Utility Functions/bin/userNRCVal'
               USING BY REFERENCE temp-nrc
           if temp-nrc EQUAL "EXIT"
               DISPLAY color-pink
               display "++++++++++++++++++++++++++"
               DISPLAY "+ Going Back to Mainmenu +"
               display "++++++++++++++++++++++++++"
               display esc resetx
               exit PROGRAM
           END-IF.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Prompt display for user input
       Prompt-Box.

           DISPLAY "----- Create New User Account -----"
           DISPLAY "Account No : " UAccNo.

           DISPLAY "=  Enter Full Name (max 20 chars):"
           ACCEPT UName
           call '../../Utility Functions/bin/userNameVal'
           using by REFERENCE UName , statusCode

           perform until statusCode equal "00"
               DISPLAY esc redx "Invalid Name" esc resetx
               DISPLAY "=  Enter Full Name (max 20 chars):"
               ACCEPT UName
               call '../../Utility Functions/bin/userNameVal'
               using by REFERENCE UName , statusCode
           END-PERFORM

           DISPLAY "Enter Address (max 20 chars): "
           ACCEPT UAddress.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Prompt display for PH NO and valid check
       ValidCheck-IniPsw.

           CALL '../../Utility Functions/bin/phoneValidCheck'
           USING BY REFERENCE UPh
           if Uph EQUAL "EXIT"
               DISPLAY color-pink
               display "++++++++++++++++++++++++++"
               display "+ Going back to Mainmenu +"
               display "++++++++++++++++++++++++++"
               display esc resetx
               exit PROGRAM
           END-IF

           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
           *>Generate random initial password

           COMPUTE RPSW = FUNCTION RANDOM() * 1000000.
           MOVE RPSW TO PlainPassword.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Generating Login name ( full name + ID )
       Generate-Login.

           MOVE FUNCTION LOWER-CASE(UName) to ULoginName

           MOVE 1 TO PTR
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF ULoginName
               IF ULoginName(I:1) NOT = SPACE
                  MOVE ULoginName(I:1) TO ULoginName(PTR:1)
                  ADD 1 TO PTR
               END-IF
           END-PERFORM

           STRING
               ULoginName(1:PTR - 1) DELIMITED BY SIZE
               UID               DELIMITED BY SIZE
           INTO ULoginName

           DISPLAY ESC REDX"======================================="
           DISPLAY "!! REMEMBER YOUR LOGIN INFOS !! "
           DISPLAY "~  LoginName : "ESC GREENX WITH NO ADVANCING
           DISPLAY ULoginName ESC REDX
           DISPLAY "~  Password  : "ESC GREENX WITH NO ADVANCING
           DISPLAY PlainPassword
           DISPLAY ESC REDX "!! DON'T FORGET TO" WITH NO ADVANCING
           DISPLAY " CHANGE YOUR PASSWORD !!"
           DISPLAY "========================================"ESC RESETX.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *> Call encryption submodule( to uncomment after encryption sub)
       Encryption-Call.

           CALL '../../Utility Functions/bin/encryption'
           USING BY REFERENCE PlainPassword,EncryptedPassword
           IF RETURN-CODE NOT = 0
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! Error encrypting password. Aborting !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
               MOVE 4 TO WS-ReturnCode
               GO TO End-Program
           END-IF

           MOVE EncryptedPassword TO UEncPsw.
           *>DISPLAY UEncPsw. *>for test ,comment this line out

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Writing a new record to user file
       Write-Record.

           MOVE    zero               TO      Balance
           MOVE    0               TO      TrxCount
           move FUNCTION CURRENT-DATE(1:8) to CurrentDate
           move FUNCTION CURRENT-DATE(9:6) to CurrentTime
           MOVE    CurrentDate     TO      UDate
           MOVE    CurrentTime     TO      UTime
           DISPLAY CurrentDate " " UDate
           DISPLAY CurrentTime " " UTime
      *     DISPLAY UserRecord
           OPEN I-O UserFile
           WRITE UserRecord
               INVALID KEY
                   DISPLAY "Error writing to file (Status=" WS-FS ")"
                   MOVE 2 TO WS-ReturnCode
               NOT INVALID KEY
                   DISPLAY ESC GREENX
                   DISPLAY "**************************************"
                   DISPLAY "* User account created successfully  *"
                   DISPLAY "**************************************"
                   MOVE 0 TO WS-ReturnCode
                   display esc RESETX
           END-WRITE

           CLOSE UserFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Sub routine to end the program if something happened
       End-Program.

           GOBACK.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
