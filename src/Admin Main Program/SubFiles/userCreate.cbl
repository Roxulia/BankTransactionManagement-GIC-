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
           SELECT UserFile ASSIGN TO 'XXXXXX/UserAccounts.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  UserFile.
       01  UserRecord.
           05  UID        PIC 9(5).
           05  UName      PIC X(20).
           05  ULoginName PIC X(20).
           05  UEncPsw    PIC X(255).
           05  UAddress   PIC X(20).
           05  UPh        PIC 9(9).
           05  Balance    PIC 9(10)V99.
           05  UDate      PIC 9(8).
           05  UTime      PIC 9(6).

       WORKING-STORAGE SECTION.
       01  WS-FS               PIC XX.
       01  CurrentDate         PIC 9(8).
       01  CurrentTime         PIC 9(6).
       01  Dup-Flag            PIC X VALUE 'N'.
       01  RPSW               PIC 9(6).
       01  PlainPassword       PIC X(20).
       01  EncryptedPassword   PIC X(32).
       01  PrevUID             PIC 9(5) value 00000.
       01  EOF-Flag            PIC X value 'N'.
       01  PTR                 PIC 9(4)  COMP-5.
       01  I                   PIC 9(4)  COMP-5.
       77  RAW-PHONE-IN     PIC X(20).
       77  DIGITS-ONLY      PIC X(20).
       77  DIGIT-COUNT      PIC 9(2) COMP.
       77  PHONE-LEN        PIC 9(2) COMP.
       77  J                PIC 9(2) COMP.
       77  VALID-PHONE         PIC X VALUE 'N'.

       *>For display colors
       77  RED-CODE            PIC X(6) VALUE "[1;31m".*> set red
       77  ESC                 PIC X    VALUE X"1B".   *> ASCII ESC
       77  RESET-CODE          PIC X(4) VALUE "[0m".   *> reset
       77  GREEN-CODE          PIC X(6) VALUE "[1;32m".*>set green

       LINKAGE SECTION.
       01  WS-ReturnCode       PIC 9(4) VALUE 0.

       PROCEDURE DIVISION USING WS-ReturnCode.

       Main-Section.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Creating a new file to store data if not already exist

           OPEN INPUT UserFile
           IF WS-FS  = '35'
               DISPLAY "No file with name AdminAccounts.DAT , creating"
               OPEN OUTPUT UserFile
               CLOSE UserFile
           END-IF
           CLOSE UserFile

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Opening the file for generating UID

           OPEN INPUT UserFile
           IF WS-FS NOT = '00'
              DISPLAY "Error opening UserAccounts.dat (Status="WS-FS")"
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
           CLOSE UserFile

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Prompt display for user input

           DISPLAY "----- Create New User Account -----"
           DISPLAY "Generated UID: " UID.

           DISPLAY "Enter Full Name (max 20 chars): "
           ACCEPT UName

           DISPLAY "Enter Address (max 20 chars): "
           ACCEPT UAddress

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Prompt display for PH NO and valid check

           PERFORM UNTIL VALID-PHONE = 'Y'
               *> 1) Prompt and read
               DISPLAY "Enter Phone (exactly 9 digits): "
               ACCEPT RAW-PHONE-IN

               *> 2) Strip out non-digits
               MOVE SPACES TO DIGITS-ONLY
               MOVE FUNCTION TRIM(RAW-PHONE-IN) TO RAW-PHONE-IN

              *> 2) Count numeric chars manually
               MOVE 0 TO DIGIT-COUNT
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF RAW-PHONE-IN
                   IF RAW-PHONE-IN(I:1) IS NUMERIC
                      ADD 1 TO DIGIT-COUNT
                   END-IF
               END-PERFORM

               *> 3) If exactly 9, build the digit string; else error
               IF DIGIT-COUNT = 9
                   *> reset and extract
                   MOVE SPACES TO DIGITS-ONLY
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
                       IF RAW-PHONE-IN(I:1) IS NUMERIC
                          STRING RAW-PHONE-IN(I:1)
                                 DELIMITED BY SIZE
                                 INTO DIGITS-ONLY
                       END-IF
                   END-PERFORM
                   MOVE 'Y' TO VALID-PHONE
               ELSE
                  DISPLAY ESC RED-CODE">> ERROR: You must enter "
                       WITH NO ADVANCING
                  DISPLAY "exactly 9 digits." ESC RESET-CODE
               END-IF
           END-PERFORM

           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Generate random initial password

           COMPUTE RPSW = FUNCTION RANDOM() * 1000000.
           MOVE RPSW TO PlainPassword.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Generating Login name ( full name + ID )

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

           DISPLAY ESC RED-CODE"======================================="
           DISPLAY "!! REMEMBER YOUR LOGIN INFOS !! "
           DISPLAY "~  LoginName : "ESC GREEN-CODE WITH NO ADVANCING
           DISPLAY ULoginName ESC RED-CODE
           DISPLAY "~  Password  : "ESC GREEN-CODE WITH NO ADVANCING
           DISPLAY PlainPassword
           DISPLAY ESC RED-CODE "!! DON'T FORGET TO" WITH NO ADVANCING
           DISPLAY " CHANGE YOUR PASSWORD !!"
           DISPLAY "=========================================="

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *> Call encryption submodule( to uncomment after encryption sub)

           CALL 'encryption' USING BY CONTENT PlainPassword
                                              EncryptedPassword
           IF RETURN-CODE NOT = 0
               DISPLAY "Error encrypting password. Aborting."
               MOVE 4 TO WS-ReturnCode
               GO TO End-Program
           END-IF

           MOVE EncryptedPassword TO UEncPsw
           DISPLAY UEncPsw
          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Writing a new record to user file

           MOVE    0               TO      Balance
           ACCEPT  CurrentDate     FROM    DATE
           ACCEPT  CurrentTime     FROM    TIME
           MOVE    CurrentDate     TO      UDate
           MOVE    CurrentTime     TO      UTime

           OPEN I-O UserFile
           WRITE UserRecord
               INVALID KEY
                   DISPLAY "Error writing to file (Status=" WS-FS ")"
                   MOVE 2 TO WS-ReturnCode
               NOT INVALID KEY
                   DISPLAY ESC GREEN-CODE "User account created"
                       WITH NO ADVANCING
                   DISPLAY "successfully."

                   MOVE 0 TO WS-ReturnCode
           END-WRITE

           CLOSE UserFile.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          *>Sub routine to end the program if something happened

           End-Program.
           STOP RUN.

          *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       END PROGRAM userCreate.
