      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-11-2025
      * Purpose:making deposits for a user from admin side
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxDeposit.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.
           SELECT UserFile ASSIGN TO '../../../../data/UserAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

           SELECT TrxFile ASSIGN TO '../../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  UserFile.
       01  UserRecord.
           05  UID        PIC 9(5).
           05  UName      PIC X(20).
           05  ULoginName PIC X(25).
           05  UEncPsw    PIC X(32).
           05  UAddress   PIC X(20).
           05  UPh        PIC X(9).
           05  Balance    PIC 9(10)V99.
           05  TrxCount   PIC 9(5).
           05  UDate      PIC 9(6).
           05  UTime      PIC 9(6).

       FD  TrxFile.
       01  TransactionRecord.
           05  TrxID       PIC 9(10).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10).
           05  TrxType     PIC 9.  *>  1 = SENT
                                   *>  2 = RECEIVED
                                   *>  3 = DEPOSIT
                                   *>  4 = WITHDRAW
           05  TimeStamp   PIC 9(16).

       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       01  WS-FS               PIC XX.
       01  depoAmo             PIC 9(10).

       01  CurrentDate         PIC 9(6).
       01  CurrentTime         PIC 9(6).

       01  WS-TrxBaseID        PIC 9(5).
       01  WS-TrxFullID        PIC X(10).


       01  minDspDepo      PIC Z(10).
       01  maxDspDepo      PIC Z(10).
       01  depoDsp         PIC Z(10).

       *>For trxConstant VALUES
       COPY "../../Utility Functions/trxConstants.cpy".

      *LINKAGE section.

       01  userId          PIC 9(5).
       01  optStatus       PIC 9(2).

       PROCEDURE DIVISION.*> using REFERENCE userId,optStatus.
       Main-Section.
           PERFORM TEST-HELPER
           PERFORM RECORD-POINTER
           PERFORM TRXID-GENERATE
           PERFORM AMOUNT-VALID-PROMPT-BOX
           PERFORM File-Check
           PERFORM WRITE-TRX
           PERFORM BALANCE-UPDATE
           GOBACK.
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Find the designated user record from the user file .
       TEST-HELPER.
           DISPLAY "================================================="
           DISPLAY "ENTER UID TO MAKE DEPOSIT :"
           ACCEPT userId.
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Find the designated user record from the user file .
       RECORD-POINTER.

           OPEN I-O UserFile
           MOVE userId TO UID
           READ UserFile KEY IS UID
               INVALID KEY
                   DISPLAY ESC REDX "[ERROR] User not found." ESC RESETX
                   MOVE 44 TO optStatus
                   CLOSE UserFile
                   GOBACK
           END-READ.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Generate a unique trxid ( TrxCount+1) + S,R,D,W + SenderID
       *> Sent, received , deposit , withdraw , D here since this sub file is for deposit
       TRXID-GENERATE.

           ADD 1 TO TrxCount
           MOVE TrxCount TO TrxID.

           STRING
               FUNCTION NUMVAL(WS-TrxBaseID) DELIMITED BY SIZE
               WS-TrxDepoPrefix DELIMITED BY SIZE
               FUNCTION NUMVAL(userId) DELIMITED BY SIZE
               INTO WS-TrxFullID.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Prompt box display for accepting amount deposit and validate Amount
       *>loop the amount prompt box, until the amount is valid or entered 'EXIT'
       AMOUNT-VALID-PROMPT-BOX.

           MOVE minAmoDepo to minDspDepo
           MOVE maxAmoDepo to maxDspDepo

           DISPLAY "================================================="
           PERFORM UNTIL depoAmo >= minAmoDepo AND depoAmo <= maxAmoDepo
               DISPLAY "Enter deposit amount " WITH NO ADVANCING
               DISPLAY "(Min: " FUNCTION TRIM(minDspDepo)
                   WITH NO ADVANCING
               DISPLAY " Max: " FUNCTION TRIM(maxDspDepo) "):"
               ACCEPT depoAmo

               IF depoAmo < minAmoDepo
                   OR depoAmo > maxAmoDepo
                   DISPLAY "==========================================="
                   DISPLAY ESC REDX "Amount out of allowed range."
                   DISPLAY ESC RESETX
                   DISPLAY "==========================================="
               END-IF
           END-PERFORM.

           MOVE depoAmo    to depoDsp.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>Creating a new file to store data if not already exist
       File-Check.

           OPEN INPUT TrxFile
           IF WS-FS  = '35'
               DISPLAY "No file with name Transactions.DAT , creating"
               OPEN OUTPUT TrxFile
               CLOSE TrxFile
           END-IF
           CLOSE TrxFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Write a new record in TrxFile
       WRITE-TRX.

           MOVE userId    TO SenderID
           MOVE userId    TO ReceiverID
           MOVE "Admin Deposit" TO Description
           MOVE depoAmo   TO Amount
           MOVE 3         TO TrxType
           ACCEPT CurrentDate FROM DATE
           ACCEPT CurrentTime FROM TIME
           STRING CurrentDate DELIMITED BY SIZE
                  CurrentTime DELIMITED BY SIZE
                  INTO TimeStamp
           END-STRING
           OPEN I-O TrxFile
           WRITE TransactionRecord
              INVALID KEY
               DISPLAY ESC REDX "Writing transaction failed." ESC RESETX
                   MOVE 97 TO optStatus
                   CLOSE TrxFile
                   GOBACK
           END-WRITE
           DISPLAY "================================================="
           DISPLAY ESC GREENX FUNCTION TRIM(depoDsp)WITH NO ADVANCING
           DISPLAY " successfully deposited" WITH NO ADVANCING
           DISPLAY ESC RESETX "into account ID :"ESC GREENX ReceiverID
           DISPLAY ESC RESETX
           DISPLAY "================================================="
           CLOSE TrxFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Update the balance in UserRecord and increment the TrxCount
       BALANCE-UPDATE.

           ADD depoAmo TO Balance
           REWRITE UserRecord
               INVALID KEY
                   DISPLAY ESC REDX "Updating user balance failed." ESC RESETX
                   MOVE 97 TO optStatus
                   CLOSE UserFile
                   GOBACK
           END-REWRITE
           DISPLAY ESC GREENX" Balance updated for ID :" ReceiverID
           DISPLAY ESC RESETX
           DISPLAY "================================================="
           MOVE 00 TO optStatus
           CLOSE UserFile.
