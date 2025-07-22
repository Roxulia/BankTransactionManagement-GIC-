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
           SELECT UserFile ASSIGN TO '../../../data/UserAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UAccNo
               FILE STATUS IS WS-FS.

           SELECT TrxFile ASSIGN TO '../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  UserFile.
       01  UserRecord.

       COPY "../../Utility Functions/userFile.cpy".

       FD  TrxFile.
       01  TransactionRecord.
           05  TrxID       PIC X(11).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10)v99.
           05  TrxType     PIC 9.
           05  TimeStamp   PIC 9(14).

       WORKING-STORAGE SECTION.

       COPY "../../Utility Functions/colorCodes.cpy".

       01  WS-FS               PIC XX.
       01  depoAmo             PIC 9(10).
       01  depoStr             PIC X(10).

       01  CurrentDate         PIC 9(6).
       01  CurrentTime         PIC 9(6).

       01  minDspDepo          PIC Z(10).
       01  maxDspDepo          PIC Z(10).
       01  depoDsp             PIC Z(10).

       01  tempInput           PIC X(10).


       COPY "../../Utility Functions/trxConstants.cpy".

       LINKAGE section.
       01  AccNo              PIC 9(16).
       01  optStatus           PIC 9(2).

       PROCEDURE DIVISION using REFERENCE AccNo,optStatus.
       Main-Section.
           INITIALIZE depoAmo
           INITIALIZE depoStr
           *>PERFORM TEST-HELPER
           PERFORM RECORD-POINTER
           PERFORM TRXID-GENERATE
           PERFORM AMOUNT-VALID-PROMPT-BOX
           PERFORM File-Check
           PERFORM WRITE-TRX
           PERFORM BALANCE-UPDATE
           exit program.

       *>>>>> Helper function to test the function standalone <<<<<<<<<*
       *>>>>> Not needed in main runtime <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       TEST-HELPER.

           DISPLAY "================================================="
           DISPLAY "ENTER Account Number TO MAKE DEPOSIT :"
           ACCEPT tempInput
           IF FUNCTION UPPER-CASE(tempInput) = "EXIT"
               MOVE 99 TO optStatus
               GOBACK
           END-IF
           MOVE FUNCTION NUMVAL(tempInput) TO AccNo.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>>>>> Finding the desired user record in the UserFile <<<<<<<<<*
       RECORD-POINTER.

           OPEN I-O UserFile
           MOVE AccNo TO UAccNo
           READ UserFile KEY IS UAccNo
               INVALID KEY
                   DISPLAY ESC REDX "[ERROR] User not found." ESC RESETX
                   MOVE 44 TO optStatus
                   CLOSE UserFile
                   GOBACK
           END-READ.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>>>>> TrxID generator to get a unique ID every transaction <<<<*
       TRXID-GENERATE.

           ADD 1 TO TrxCount

           STRING
               TrxCount DELIMITED BY SIZE
               WS-TrxDepoPrefix DELIMITED BY SIZE
               AccNo DELIMITED BY SIZE
               INTO TrxID
           END-STRING

           DISPLAY "Generated TrxID: " TrxID.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>>>>>>>>>>>> To show prompt and ask input from user<<<<<<<<<<<<*
       AMOUNT-VALID-PROMPT-BOX.

           MOVE minAmoDepo to minDspDepo
           MOVE maxAmoDepo to maxDspDepo

           DISPLAY "================================================="
           PERFORM UNTIL depoAmo >= minAmoDepo AND depoAmo <= maxAmoDepo
               DISPLAY "Enter deposit amount " WITH NO ADVANCING
               DISPLAY "(Min: " FUNCTION TRIM(minDspDepo)
                   WITH NO ADVANCING
               DISPLAY " Max: " FUNCTION TRIM(maxDspDepo) "):"
               DISPLAY " or type 'exit' to go back to mainmenu..."
               ACCEPT depoStr
               IF FUNCTION UPPER-CASE(depoStr) = "EXIT"
                   MOVE 99 TO optStatus
                   CLOSE UserFile
                   GOBACK
               END-IF
               MOVE FUNCTION NUMVAL(depoStr) TO depoAmo
               IF depoAmo < minAmoDepo OR depoAmo > maxAmoDepo
                   DISPLAY ESC REDX "Amount out of allowed range."
                   DISPLAY ESC RESETX
               END-IF
           END-PERFORM
           MOVE depoAmo TO depoDsp.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>> checking the file availability and creating one if not exist<<<*
       File-Check.

           OPEN INPUT TrxFile
           IF WS-FS  = '35'
               DISPLAY "No file with name Transactions.DAT , creating"
               OPEN OUTPUT TrxFile
               CLOSE TrxFile
           END-IF
           CLOSE TrxFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>>>>>>>>>>>> writing a new transaction record in the TrxFile <<<*
       WRITE-TRX.

           MOVE 0    TO SenderID
           MOVE AccNo    TO ReceiverID
           MOVE "Admin Deposit" TO Description
           MOVE depoAmo   TO Amount
           MOVE 3         TO TrxType
           move FUNCTION CURRENT-DATE(1:14) to TimeStamp
           OPEN I-O TrxFile
           WRITE TransactionRecord
              INVALID KEY
                   DISPLAY ESC REDX "Writing transaction failed."
                   DISPLAY ESC RESETX
                   MOVE 97 TO optStatus
                   CLOSE TrxFile
                   GOBACK
           END-WRITE
           DISPLAY "================================================="
           DISPLAY ESC GREENX FUNCTION TRIM(depoDsp) WITH NO ADVANCING
           DISPLAY " successfully deposited into account ID :"
               WITH NO ADVANCING
           DISPLAY ESC GREENX AccNo
           DISPLAY ESC RESETX
           CLOSE TrxFile.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       *>>>>>>> Updating balance in the user file <<<<<<<<<<<<<<<<<<<<<*
       BALANCE-UPDATE.

           ADD depoAmo TO Balance
           DISPLAY "================================================="
           REWRITE UserRecord
               INVALID KEY
                   DISPLAY ESC REDX "Updating user balance failed."
                   DISPLAY ESC RESETX
                   MOVE 97 TO optStatus
                   CLOSE UserFile
                   GOBACK
           END-REWRITE

           DISPLAY ESC GREENX "Balance updated for Account : "AccNo
           DISPLAY ESC RESETX
           MOVE 00 TO optStatus
           CLOSE UserFile.
