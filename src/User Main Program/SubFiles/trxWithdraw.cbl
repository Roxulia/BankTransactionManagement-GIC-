      ** *****************************************************************
   ****    * Author: YOUR-NAME
   ****    * Date: April 5, 2025
   ** **   * Purpose: Handle withdrawal transaction for logged-in user
    ***    * Tectonics: GnuCOBOL / OpenCOBOL
     **  *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxWithdraw.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERfile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS1.
           SELECT TrxFile
           ASSIGN TO "../../../data/Transactions.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRXID
               FILE STATUS IS WS-FS1.

       DATA DIVISION.
       FILE SECTION.
       FD  UserFile.
       01  UserRecord.

       COPY "../../Utility Functions/userFile.cpy".

       FD  TrxFile.
       01  TransactionRecord.

       COPY "../../Utility Functions/transactionFile.cpy".


       WORKING-STORAGE SECTION.
       01 WS-UID             PIC 9(5) VALUE ZERO.
       01 WS-AMOUNT          PIC s9(10)V99 VALUE ZERO.
       01 EOF-FLAG           PIC X VALUE 'N'.
       01 USER-FOUND         PIC X VALUE 'N'.
       01 TEMP-BALANCE       PIC s9(10)V99 VALUE ZERO.
       01  password pic x(20).
       01  enc_psw pic x(32).
       01  ws-fs1 pic x(2).
       01  statusCode pic x(2).
       01  C-user.
           05  C-UID        PIC 9(5).
           05  c-UName      PIC X(20).
           05  c-ULoginName PIC X(25).
           05  c-UAccNo     PIC 9(16).
           05  c-UEncPsw    PIC X(32).
           05  c-UAddress   PIC X(20).
           05  c-UNrc       PIC X(30).
           05  c-UPh        PIC X(9).
           05  c-Balance    PIC 9(10)V99.
           05  c-TrxCount   PIC 9(5).
           05  c-UDate      PIC 9(8).
           05  c-UTime      PIC 9(6).

       copy '../../Utility Functions/trxConstants.cpy'.

       copy '../../Utility Functions/colorCodes.cpy'.

       LINKAGE SECTION.
       01 LS-UID PIC 9(5).

       PROCEDURE DIVISION USING LS-UID.
       MAIN-PROCEDURE.
           INITIALIZE WS-UID
           INITIALIZE WS-AMOUNT
           MOVE LS-UID TO WS-UID

           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-UID C-user statusCode

           EVALUATE statusCode
               when equal "99"
                   Display esc redx "Error in opening File" esc resetx
                   exit program
               when equal "96"
                   DISPLAY esc redx "User Not Found" esc resetx
                   exit PROGRAM
               when equal "00"
                   *>DISPLAY c-user
                   perform withdraw_process
                   exit program
           END-EVALUATE.

       withdraw_process.
           DISPLAY "Enter Withdrawal Amount:"
           ACCEPT WS-AMOUNT
           compute TEMP-BALANCE = c-Balance - WS-AMOUNT
           if TEMP-BALANCE < minaccountbalance
               display esc redx"Ur Minimum Account Balance Reached "
               esc resetx
               exit PROGRAM
           else
               if WS-AMOUNT < minwithdraw or WS-AMOUNT > maxwithdraw
                   DISPLAY esc redx"Invalid Withdraw Amount " esc resetx
                   exit PROGRAM
               END-IF
           END-IF
           perform validate-user
           perform TRXID-GENERATE
           perform WRITE-TRANSACTION
           perform BALANCE-UPDATE

           .
       validate-user.
           DISPLAY "Enter Password : "
           accept password
           call '../../Utility Functions/bin/encryption'
           using by REFERENCE password enc_psw
           if enc_psw not equal c-UEncPsw
               display esc redx "INVALID CREDENTIAL" esc resetx
               exit PROGRAM
           end-if.


       TRXID-GENERATE.

           ADD 1 TO C-TrxCount

           STRING
               C-TrxCount DELIMITED BY SIZE
               WS-TrxWDPrefix DELIMITED BY SIZE
               C-UID DELIMITED BY SIZE
               INTO TrxID
           END-STRING
           display esc greenx
           DISPLAY "Generated TrxID: " TrxID
           DISPLAY esc resetx.

       write-transaction.
           MOVE C-UAccNo    TO SenderAcc
           MOVE 0           TO ReceiverAcc
           MOVE "WithDraw"  TO Description
           MOVE WS-AMOUNT   TO Amount
           MOVE 2           TO TrxType
           move FUNCTION CURRENT-DATE(1:14) to TimeStamp
           OPEN I-O TrxFile
           WRITE TransactionRecord
              INVALID KEY
                   DISPLAY ESC REDX "Writing transaction failed."
                   DISPLAY ESC RESETX
                   CLOSE TrxFile
                   exit PROGRAM
           END-WRITE
           DISPLAY "================================================="
           DISPLAY ESC GREENX FUNCTION TRIM(WS-AMOUNT) WITH NO ADVANCING
           DISPLAY " successfully withdrawn from account  :"
               WITH NO ADVANCING
           DISPLAY ESC GREENX C-UAccNo
           DISPLAY ESC RESETX
           CLOSE TrxFile.

       BALANCE-UPDATE.

           move TEMP-BALANCE to c-Balance
           move c-user to UserRecord
           open I-O UserFile
           DISPLAY "================================================="
           REWRITE UserRecord
               INVALID KEY
                   DISPLAY ESC REDX "Updating user balance failed."
                   DISPLAY ESC RESETX
                   CLOSE UserFile
                   exit PROGRAM
           END-REWRITE

           DISPLAY ESC GREENX "Balance updated for Acc : " uaccno
           DISPLAY ESC RESETX
           CLOSE UserFile.
       END PROGRAM trxWithdraw.
