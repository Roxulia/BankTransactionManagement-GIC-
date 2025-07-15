      ** *****************************************************************
   ****    * Author: YOUR-NAME
   ****    * Date: April 5, 2025
   ** **   * Purpose: Handle withdrawal transaction for logged-in user
    ***   * Tectonics: GnuCOBOL / OpenCOBOL
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
           05  TrxID       PIC X(11).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10)v99.
           05  TrxType     PIC 9.
           05  TimeStamp   PIC 9(12).


       WORKING-STORAGE SECTION.
       01 WS-UID             PIC 9(5) VALUE ZERO.
       01 WS-AMOUNT          PIC 9(10)V99 VALUE ZERO.
       01 EOF-FLAG           PIC X VALUE 'N'.
       01 CURRENT-DATE       PIC 9(6) VALUE ZERO.
       01 CURRENT-TIME       PIC 9(6) VALUE ZERO.
       01 USER-FOUND         PIC X VALUE 'N'.
       01 TEMP-BALANCE       PIC s9(10)V99 VALUE ZERO.
       01  ws-fs1 pic x(2).
       01  statusCode pic x(2).
       01  C-user.
           05  C-UID        PIC 9(5).
           05  c-UName      PIC X(20).
           05  c-ULoginName PIC X(25).
           05  c-UEncPsw    PIC X(32).
           05  c-UAddress   PIC X(20).
           05  c-UPh        PIC X(9).
           05  c-Balance    PIC 9(10)V99.
           05  c-TrxCount   PIC 9(5).
           05  c-UDate      PIC 9(6).
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
                   Display "Error in opening File"
                   exit program
               when equal "96"
                   DISPLAY "User Not Found"
                   exit PROGRAM
               when equal "00"
                   DISPLAY c-user
                   perform withdraw_process
                   exit program
           END-EVALUATE.

       withdraw_process.
           DISPLAY "Enter Withdrawal Amount:"
           ACCEPT WS-AMOUNT
           compute TEMP-BALANCE = c-Balance - WS-AMOUNT
           if TEMP-BALANCE < minaccountbalance
               display "Ur Minimum Account Balance Reached"
               exit PROGRAM
           else
               if WS-AMOUNT < minwithdraw or WS-AMOUNT > maxwithdraw
                   DISPLAY "Invalid Withdraw Amount"
                   exit PROGRAM
               END-IF
           END-IF
           perform TRXID-GENERATE
           perform WRITE-TRANSACTION
           perform BALANCE-UPDATE

           .



       TRXID-GENERATE.

           ADD 1 TO C-TrxCount

           STRING
               C-TrxCount DELIMITED BY SIZE
               WS-TrxDepoPrefix DELIMITED BY SIZE
               C-UID DELIMITED BY SIZE
               INTO TrxID
           END-STRING

           DISPLAY "Generated TrxID: " TrxID.

       write-transaction.
           MOVE C-UID    TO SenderID
           MOVE 0    TO ReceiverID
           MOVE "WithDraw" TO Description
           MOVE WS-AMOUNT   TO Amount
           MOVE 2         TO TrxType
           ACCEPT Current-Date FROM DATE
           ACCEPT Current-Time FROM TIME
           STRING Current-Date DELIMITED BY SIZE
                  Current-Time DELIMITED BY SIZE
                  INTO TimeStamp
           END-STRING
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
           DISPLAY " successfully withdrawn from account ID :"
               WITH NO ADVANCING
           DISPLAY ESC GREENX C-UID
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

           DISPLAY ESC GREENX "Balance updated for ID : " uId
           DISPLAY ESC RESETX
           CLOSE UserFile.
       END PROGRAM trxWithdraw.
