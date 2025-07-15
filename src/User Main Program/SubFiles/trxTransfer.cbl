      ******************************************************************
      * Author: Nyan Ye Thu ,Myo Thein Chit
      * Date: 7/7/2025 , 15/7/2025
      * Purpose: Bank Transaction System
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. txrTransfer.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT USERACCOUNTS
           ASSIGN TO "../../../data/UserAccounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS2.

           SELECT TrxFile
           ASSIGN TO "../../../data/Transactions.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRXID
               FILE STATUS IS WS-FS1.

       DATA DIVISION.
       FILE SECTION.
       FD USERACCOUNTS.
       01 USERDATA.
           05 UID          PIC 9(5).
           05 UName        PIC X(20).
           05 UEncPsw      PIC X(32).
           05 UAddress     PIC X(20).
           05 UPhone       PIC 9(9).
           05 Balance      PIC 9(10)V99.
           05 TrxCount     PIC 9(5).
           05 UDate        PIC 9(8).
           05 UTime        PIC 9(6).

       FD  TrxFile.
       01  TransactionRecord.
           05  TrxID       PIC X(11).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10)v99.
           05  TrxType     PIC 9.  *>  1 = SENT
                                   *>  2 = RECEIVED
                                   *>  3 = DEPOSIT
                                   *>  4 = WITHDRAW
           05  TimeStamp   PIC 9(12).

       WORKING-STORAGE SECTION.

       01  WS-SenderUID        PIC 9(5) VALUE ZERO.
       01  WS-ReceiverUID      PIC 9(5) VALUE ZERO.
       01  WS-Amount           PIC 9(10)V99 VALUE ZERO.
       01  EOF-FLAG            PIC X VALUE 'N'.
       01  Current-Date        PIC 9(8) VALUE ZERO.
       01  Current-Time        PIC 9(6) VALUE ZERO.
       01  SENDER-FOUND        PIC X VALUE 'N'.
       01  RECEIVER-FOUND      PIC X VALUE 'N'.
       01  TEMP-BALANCE        PIC 9(10)V99 VALUE ZERO.
       01  WS-FS1              PIC XX.
       01  WS-FS2              PIC XX.
       01  WS-TRXID            PIC 9(10) VALUE 1.

       *>For trxConstant VALUES
       COPY "../../Utility Functions/trxConstants.cpy".

       *>For Display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       *>LINKAGE SECTION.
       01 LS-SenderID          PIC 9(5).
       01 LS-StatusCode        PIC X(2) VALUE SPACES.

       PROCEDURE DIVISION.*>USING LS-SenderID LS-StatusCode.
       MAIN-PROCEDURE.

           MOVE LS-SENDERID TO WS-SenderUID

           DISPLAY "================================================="
           DISPLAY "Enter Sender's UID:".
           ACCEPT WS-SenderUID.

           DISPLAY "================================================="
           DISPLAY "Enter Receiver's UID:".
           ACCEPT WS-ReceiverUID.

           DISPLAY "================================================="
           DISPLAY "Enter Transfer Amount:".
           ACCEPT WS-Amount.

           OPEN I-O USERACCOUNTS

           IF WS-FS2 NOT = "00"
               DISPLAY "ERROR: Unable to OPEN , STATUS=" WS-FS2
               *> handle error: e.g., STOP RUN or GO TO error-handling paragraph
               STOP RUN
           END-IF

           PERFORM Find-Accounts

               IF SENDER-FOUND = 'Y' AND RECEIVER-FOUND = 'Y'
                   AND Balance >= WS-AMOUNT
                   THEN
                       PERFORM PROCESS-AMOUNTS
                       PERFORM LOG-TRANSACTIONS
           DISPLAY "================================================="
                       DISPLAY "Transfer Successful."
                       DISPLAY "Amount : " ESC GREENX WS-Amount
                       DISPLAY ESC RESETX
                       DISPLAY "To Account :"ESC GREENX WS-ReceiverUID
                       DISPLAY ESC RESETX
           DISPLAY "================================================="
               ELSE
                   DISPLAY ESC REDX
            "Transfer Failed: Insufficient balance or invalid user ID."
                   DISPLAY ESC RESETX
                   MOVE "99" TO LS-StatusCode
               END-IF

           CLOSE UserAccounts.
           GOBACK.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*

       Find-Accounts.

           MOVE WS-SenderUID TO UID
           READ USERACCOUNTS INVALID KEY
               DISPLAY "Sender not found."
               MOVE 'N' TO SENDER-FOUND
               GOBACK
           END-READ
           *>MOVE USERDATA TO Sender-Data
           MOVE 'Y' TO SENDER-FOUND.

           MOVE WS-RECEIVERUID TO UID
           READ USERACCOUNTS INVALID KEY
               DISPLAY "Receiver not found."
               MOVE 'N' TO RECEIVER-FOUND
               GOBACK
           END-READ
           *>MOVE USERDATA TO Receiver-Data
           MOVE 'Y' TO RECEIVER-FOUND
           .
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       PROCESS-AMOUNTS.

           MOVE WS-SenderUID TO UID
           READ USERACCOUNTS KEY IS UID

           ADD 1 to TrxCount

           STRING
               FUNCTION NUMVAL(WS-SenderUID) DELIMITED BY SIZE
               WS-TrxSentPrefix DELIMITED BY SIZE
               FUNCTION NUMVAL(WS-ReceiverUID) DELIMITED BY SIZE
               INTO TrxID.

           DISPLAY "Sender Balance : " Balance
           SUBTRACT WS-Amount FROM Balance
           DISPLAY "After Balance  : " Balance

           REWRITE USERDATA

           MOVE WS-ReceiverUID TO UID
           READ USERACCOUNTS KEY IS UID

           ADD WS-Amount TO Balance

           REWRITE USERDATA

           MOVE "00" TO LS-StatusCode
           .
       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
       LOG-TRANSACTIONS.
           OPEN EXTEND TrxFile

           MOVE FUNCTION CURRENT-DATE (1:8) TO CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO CURRENT-TIME

           *> Log sender transaction
           MOVE WS-SenderUID       TO SenderID
           MOVE WS-ReceiverUID     TO ReceiverID
           MOVE WS-SentDescript    TO Description
           MOVE WS-AMOUNT TO AMOUNT
           MOVE 1 TO TrxType
           STRING CURRENT-DATE DELIMITED BY SIZE
                  CURRENT-TIME DELIMITED BY SIZE
                  INTO TimeStamp
           WRITE TransactionRecord

           CLOSE TrxFile
           .
       END PROGRAM txrTransfer.
