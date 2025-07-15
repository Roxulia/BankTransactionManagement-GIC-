      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 7/7/2025
      * Purpose: Bank Transaction System
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxTransfer.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USERACCOUNTS
           ASSIGN TO "../../../data/UserAccounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS2.
           SELECT TRANSACTIONS
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
           05 ULoginName   PIC X(25).
           05 UEncPsw      PIC X(32).
           05 UAddress     PIC X(20).
           05 UPh          PIC x(9).
           05 Balance      PIC 9(10)V99.
           05 TrxCount     PIC 9(5).
           05 UDate        PIC 9(6).
           05 UTime        PIC 9(6).

       FD Transactions.
       01 TRXRECORD.
           05 TrxID        PIC x(11).
           05 SenderID     PIC 9(5).
           05 ReceiverID   PIC 9(5).
           05 Description  PIC X(30).
           05 Amount       PIC 9(10)V99.
           05 T-Type       PIC 9.
           05 TimeStamp    PIC 9(12).

       WORKING-STORAGE SECTION.
       01 WS-SenderUID    PIC 9(5) VALUE ZERO.
       01 WS-ReceiverUID  PIC 9(5) VALUE ZERO.
       01 WS-Amount       PIC 9(10)V99 VALUE ZERO.
       01  TEMP-BALANCE   pic s9(10)v99.
       01 EOF-FLAG          PIC X VALUE 'N'.
       01 Current-Date   PIC 9(6) VALUE ZERO.
       01 Current-Time   PIC 9(6) VALUE ZERO.
       01 SENDER-FOUND      PIC X VALUE 'N'.
       01 RECEIVER-FOUND    PIC X VALUE 'N'.
       01 WS-FS1            PIC XX.
       01 WS-FS2            PIC XX.
       01 WS-TRXID          PIC 9(10) VALUE 1.
       01 WS-TrxBaseID     PIC 9(10).
      * 01 WS-TrxDepoPrefix1 PIC 9(10).
       01 WS-TrxFullID     PIC 9(10).
       01 WS-TrxCount      Pic 9(5).
       01  statusCode pic xx.
       01 USER-RECORD.
           05 U-UID       PIC 9(5).
           05 U-NAME      PIC X(20).
           05 U-LoginName PIC X(25).
           05 U-EncPsw    PIC X(32).
           05 U-ADDRESS   PIC X(20).
           05 U-PHONE     PIC x(9).
           05 U-BALANCE   PIC 9(10)V99.
           05 U-TrxCount  PIC 9(5).
           05 U-DATE      PIC 9(6).
           05 U-TIME      PIC 9(6).

       01 RECEIVER-RECORD.
           05 R-UID        PIC 9(5).
           05 R-NAME       PIC X(20).
           05 R-ULoginName PIC X(25).
           05 R-EncPsw     PIC X(32).
           05 R-ADDRESS    PIC X(20).
           05 R-PHONE      PIC x(9).
           05 R-BALANCE    PIC 9(10)V99.
           05 R-TrxCount   PIC 9(5).
           05 R-DATE       PIC 9(6).
           05 R-TIME       PIC 9(6).
       COPY "../../Utility Functions/trxConstants.cpy".
       COPY "../../Utility Functions/colorCodes.cpy".
       LINKAGE SECTION.
       01 LS-SenderID    PIC 9(5).
       01 LS-StatusCode     PIC X(2) VALUE SPACES.
       PROCEDURE DIVISION USING LS-SenderID LS-StatusCode.

       MAIN-PROCEDURE.
            initialize WS-Amount
            initialize WS-ReceiverUID
            INITIALIZE WS-SenderUID
            MOVE LS-SENDERID TO WS-SenderUID
            PERFORM FIND-SENDER
            *>DISPLAY "Enter SenderID : "
            *>ACCEPT WS-SenderUID


           perform FIND-RECEIVER

           DISPLAY "Enter Transfer Amount:".
           ACCEPT WS-AMOUNT

           perform validate_amount
           perform TRXID-GENERATE
           PERFORM write-transaction
           perform BALANCE-UPDATE

           exit program.

       FIND-SENDER.
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-SenderUID,USER-RECORD,statusCode

           if statusCode  EQUAL "99"
               display "FILE ERROR"
               exit PROGRAM
           else if statusCode EQUAL "96"
               DISPLAY "SENDER NOT FOUND"
               exit PROGRAM
           end-if.


       FIND-RECEIVER.
           DISPLAY "Enter Receiver's UID:".
           ACCEPT WS-RECEIVERUID
           if WS-SenderUID = WS-ReceiverUID
               DISPLAY "CAN'T TRANSFER TO URSELF"
               exit PROGRAM
           initialize statusCode
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-ReceiverUID,RECEIVER-RECORD,statusCode
           if statusCode  EQUAL "99"
               display "FILE ERROR"
               exit PROGRAM
           else if statusCode EQUAL "96"
               DISPLAY "RECEIVER NOT FOUND"
               exit PROGRAM
           end-if
           DISPLAY RECEIVER-RECORD.


       validate_amount.
           compute TEMP-BALANCE = u-Balance - WS-AMOUNT
           if TEMP-BALANCE < minaccountbalance
               display "Ur Minimum Account Balance Reached"
               exit PROGRAM
           END-IF.

           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Generate a unique trxid ( TrxCount+1) + S,R,D,W + SenderID
       *> Sent, received , deposit , withdraw , D here since this sub file is for deposit
       TRXID-GENERATE.

           ADD 1 TO u-TrxCount

           STRING
               u-TrxCount DELIMITED BY SIZE
               WS-TrxDepoPrefix DELIMITED BY SIZE
               u-UID DELIMITED BY SIZE
               INTO TrxID
           END-STRING

           DISPLAY "Generated TrxID: " TrxID.

       BALANCE-UPDATE.

           subtract WS-Amount from u-Balance
           add WS-Amount to R-BALANCE
           open I-O USERACCOUNTS
           move USER-RECORD to USERDATA

           DISPLAY "================================================="
           REWRITE USERDATA
               INVALID KEY
                   DISPLAY ESC REDX "Updating user balance failed."
                   DISPLAY ESC RESETX
                   CLOSE USERACCOUNTS
                   exit PROGRAM
           END-REWRITE
           DISPLAY ESC GREENX "Balance updated for ID : " uId
           DISPLAY ESC RESETX
           move RECEIVER-RECORD to USERDATA
           close USERACCOUNTS

           open i-o USERACCOUNTS

           DISPLAY "================================================="
           REWRITE USERDATA
               INVALID KEY
                   DISPLAY ESC REDX "Updating user balance failed."
                   DISPLAY ESC RESETX
                   CLOSE USERACCOUNTS
                   exit PROGRAM
           END-REWRITE
           DISPLAY ESC GREENX "Balance updated for ID : " uId
           DISPLAY ESC RESETX
           CLOSE USERACCOUNTS.

       write-transaction.
           MOVE u-UID    TO SenderID
           MOVE R-UID    TO ReceiverID
           MOVE "Transfer" TO Description
           MOVE WS-AMOUNT   TO Amount
           MOVE 4         TO T-Type
           ACCEPT Current-Date FROM DATE
           ACCEPT Current-Time FROM TIME
           STRING Current-Date DELIMITED BY SIZE
                  Current-Time DELIMITED BY SIZE
                  INTO TimeStamp
           END-STRING
           OPEN extend Transactions
           WRITE TrxRecord
              INVALID KEY
                   DISPLAY ESC REDX "Writing transaction failed."
                   DISPLAY ESC RESETX
                   CLOSE Transactions
                   exit PROGRAM
           END-WRITE
           DISPLAY "================================================="
           DISPLAY ESC GREENX FUNCTION TRIM(WS-AMOUNT) WITH NO ADVANCING
           DISPLAY " successfully transfer to account ID :"
               WITH NO ADVANCING
           DISPLAY ESC GREENX R-UID
           DISPLAY ESC RESETX
           CLOSE Transactions.
       END PROGRAM trxTransfer.
