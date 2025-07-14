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
           05 TrxID        PIC 9(10).
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
       01 EOF-FLAG          PIC X VALUE 'N'.
       01 Current-Date   PIC 9(6) VALUE ZERO.
       01 Current-Time   PIC 9(6) VALUE ZERO.
       01 SENDER-FOUND      PIC X VALUE 'N'.
       01 RECEIVER-FOUND    PIC X VALUE 'N'.
       01 TEMP-BALANCE      PIC 9(10)V99 VALUE ZERO.
       01 WS-FS1            PIC XX.
       01 WS-FS2            PIC XX.
       01 WS-TRXID          PIC 9(10) VALUE 1.
       01 WS-TrxBaseID     PIC 9(10).
       01 WS-TrxDepoPrefix PIC 9(10).
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
       LINKAGE SECTION.
       01 LS-SenderID    PIC 9(5).
       01 LS-StatusCode     PIC X(2) VALUE SPACES.
       PROCEDURE DIVISION USING LS-SenderID LS-StatusCode.
           
       MAIN-PROCEDURE.
            *>MOVE LS-SENDERID TO WS-SenderUID
            DISPLAY "Enter SenderID : "
            ACCEPT WS-SenderUID
            DISPLAY "Enter Receiver's UID:".
           ACCEPT WS-RECEIVERUID.

           DISPLAY "Enter Transfer Amount:".
           ACCEPT WS-AMOUNT.

           
           PERFORM FIND-SENDER
           PERFORM FIND-RECEIVER
           IF SENDER-FOUND = 'Y' AND RECEIVER-FOUND = 'Y'
               AND U-Balance >= WS-AMOUNT
               THEN
                   PERFORM PROCESS-TRANSFER
                   DISPLAY "Transfer Successful."
               ELSE
                   DISPLAY 
            "Transfer Failed: Insufficient balance or invalid user ID."
                   MOVE "99" TO LS-StatusCode
           END-IF      
           
            GOBACK.
             
           FIND-SENDER.
           MOVE WS-SENDERUID TO UID
           call '../../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-SenderUID,USER-RECORD,statusCode
           
           if statusCode  EQUAL "00"
               DISPLAY "Fecth Sender"
               DISPLAY USER-RECORD
               move U-TrxCount to WS-TrxCount
               MOVE 'Y' TO SENDER-FOUND
           else
               DISPLAY statusCode
           end-if
           
           .
           FIND-RECEIVER.
           MOVE WS-RECEIVERUID TO UID
           MOVE WS-SENDERUID TO UID
           call '../../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-SenderUID,RECEIVER-RECORD,statusCode
           
           if statusCode  EQUAL "00"
               DISPLAY "Fetch Receiver"
              DISPLAY RECEIVER-RECORD
           MOVE 'Y' TO RECEIVER-FOUND
           ELSE
               DISPLAY statusCode
           end-if
           
           .
           PERFORM TRXID-GENERATE.
           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Generate a unique trxid ( TrxCount+1) + S,R,D,W + SenderID
       *> Sent, received , deposit , withdraw , D here since this sub file is for deposit
       TRXID-GENERATE.
 
           ADD 1 TO ws-TrxCount
           MOVE ws-TrxCount TO TrxID.
 
           STRING
               FUNCTION NUMVAL(WS-TrxBaseID) DELIMITED BY SIZE
               WS-TrxDepoPrefix DELIMITED BY SIZE
               FUNCTION NUMVAL(WS-SenderUID) DELIMITED BY SIZE
               INTO WS-TrxFullID.

            PROCESS-TRANSFER.
            open I-O USERACCOUNTS
           SUBTRACT WS-AMOUNT FROM U-Balance GIVING TEMP-BALANCE
           MOVE TEMP-BALANCE TO U-Balance
           move USER-RECORD to USERDATA
           REWRITE USERDATA
           

           ADD WS-AMOUNT TO R-Balance GIVING TEMP-BALANCE
           MOVE TEMP-BALANCE TO R-Balance
           move RECEIVER-RECORD to USERDATA
           REWRITE USERDATA
           close USERACCOUNTS
           
           PERFORM LOG-TRANSACTIONS
           MOVE "00" TO LS-StatusCode
           .

           LOG-TRANSACTIONS.
           OPEN EXTEND TRANSACTIONS

           MOVE FUNCTION CURRENT-DATE (1:8) TO CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO CURRENT-TIME

           *> Log sender transaction
           MOVE WS-SENDERUID TO SENDERID OF TRXRECORD
           MOVE WS-RECEIVERUID TO RECEIVERID OF TRXRECORD
           MOVE "FUND TRANSFER SENT" TO DESCRIPTION OF TRXRECORD
           MOVE WS-AMOUNT TO AMOUNT OF TRXRECORD
           MOVE "SEND" TO T-TYPE OF TRXRECORD
           STRING CURRENT-DATE DELIMITED BY SIZE
                  CURRENT-TIME DELIMITED BY SIZE
                  INTO TIMESTAMP OF TRXRECORD
           WRITE TRXRECORD

           *> Log receiver transaction
           MOVE WS-RECEIVERUID TO SENDERID OF TRXRECORD
           MOVE WS-SENDERUID TO RECEIVERID OF TRXRECORD
           MOVE "FUND TRANSFER RECEIVED" TO DESCRIPTION OF TRXRECORD
           MOVE WS-AMOUNT TO AMOUNT OF TRXRECORD
           MOVE "RECEIVE" TO T-TYPE OF TRXRECORD
           STRING CURRENT-DATE DELIMITED BY SIZE
                  CURRENT-TIME DELIMITED BY SIZE
                  INTO TIMESTAMP OF TRXRECORD
           WRITE TRXRECORD

           CLOSE TRANSACTIONS
           .
       END PROGRAM trxTransfer.
