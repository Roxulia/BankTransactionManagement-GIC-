      ******************************************************************
      * Author: Nyan Ye Thu, Myo Thein Chit
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

       COPY "../../Utility Functions/userFile.cpy".

       FD Transactions.
       01 TRXRECORD.

       COPY "../../Utility Functions/transactionFile.cpy".

       WORKING-STORAGE SECTION.
       01  WS-SenderID     PIC 9(5) VALUE ZERO.
       01  WS-ReceiverAcc  PIC 9(16) VALUE ZERO.
       01  WS-Amount       PIC s9(10)V99 VALUE ZERO.
       01  TEMP-BALANCE    pic s9(10)v99.
       01  EOF-FLAG          PIC X VALUE 'N'.
       01  SENDER-FOUND      PIC X VALUE 'N'.
       01  RECEIVER-FOUND    PIC X VALUE 'N'.
       01  WS-FS1            PIC XX.
       01  WS-FS2            PIC XX.
       01  WS-TRXID          PIC 9(10) VALUE 1.
       01  WS-TrxBaseID     PIC 9(10).
      * 01 WS-TrxDepoPrefix1 PIC 9(10).
       01  WS-TrxFullID     PIC 9(10).
       01  WS-TrxCount      Pic 9(5).
       01  statusCode pic xx.
       01  password pic x(20).
       01  enc_psw pic x(32).

       01  SENDER-RECORD.
           05 U-UID       PIC 9(5).
           05 U-NAME      PIC X(20).
           05 U-LoginName PIC X(25).
           05 U-UAccNo    PIC 9(16).
           05 U-EncPsw    PIC X(32).
           05 U-Unrc      PIC X(30).
           05 U-ADDRESS   PIC X(20).
           05 U-PHONE     PIC x(11).
           05 U-BALANCE   PIC s9(10)V99.
           05 U-TrxCount  PIC 9(5).
           05 U-DATE      PIC 9(8).
           05 U-TIME      PIC 9(6).

       01  RECEIVER-RECORD.
           05 R-UID        PIC 9(5).
           05 R-NAME       PIC X(20).
           05 R-LoginName PIC X(25).
           05 R-UAccNo     PIC 9(16).
           05 R-EncPsw     PIC X(32).
           05 R-UNRC       PIC X(30).
           05 R-ADDRESS    PIC X(20).
           05 R-PHONE      PIC x(11).
           05 R-BALANCE    PIC s9(10)V99.
           05 R-TrxCount   PIC 9(5).
           05 R-DATE       PIC 9(8).
           05 R-TIME       PIC 9(6).

       COPY "../../Utility Functions/trxConstants.cpy".

       COPY "../../Utility Functions/colorCodes.cpy".

       LINKAGE SECTION.

       01  LS-SenderID    PIC 9(5).
       01  LS-StatusCode   PIC X(2) VALUE SPACES.

       PROCEDURE DIVISION USING LS-SenderID LS-StatusCode.

       MAIN-PROCEDURE.
            initialize WS-Amount
            INITIALIZE TEMP-BALANCE
            initialize WS-ReceiverAcc
            INITIALIZE WS-SenderID
            MOVE LS-SenderID TO WS-SenderID
            PERFORM FIND-SENDER
            *>DISPLAY "Enter SenderID : "
            *>ACCEPT WS-SenderUID


           perform FIND-RECEIVER

           DISPLAY "Enter Transfer Amount:".
           ACCEPT WS-AMOUNT

           perform validate_amount
           perform validate-user
           perform TRXID-GENERATE
           PERFORM write-transaction
           perform BALANCE-UPDATE

           exit program.

       FIND-SENDER.
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE WS-SenderID,SENDER-RECORD,statusCode

           if statusCode  EQUAL "99"
               display esc redx
               display "!!!!!!!!!!!!!!"
               display "! FILE ERROR !"
               display "!!!!!!!!!!!!!!"
               display esc resetx
               exit PROGRAM
           else if statusCode EQUAL "96"
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display "! Sender account not found !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
               exit PROGRAM
           end-if.


       FIND-RECEIVER.
           DISPLAY "Enter Receiver's Account Number :".
           ACCEPT WS-ReceiverAcc
           if U-UAccNo = WS-ReceiverAcc
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display "! Can't transfer to yourself !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
               exit PROGRAM
           end-if
           initialize statusCode

           call '../../Utility Functions/bin/getUserByAccNumber'
           using by REFERENCE WS-ReceiverAcc,RECEIVER-RECORD,statusCode

           if statusCode not EQUAL "00"
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display "!      Account NOT Found     !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
               exit PROGRAM
           end-if
           .

       validate-user.
           DISPLAY "Enter Password : "
           accept password
           call '../../Utility Functions/bin/encryption'
           using by REFERENCE password enc_psw
           if enc_psw not equal U-EncPsw
               display esc redx "INVALID CREDENTIAL" esc resetx
               exit program
           end-if.

       validate_amount.
           if WS-Amount < 0
               display esc redx
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display "!        Invalid Amount      !"
               display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
               display esc resetx
               exit PROGRAM
           END-IF
           compute TEMP-BALANCE = u-Balance - WS-AMOUNT

           if TEMP-BALANCE < minaccountbalance
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "! Minimum balance limit reached !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
               exit PROGRAM
           END-IF.

           *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Generate a unique trxid ( TrxCount+1) + S,R,D,W + SenderID
       *> Sent, received , deposit , withdraw , D here since this sub file is for deposit
       TRXID-GENERATE.

           ADD 1 TO u-TrxCount

           STRING
               U-TrxCount DELIMITED BY SIZE
               WS-TrxSentPrefix DELIMITED BY SIZE
               U-UID DELIMITED BY SIZE
               INTO TrxID
           END-STRING

           DISPLAY "Generated TrxID: " TrxID.

       BALANCE-UPDATE.

           subtract WS-Amount from u-Balance
           add WS-Amount to R-BALANCE
           open I-O USERACCOUNTS
           move SENDER-RECORD to USERDATA

           DISPLAY "================================================="
           REWRITE USERDATA
               INVALID KEY
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "!  Updating user balace failed  !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
                   CLOSE USERACCOUNTS
                   exit PROGRAM
           END-REWRITE
           DISPLAY ESC GREENX
           DISPLAY "************************************************"
           DISPLAY "* Balance updated for Acc : " uaccno " *"
           DISPLAY "************************************************"
           DISPLAY ESC RESETX
           move RECEIVER-RECORD to USERDATA
           close USERACCOUNTS

           open i-o USERACCOUNTS

           DISPLAY "================================================="
           REWRITE USERDATA
               INVALID KEY
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "!  Updating user balace failed  !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
                   CLOSE USERACCOUNTS
                   exit PROGRAM
           END-REWRITE
           DISPLAY ESC GREENX
           DISPLAY "************************************************"
           DISPLAY "* Balance updated for Acc : " uaccno " *"
           DISPLAY "************************************************"
           DISPLAY ESC RESETX
           CLOSE USERACCOUNTS.

       write-transaction.
           MOVE U-UAccNo    TO SenderAcc
           MOVE R-UAccNo    TO ReceiverAcc
           MOVE "Transfer" TO Description
           MOVE WS-AMOUNT   TO Amount
           MOVE 4         TO TrxType
           move FUNCTION CURRENT-DATE(1:14) to TimeStamp
           OPEN i-o Transactions
           WRITE TrxRecord
              INVALID KEY
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "!  Writing transactions failed  !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
                   CLOSE Transactions
                   exit PROGRAM
           END-WRITE
           DISPLAY "================================================="
           DISPLAY ESC GREENX FUNCTION TRIM(WS-AMOUNT) WITH NO ADVANCING
           DISPLAY " successfully transfer to account No :"
               WITH NO ADVANCING
           DISPLAY ESC GREENX R-UAccNo
           DISPLAY ESC RESETX
           CLOSE Transactions.
       END PROGRAM trxTransfer.
