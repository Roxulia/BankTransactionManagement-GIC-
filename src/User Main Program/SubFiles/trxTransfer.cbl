      ******************************************************************
      * Author: Nyan Ye Thu
      * Date: 7/7/2025
      * Purpose: Bank Transaction System
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. txrTransferTest.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT testfile ASSIGN TO 'userfile.txt'
           ORGANIZATION IS SEQUENTIAL.
           SELECT Transactions ASSIGN TO "Transactions.txt"
           ORGANIZATION IS  SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD testfile.
       01 userdata.
           05 UID          PIC 9(5).
           05 UName        PIC X(20).
           05 ULoginName   PIC X(20).
           05 UEncPsw      PIC X(255).
           05 UAddress     PIC X(20).
           05 Phone        PIC 9(9).
           05 Balance      PIC 9(10)V99.
           05 UDate        PIC 9(8).
           05 UTime        PIC 9(6).
       FD Transactions.
       01 TrxRecord.
           05 TrxID        PIC 9(10).
           05 TrxUID          PIC 9(5).
           05 ReceiverID   PIC 9(5).
           05 Description  PIC X(30).
           05 Amount       PIC 9(10)V99.
           05 T-Type       PIC X(20).
           05 TimeStamp    PIC 9(16).
       WORKING-STORAGE SECTION.
       01 SenderUID        PIC 9(5) VALUE ZERO.
       01 ReceiverUID      PIC 9(5).
       01 TransferAmount   PIC 9(10)V99.
       01 FoundSender      PIC X VALUE 'N'.
       01 FoundReceiver    PIC X VALUE 'N'.
       01 EOF-FLAG         PIC X VALUE 'N'.
       01 DateTime         PIC X(16) VALUES SPACES.
       01 TrxTID           PIC 9(10) VALUE 1000000000.
       01 EOF            PIC X VALUE "N".
       01 SenderBalance    PIC 9(10)V99.
       01 ReceiverBalance  PIC 9(10)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter Sender's UID:"
            ACCEPT SenderUID.

            DISPLAY "Enter Receiver's UID:"
            ACCEPT ReceiverID.

            DISPLAY "Enter Transfer Amount:"
            ACCEPT TransferAmount.

            OPEN I-O testfile.

            *>Find Sender

           PERFORM UNTIL EOF = "Y"
            READ testfile
            AT END
            EXIT PROGRAM
            END-READ

             IF UID = SenderUID THEN
                 MOVE Balance TO SenderBalance
                 IF SenderBalance >= Amount THEN
                     COMPUTE SenderBalance = SenderBalance - Amount
                     MOVE SenderBalance TO Balance
                     MOVE FUNCTION CURRENT-DATE TO UDate
                     REWRITE userdata
                     MOVE "Y" TO FoundSender
                END-IF
             END-IF
            END-PERFORM.

            IF FoundSender NOT ="Y"
                DISPLAY "Sender not Found or Insufficient Funds."
                CLOSE testfile
            END-IF.

            REWRITE-SENDER-FINISHED.
                CLOSE testfile.
                OPEN I-O testfile.

                PERFORM UNTIL EOF = "Y"
                READ testfile
                AT END
                EXIT PROGRAM
                END-READ


               IF UID = ReceiverUID THEN
                   MOVE Balance TO ReceiverBalance
                   COMPUTE ReceiverBalance = ReceiverBalance+Amount
                   MOVE ReceiverBalance TO Balance
                   MOVE FUNCTION CURRENT-DATE TO UDate
                   REWRITE userdata
                   MOVE "Y" TO FoundReceiver
               END-IF
           END-PERFORM.

               IF FoundReceiver NOT = "Y"
               DISPLAY "Receiver not found."
               CLOSE testfile
               STOP RUN
           END-IF.
           Close testfile


           OPEN OUTPUT Transactions

           *> Sender Log
           ADD 1 TO TrxTID
           MOVE TrxTID TO TrxID
           MOVE SenderUID TO TrxUID
           MOVE ReceiverUID TO ReceiverID
           MOVE "Sent to UID " TO Description
           MOVE TransferAmount TO Amount
           MOVE "2" TO T-Type
           MOVE FUNCTION CURRENT-DATE TO TimeStamp
           WRITE TrxRecord.



           *> Receiver Log
            ADD 1 TO TrxTID
           MOVE TrxTID TO TrxID
           MOVE ReceiverUID TO TrxUID
           MOVE SenderUID TO ReceiverID
           MOVE "Received from UID " TO Description
           MOVE TransferAmount TO Amount
           MOVE "1" TO T-Type
           MOVE FUNCTION CURRENT-DATE TO TimeStamp
           WRITE TrxRecord.
           CLOSE Transactions

           DISPLAY "Transfer successful.".

       END PROGRAM txrTransferTest.
