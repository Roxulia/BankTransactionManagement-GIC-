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
           SELECT USERACCOUNTS ASSIGN TO "UserAccounts.dat"
               ORGANIZATION IS SEQUENTIAL.
           SELECT TRANSACTIONS ASSIGN TO "Transactions.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD USERACCOUNTS.
       01 USERDATA.
           05 UID          PIC 9(5).
           05 UName        PIC X(20).
           05 UEncPsw      PIC X(255).
           05 UAddress      PIC X(20).
           05 Phone        PIC 9(9).
           05 Balance      PIC 9(10)V99.
           05 UDate        PIC 9(8).
           05 UTime        PIC 9(6).

       FD TRANSACTIONS.
       01 TRXRECORD.
           05 TRXID        PIC 9(10).
           05 UID          PIC 9(5).
           05 RECEIVERID   PIC 9(5).
           05 DESCRIPTION  PIC X(30).
           05 AMOUNT       PIC 9(10)V99.
           05 T-TYPE       PIC X(20).
           05 TIMESTAMP    PIC 9(16).

          FD  USERACCOUNTS
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 360 CHARACTERS.
       01 USER-RECORD.
           05 U-UID       PIC 9(5).
           05 U-NAME     PIC X(20).
           05 U-LOGIN    PIC X(25).
           05 U-PASSWORD PIC X(255).
           05 U-ADDRESS  PIC X(20).
           05 U-PHONE    PIC 9(9).
           05 U-BALANCE  PIC 9(10)V99.
           05 U-DATE     PIC 9(8).
           05 U-TIME     PIC 9(6).

       WORKING-STORAGE SECTION.
       01 WS-UID             PIC 9(5) VALUE ZERO.
       01 WS-AMOUNT          PIC 9(10)V99 VALUE ZERO.
       01 EOF-FLAG           PIC X VALUE 'N'.
       01 CURRENT-DATE       PIC 9(8) VALUE ZERO.
       01 CURRENT-TIME       PIC 9(6) VALUE ZERO.
       01 USER-FOUND         PIC X VALUE 'N'.
       01 TEMP-BALANCE       PIC 9(10)V99 VALUE ZERO.


       LINKAGE SECTION.
       01 LS-UID PIC 9(5).

       PROCEDURE DIVISION USING LS-UID.
       MAIN-PROCEDURE.
           MOVE LS-UID TO WS-UID

           DISPLAY "Enter Withdrawal Amount:"
           ACCEPT WS-AMOUNT

           OPEN I-O USERACCOUNTS
           PERFORM FIND-USER

           IF USER-FOUND = 'Y'
               AND U-Balance >= WS-AMOUNT
               THEN
                   SUBTRACT WS-AMOUNT FROM U-Balance GIVING TEMP-BALANCE
                   MOVE TEMP-BALANCE TO U-Balance
                   PERFORM WRITE-TRANSACTION
                   REWRITE USER-RECORD
                   DISPLAY "Withdrawal Successful."
               ELSE
                   DISPLAY "Insufficient balance or user not found."
           END-IF

           CLOSE USERACCOUNTS
           GOBACK .

       FIND-USER.
           MOVE 'N' TO USER-FOUND
           MOVE 'N' TO EOF-FLAG
           CLOSE USERACCOUNTS
           OPEN INPUT USERACCOUNTS

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ USERACCOUNTS INTO USER-RECORD
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF U-UID = WS-UID
                           MOVE 'Y' TO USER-FOUND
                           MOVE 'Y' TO EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM
           .

       WRITE-TRANSACTION.
           OPEN EXTEND TRANSACTIONS

           MOVE FUNCTION CURRENT-DATE (1:8) TO CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE (9:6) TO CURRENT-TIME

           MOVE WS-UID TO UID OF TRXRECORD
           MOVE 0 TO RECEIVERID OF TRXRECORD
           MOVE "WITHDRAW" TO DESCRIPTION OF TRXRECORD
           MOVE WS-AMOUNT TO AMOUNT OF TRXRECORD
           MOVE "WITHDRAW" TO T-TYPE OF TRXRECORD
           STRING CURRENT-DATE DELIMITED BY SIZE
                  CURRENT-TIME DELIMITED BY SIZE
                  INTO TIMESTAMP OF TRXRECORD

           WRITE TRXRECORD
           CLOSE TRANSACTIONS.
       END PROGRAM trxWithdraw.
