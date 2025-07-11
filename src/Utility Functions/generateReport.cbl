       IDENTIFICATION DIVISION.
       PROGRAM-ID. generateReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TrxFile ASSIGN TO "Transactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
     **     SELECT TRANSACTIONS ASSIGN TO "C:\Users\W24016\Desktop
      *     \BankTransactionManagement-GIC-hsy\src\Utility Functions\Transactions.dat"
       DATA DIVISION.
       FILE SECTION.
       FD TrxFile.
       01 TrxRecord.
           05 TrxID         PIC 9(10).
           05 UID           PIC 9(5).
           05 ReceiverID    PIC 9(5).
           05 Description   PIC X(30).
           05 Amount        PIC 9(10)V99.
           05 T-Type        PIC X(1).
           05 TimeStamp     PIC 9(16).

       WORKING-STORAGE SECTION.
       01 INPUT-UID         PIC 9(5).
       01 END-FILE          PIC X VALUE "N".
       01 BALANCE           PIC 9(12)V99 VALUE 0.
       01 TYPE-NAME         PIC X(10).
       01 DISPLAY-TIME      PIC X(16).
       01 WITHDRAW-AMT      PIC Z(10).99.
       01 DEPOSIT-AMT       PIC Z(10).99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Enter your User ID to generate report: ".
           ACCEPT INPUT-UID.

           OPEN INPUT TrxFile.

           DISPLAY "==============================================="
           DISPLAY "  Date-Time    Description   Withdraw   Deposit"
           DISPLAY "-----------------------------------------------"

           PERFORM UNTIL END-FILE = "Y"
               READ TrxFile
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       IF UID = INPUT-UID
                           MOVE SPACES TO TYPE-NAME
                           MOVE zeroS TO WITHDRAW-AMT
                           MOVE zeroS TO DEPOSIT-AMT

                           MOVE TimeStamp TO DISPLAY-TIME

                           IF T-Type = "1"
                               MOVE Amount TO DEPOSIT-AMT
                               ADD Amount TO BALANCE
                           ELSE IF T-Type = "2" OR T-Type = "4"
                               MOVE Amount TO WITHDRAW-AMT
                               SUBTRACT Amount FROM BALANCE
                           ELSE IF T-Type = "3"
                               MOVE Amount TO DEPOSIT-AMT
                               ADD Amount TO BALANCE
                           END-IF

                           DISPLAY DISPLAY-TIME "  " Description
                                   "  " WITHDRAW-AMT "   " DEPOSIT-AMT
                       END-IF
               END-READ
           END-PERFORM.

           DISPLAY "-----------------------------------------------"
           DISPLAY "Current Balance: " BALANCE
           DISPLAY "==============================================="

           CLOSE TrxFile.
           STOP RUN.
