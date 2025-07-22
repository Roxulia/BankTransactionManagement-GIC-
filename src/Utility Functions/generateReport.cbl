       IDENTIFICATION DIVISION.
       PROGRAM-ID. generateReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TrxFile ASSIGN TO '../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.
     **     SELECT TRANSACTIONS ASSIGN TO "C:\Users\W24016\Desktop
      *     \BankTransactionManagement-GIC-hsy\src\Utility Functions\Transactions.dat"
       DATA DIVISION.
       FILE SECTION.
       FD TrxFile.
       01 TrxRecord.
           copy '../Utility Functions/transactionFile.cpy'.

       WORKING-STORAGE SECTION.
       01 END-FILE          PIC X VALUE "N".
       01 T-BALANCE           PIC s9(12)V99 VALUE 0.
       01 withdraw         pic s9(10)v99.
       01 deposit           pic s9(10)v99.
       01 format-balance pic zzzzzzzzzzz9.99.
       01 TYPE-NAME         PIC X(10).
       01 DISPLAY-TIME      PIC X(16).
       01 WITHDRAW-AMT      PIC Zzzzzzzzz9.99.
       01 DEPOSIT-AMT       PIC Zzzzzzzzz9.99.
       01  anykey pic x.
       01  ws-fs pic xx.
       01  statusCode pic xx.
       01  UserData.
           copy '../Utility Functions/userFile.cpy'.

       copy '../Utility Functions/colorCodes.cpy'.

       LINKAGE SECTION.
       01  Input-uid pic 9(5).

       PROCEDURE DIVISION using INPUT-UID.
       MAIN-PARA.
           move "n" to END-FILE
           initialize balance
           INITIALIZE WITHDRAW
           INITIALIZE deposit
           call '../../Utility Functions/bin/getUserByID'
           using by REFERENCE
           Input-uid , UserData , statusCode

           EVALUATE statusCode
               when EQUAL "96"
                   DISPLAY esc redx
                   DISPLAY "User Not Found"
                   DISPLAY esc resetx
                   exit PROGRAM
               when EQUAL "99"
                   display esc redx
                   DISPLAY "Unknown Error Occurs"
                   display esc resetx
                   exit PROGRAM
           END-EVALUATE

           OPEN INPUT TrxFile.
           display esc greenx
           DISPLAY "==============================================="
                   "=============================================="
           DISPLAY "  Date-Time           Withdraw        Deposit  "
                   "  Description"
           DISPLAY "-----------------------------------------------"
                   "-----------------------------------------------"

           PERFORM UNTIL END-FILE = "Y"
               READ TrxFile into TrxRecord
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       *>display TrxRecord
                       MOVE SPACES TO TYPE-NAME
                       MOVE zeroS TO WITHDRAW-AMT
                       MOVE zeroS TO DEPOSIT-AMT
                       IF SenderAcc = UAccNo

                           *>display TrxRecord
                           MOVE TimeStamp TO DISPLAY-TIME
                           MOVE Amount to WITHDRAW-AMT
                           Add Amount to withdraw
                           DISPLAY DISPLAY-TIME
                                   "  " WITHDRAW-AMT "  " DEPOSIT-AMT
                                   "  " Description
                       else if ReceiverAcc = UAccNo

                           MOVE TimeStamp TO DISPLAY-TIME
                           MOVE Amount to deposit-AMT
                           Add Amount to deposit
                           DISPLAY DISPLAY-TIME
                                   "  " WITHDRAW-AMT "  " DEPOSIT-AMT
                                   "  " Description
                       END-IF
               END-READ
           END-PERFORM.
           compute BALANCE = DEPOSIT - WITHDRAW
           move BALANCE to format-balance
           DISPLAY "-----------------------------------------------"
                   "-----------------------------------------------"
           DISPLAY "Current Balance: " format-balance
           DISPLAY "==============================================="
                   "==============================================="
           display esc resetx
           CLOSE TrxFile

           DISPLAY esc redx
           display "Press any key to exit : "
           accept anykey
           DISPLAY esc resetx

           exit program.
