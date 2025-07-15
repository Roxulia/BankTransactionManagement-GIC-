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
           05  TrxID       PIC X(11).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10)v99.
           05  TrxType     PIC 9.
           05  TimeStamp   PIC 9(12).

       WORKING-STORAGE SECTION.
       01 END-FILE          PIC X VALUE "N".
       01 BALANCE           PIC 9(12)V99 VALUE 0.
       01 format-balance pic zzzzzzzzzzz9.99.
       01 TYPE-NAME         PIC X(10).
       01 DISPLAY-TIME      PIC X(16).
       01 WITHDRAW-AMT      PIC Zzzzzzzzz9.99.
       01 DEPOSIT-AMT       PIC Zzzzzzzzz9.99.
       01  anykey pic x.
       01  ws-fs pic xx.
       01  statusCode pic xx.
       01  UserData.
           05  UID        PIC 9(5).
           05  UName      PIC X(20).
           05  ULoginName PIC X(25).
           05  UEncPsw    PIC X(32).
           05  UAddress   PIC X(20).
           05  UPh        PIC X(9).
           05  UBalance    PIC 9(10)V99.
           05  trxCount pic 9(5).
           05  UDate      PIC 9(6).
           05  UTime      PIC 9(6).
           
       copy '../Utility Functions/colorCodes.cpy'.

       LINKAGE SECTION.
       01  Input-uid pic 9(5).

       PROCEDURE DIVISION using INPUT-UID.
       MAIN-PARA.
           move "n" to END-FILE
           initialize balance
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
               READ TrxFile
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       IF SenderID = INPUT-UID or ReceiverID = Input-uid
                           MOVE SPACES TO TYPE-NAME
                           MOVE zeroS TO WITHDRAW-AMT
                           MOVE zeroS TO DEPOSIT-AMT

                           MOVE TimeStamp TO DISPLAY-TIME

                           IF TrxType = "1"
                               MOVE Amount TO DEPOSIT-AMT
                               ADD Amount TO BALANCE
                           ELSE IF TrxType = "2" OR TrxType = "4"
                               MOVE Amount TO WITHDRAW-AMT
                               SUBTRACT Amount FROM BALANCE
                           ELSE IF TrxType = "3"
                               MOVE Amount TO DEPOSIT-AMT
                               ADD Amount TO BALANCE
                           END-IF

                           DISPLAY DISPLAY-TIME 
                                   "  " WITHDRAW-AMT "  " DEPOSIT-AMT
                                   "  " Description
                       END-IF
               END-READ
           END-PERFORM.
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
