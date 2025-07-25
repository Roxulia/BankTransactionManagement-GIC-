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

           SELECT SeqTrx    ASSIGN TO "TmpSeqTrx.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE    IS SEQUENTIAL
               FILE STATUS    IS WS-FS.

           SELECT TrxChrono ASSIGN TO '../../../data/TrxChrono.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

           SELECT WORK ASSIGN TO 'WRK.DAT'.

     **     SELECT TRANSACTIONS ASSIGN TO "C:\Users\W24016\Desktop
      *     \BankTransactionManagement-GIC-hsy\src\Utility Functions\Transactions.dat"
       DATA DIVISION.
       FILE SECTION.
       FD  TrxFile.
       01  TrxRecord.
           copy '../Utility Functions/transactionFile.cpy'.

       FD  TrxChrono.
       01  Chr-TrxRecord.
           05  Chr-TrxID        PIC X(11).
           05  Chr-SenderAcc    PIC 9(16).
           05  Chr-ReceiverAcc  PIC 9(16).
           05  Chr-Description  PIC X(30).
           05  Chr-Amount       PIC s9(10)v99.
           05  Chr-TrxType      PIC 9.
           05  Chr-TimeStamp    PIC 9(14).

       FD  SeqTrx.
       01  S-TrxRecord.
           05  S-TrxID        PIC X(11).
           05  S-SenderAcc    PIC 9(16).
           05  S-ReceiverAcc  PIC 9(16).
           05  S-Description  PIC X(30).
           05  S-Amount       PIC s9(10)v99.
           05  S-TrxType      PIC 9.
           05  S-TimeStamp    PIC 9(14).

       SD  WORK.
       01  WorkRecord.
           05  W-TrxID        PIC X(11).
           05  W-SenderAcc    PIC 9(16).
           05  W-ReceiverAcc  PIC 9(16).
           05  W-Description  PIC X(30).
           05  W-Amount       PIC s9(10)v99.
           05  W-TrxType      PIC 9.
           05  W-TimeStamp    PIC 9(14).

       WORKING-STORAGE SECTION.
       01  END-FILE            PIC X VALUE "N".
       01  T-BALANCE           PIC s9(12)V99 VALUE 0.
       01  withdraw            pic s9(10)v99.
       01  deposit             pic s9(10)v99.
       01  format-balance      pic zzzzzzzzzzz9.99.
       01  TYPE-NAME           PIC X(10).
       01  WITHDRAW-AMT        PIC Zzzzzzzzz9.99.
       01  DEPOSIT-AMT         PIC Zzzzzzzzz9.99.
       01  anykey              pic x.
       01  ws-fs               pic xx.
       01  statusCode          pic xx.

       01  DISPLAY-TIME.
           05 DSP-DATE.
              10 DSP-YEAR      PIC X(4).
              10 FILLER        PIC X(3)    VALUE " / ".
              10 DSP-MONTH     PIC X(2).
              10 FILLER        PIC X(3)    VALUE " / ".
              10 DSP-DAY       PIC X(2).

           05 DSP-HMS.
              10 DSP-HOUR      PIC X(2).
              10 FILLER        PIC X(3)    VALUE " : ".
              10 DSP-MIN       PIC X(2).


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

           OPEN INPUT  TrxFile
           *>display ws-fs
           OPEN OUTPUT SeqTrx
           *>display ws-fs
           MOVE "N" TO END-FILE
           PERFORM UNTIL END-FILE = "Y"
              READ TrxFile INTO TrxRecord
                 AT END
                    MOVE "Y" TO END-FILE
                 NOT AT END
                   IF SenderAcc = UAccNo OR
                      ReceiverAcc = UAccNo
                       WRITE S-TrxRecord FROM TrxRecord
                   END-IF
              END-READ
           END-PERFORM
           CLOSE TrxFile
           CLOSE SeqTrx.

           SORT WORK
               ON ASCENDING KEY S-TimeStamp
               USING SeqTrx GIVING TrxChrono

           OPEN INPUT TrxChrono.
           *>display ws-fs

           DISPLAY "==============================================="
                   "=============================================="
           DISPLAY color-blue "  Date          Time         Withdraw"
                   "       Deposit    Description" ESC RESETX
           DISPLAY "-----------------------------------------------"
                   "-----------------------------------------------"

           MOVE "N" TO END-FILE
           PERFORM UNTIL END-FILE = "Y"
               READ TrxChrono into Chr-TrxRecord
                   AT END
                       MOVE "Y" TO END-FILE
                   NOT AT END
                       *>display Chr-TrxRecord
                       MOVE SPACES TO TYPE-NAME
                       MOVE zeroS TO WITHDRAW-AMT
                       MOVE zeroS TO DEPOSIT-AMT
                       IF Chr-SenderAcc = UAccNo

                           *>display TrxRecord
                           MOVE Chr-TimeStamp(1:4)   TO DSP-YEAR
                           MOVE Chr-TimeStamp(5:2)   TO DSP-MONTH
                           MOVE Chr-TimeStamp(7:2)   TO DSP-DAY
                           MOVE Chr-TimeStamp(9:2)   TO DSP-HOUR
                           MOVE Chr-TimeStamp(11:2)  TO DSP-MIN

                           MOVE Chr-Amount to WITHDRAW-AMT
                           Add Chr-Amount to withdraw
                           DISPLAY color-blue   DSP-DATE"  "
                                   color-yellow DSP-HMS
                                   "  "ESC REDX WITHDRAW-AMT
                                   "  "ESC GREENX DEPOSIT-AMT ESC RESETX
                                   "  "Chr-Description
                       else if Chr-ReceiverAcc = UAccNo

                           MOVE Chr-TimeStamp(1:4)   TO DSP-YEAR
                           MOVE Chr-TimeStamp(5:2)   TO DSP-MONTH
                           MOVE Chr-TimeStamp(7:2)   TO DSP-DAY
                           MOVE Chr-TimeStamp(9:2)   TO DSP-HOUR
                           MOVE Chr-TimeStamp(11:2)  TO DSP-MIN

                           MOVE Chr-Amount to deposit-AMT
                           Add Chr-Amount to deposit
                           DISPLAY color-blue   DSP-DATE"  "
                                   color-yellow DSP-HMS
                                   "  "ESC REDX WITHDRAW-AMT
                                   "  "ESC GREENX DEPOSIT-AMT ESC RESETX
                                   "  "Chr-Description
                       END-IF
               END-READ
           END-PERFORM.
           compute BALANCE = DEPOSIT - WITHDRAW
           move BALANCE to format-balance
           DISPLAY "-----------------------------------------------"
                   "-----------------------------------------------"
           DISPLAY "Current Balance:                      "ESC GREENX
                   format-balance
           DISPLAY "==============================================="
                   "==============================================="
           display esc resetx
           CLOSE TrxFile

           DISPLAY esc redx
           display "Press any key to exit : "
           accept anykey
           DISPLAY esc resetx

           exit program.
