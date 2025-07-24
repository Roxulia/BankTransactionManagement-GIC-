      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TrxFile ASSIGN TO '../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.
           SELECT UserAccounts
               ASSIGN TO "../../../data/UserAccounts.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
           Select nrcfile
               assign to "../../../data/NRC.dat"
               ORGANIZATION is line SEQUENTIAL
               file status is ws-fs.
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
           05  TimeStamp   PIC 9(14).

       FD  UserAccounts.
       01  UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UAccNum     pic 9(16).
           05 UEncPsw     PIC X(32).
           05  UNRC       pic x(30).
           05 UAddress    PIC X(20).
           05 Phone       PIC X(9).
           05 Balance     PIC s9(10)V99.
           05 TrxCount    PIC 9(5).
           05 UDate       PIC 9(8).
           05 UTime       PIC 9(6).

       FD  nrcfile.
       01  nrclist.
           05  city_code pic xx.
           05  city_name pic x(10).
       WORKING-STORAGE SECTION.
       01  ws-fs pic xx.
       01  eof pic x value 'n'.
       01  txt-date pic x(8).
       01  txt-time pic x(6).
       01  curr-date pic 9(8).
       01  curr-Time pic 9(6).
       01  cname pic x(10).
       01  ccode pic xx.
       01  found pic x value 'n'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *>open INPUT TrxFile
           *>perform until eof = 'y'
             *>  read TrxFile into TrxRecord
             *>  at end
             *>      move 'y' to eof
             *>  not at end
             *>      DISPLAY TrxRecord
             *>  END-READ
           *>END-PERFORM
           *>close TrxFile
           open INPUT UserAccounts
           move 'n' to eof
           perform until eof = 'y'
               read UserAccounts into UserRecord
               at end
                   move 'y' to eof
               not at end
                   DISPLAY UserRecord
               END-READ
           END-PERFORM
           close UserAccounts
           move FUNCTION CURRENT-DATE(1:8) to curr-date
           move FUNCTION CURRENT-DATE(9:6) to curr-time
           DISPLAY txt-time
           DISPLAY txt-date
           DISPLAY curr-date
           DISPLAY curr-Time
           DISPLAY "enter City Code: "
           accept ccode
           DISPLAY "Enter City Name : "
           accept cname
           open INPUT nrcfile
           move 'n' to eof
           move 'n' to found
           perform until eof = 'y'
               read nrcfile into nrclist
               at end
                   move 'y' to eof
               not at end
                   if ccode = city_code and cname = city_name
                   move 'y' to found
                   move 'y' to eof
                   END-IF
               END-READ
           END-PERFORM
           if found = 'y'
               DISPLAY city_code " " city_name
           ELSE
               DISPLAY "Not Found"
           END-IF
           close nrcfile

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
