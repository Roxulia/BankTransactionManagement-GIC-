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
           SELECT TrxFile ASSIGN TO 'TmpSeqTrx.dat'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE    IS SEQUENTIAL
               FILE STATUS    IS WS-FS.

           SELECT ChronoFile ASSIGN TO '../../../data/TrxChrono.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

           SELECT WORK ASSIGN TO 'WRK.DAT'.

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
           05  SenderAcc    PIC 9(16).
           05  ReceiverAcc  PIC 9(16).
           05  Description PIC X(30).
           05  Amount      PIC s9(10)v99.
           05  TrxType     PIC 9.
           05  TimeStamp   PIC 9(14).

       FD ChronoFile.
       01 ChrRecord.
           05  C-TrxID       PIC X(11).
           05  C-SenderAcc    PIC 9(16).
           05  C-ReceiverAcc  PIC 9(16).
           05  C-Description PIC X(30).
           05  C-Amount      PIC s9(10)v99.
           05  C-TrxType     PIC 9.
           05  C-TimeStamp   PIC 9(14).

       SD  WORK.
       01  WorkRecord.
           05  W-TrxID        PIC X(11).
           05  W-SenderAcc    PIC 9(16).
           05  W-ReceiverAcc  PIC 9(16).
           05  W-Description  PIC X(30).
           05  W-Amount       PIC s9(10)v99.
           05  W-TrxType      PIC 9.
           05  W-TimeStamp    PIC 9(14).

       FD  UserAccounts.
       01  UserRecord.
           05 UID         PIC 9(5).
           05 UName       PIC X(20).
           05 ULoginName  PIC X(25).
           05 UAccNum     pic 9(16).
           05 UEncPsw     PIC X(32).
           05  UNRC       pic x(30).
           05 UAddress    PIC X(20).
           05 Phone       PIC X(11).
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

           SORT WORK
               ON ASCENDING KEY TimeStamp
               USING TrxFile GIVING ChronoFile

           open INPUT UserAccounts
           DISPLAY ws-fs
           perform until eof = 'y'
               read UserAccounts into userRecord
               at end
                   move 'y' to eof
               not at end
                   DISPLAY UserRecord
               END-READ
           END-PERFORM
           close TrxFile

           open INPUT TrxFile
           DISPLAY ws-fs
           perform until eof = 'y'
               read TrxFile into TrxRecord
               at end
                   move 'y' to eof
               not at end
                   DISPLAY TimeStamp
               END-READ
           END-PERFORM
           close TrxFile

           open INPUT ChronoFile
           DISPLAY ws-fs
           move 'n' to eof
           perform until eof = 'y'
               read ChronoFile into ChrRecord
               at end
                   move 'y' to eof
               not at end
                   DISPLAY ChrRecord
               END-READ
           END-PERFORM
           close ChronoFile


            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
