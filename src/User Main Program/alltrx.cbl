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
       01  ws-fs pic xx.
       01  eof pic x value 'n'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           open INPUT TrxFile
           perform until eof = 'y'
               read TrxFile into TrxRecord
               at end
                   move 'y' to eof
               not at end
                   DISPLAY TrxRecord
               END-READ
           END-PERFORM
           close TrxFile
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
