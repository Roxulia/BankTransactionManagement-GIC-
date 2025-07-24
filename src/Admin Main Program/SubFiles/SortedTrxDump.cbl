      *====================================================================*
      * Program: SortedTrxDumpWithSort.cbl                                 *
      * Purpose: Dump ALL transactions sorted by TimeStamp using COBOL SORT*
      *====================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SortedTrxDumpWithSort.
       AUTHOR. ChatGPT.
       DATE-WRITTEN. 2025-07-24.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Original indexed transactions file
           SELECT TrxFile ASSIGN TO 'Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRX-ID
               FILE STATUS IS WS-FS.

      * Sorted output file
           SELECT SortedOutput ASSIGN TO 'SORTED.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS.

      * Sort work file (temporary file for sorting)
           SELECT SortWork ASSIGN TO 'SORTWORK.TMP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

      *-- Input transactions file
       FD  TrxFile.
       01  TrxRecord.
           05  TRX-ID        PIC X(11).
           05  TRX-SENDER    PIC 9(16).
           05  TRX-RECEIVER  PIC 9(16).
           05  TRX-DESC      PIC X(30).
           05  TRX-AMOUNT    PIC S9(10)V99.
           05  TRX-TYPE      PIC 9.
           05  TRX-TIMESTAMP PIC 9(14).

      *-- Sorted output file
       FD  SortedOutput.
       01  OutputRecord.
           05  OUT-ID        PIC X(11).
           05  OUT-SENDER    PIC 9(16).
           05  OUT-RECEIVER  PIC 9(16).
           05  OUT-DESC      PIC X(30).
           05  OUT-AMOUNT    PIC S9(10)V99.
           05  OUT-TYPE      PIC 9.
           05  OUT-TIMESTAMP PIC 9(14).

      *-- Sort work file
       FD  SortWork.
       01  SortRecord.
           05  SRT-TIMESTAMP PIC 9(14).
           05  SRT-REST-OF-RECORD PIC X(86).

       WORKING-STORAGE SECTION.
       01  WS-FS          PIC XX.
       01  END-OF-FILE    PIC X    VALUE 'N'.
       01  DISPLAY-TIME   PIC X(14).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           *> Sort by timestamp (earliest to latest)
           SORT SortWork
                ON ASCENDING KEY SRT-TIMESTAMP
                USING TrxFile
                GIVING SortedOutput.

           *> Open sorted file for reading
           OPEN INPUT SortedOutput.

           *> Read and display sorted records
           PERFORM UNTIL END-OF-FILE = 'Y'
               READ SortedOutput
                   AT END
                       MOVE 'Y' TO END-OF-FILE
                   NOT AT END
                       MOVE OUT-TIMESTAMP TO DISPLAY-TIME
                       DISPLAY
                           'ID: ' OUT-ID
                           ' Sender: ' OUT-SENDER
                           ' Receiver: ' OUT-RECEIVER
                           ' Amount: ' OUT-AMOUNT
                           ' Type: '   OUT-TYPE
                           ' DateTime: ' DISPLAY-TIME
                           ' Desc: '   OUT-DESC
               END-READ
           END-PERFORM.

           *> Cleanup and exit
           CLOSE SortedOutput.
           STOP RUN.
