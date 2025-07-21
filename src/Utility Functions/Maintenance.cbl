       IDENTIFICATION DIVISION.
       PROGRAM-ID. Maintenance.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT userfile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
           SELECT TrxFile ASSIGN TO '../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
        FILE SECTION.
       FD userfile.
       01 userdata.
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(25).
           05 UEncodedPassword PIC X(32).
           05 UAddress PIC X(20).
           05 UPhone PIC x(9).
           05 UBalance PIC 9(10)V99.
           05 UTrxCount PIC 9(5).
           05 UDate PIC 9(8).
           05 UTime PIC 9(6).

       FD  TrxFile.
       01  TransactionRecord.
           05  TrxID       PIC X(11).
           05  SenderID    PIC 9(5).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10)v99.
           05  TrxType     PIC 9.
           05  TimeStamp   PIC 9(14).

       WORKING-STORAGE SECTION.
       01 WS-UID          PIC x(25).
       01 ws-fs pic x(2).
       01  eof pic x.
       01  statusCode pic xx.
       01  currentdate pic 9(6).
       01  currenttime pic 9(6).
       01  current-yr pic 9(2).
       01  interest pic v999 value 0.002.
       01  temp-balance pic 9(10)v99.
       01  record_count pic 9(11).
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURR-DATE.
               10  WS-CURR-YEAR    PIC 9(4).
               10  WS-CURR-MONTH   PIC 9(2).
               10  WS-CURR-DAY     PIC 9(2).
           05  WS-CURR-TIME.
               10  WS-CURR-HOUR    PIC 9(2).
               10  WS-CURR-MINUTES PIC 9(2).
               10  WS-CURR-SECONDS PIC 9(2).
       01  ws-user-udate.
           05  udate-yr pic 9(4).
           05  udate-month pic 9(2).
           05  udate-day pic 99.
       01  ws-user-utime.
           05  utime-hr pic 99.
           05  utime-mm pic 99.
           05  utime-ss pic 99.

       copy 'trxConstants.cpy'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           move 'n' to eof
           move 0 to record_count
           move FUNCTION CURRENT-DATE to WS-CURRENT-DATE-FIELDS
           if WS-CURR-DAy = payday *>and WS-CURR-TIME = paytime
               perform interest-add
           ELSE
               DISPLAY WS-CURR-DAY " " WS-CURR-TIME
           end-if
           stop run
           .

       interest-add.
           open i-o userfile
           if ws-fs not equal "00"
               display "FILE ERROR"
               close userfile
               stop run
           END-IF
           perform until eof = 'y'
               read userfile into userdata
               at end
                   move 'y' to eof
                   DISPLAY "Done" record_count "record"
               not at END
                   *>DISPLAY userdata
                   move UDate to ws-user-udate
                   if WS-CURR-DATE > ws-user-udate
                       add 1 to record_count
                       compute temp-balance = UBalance * interest
                       compute UBalance = UBalance + temp-balance

                       move ws-curr-date to UDate
                       move WS-CURR-TIME to UTime
                       PERFORM TRXID-GENERATE
                       REWRITE userdata
                           INVALID KEY
                               display "Error in updating"
                           not INVALID KEY
                               DISPLAY "Successfully updated"
                               move temp-balance to Amount
                               move UID to ReceiverID
                               move 0 to SenderID
                               move 'Interest' to Description
                               move 4 to TrxType
                               move FUNCTION CURRENT-DATE(1:14)
                               to TimeStamp
                               open i-o TrxFile
                               WRITE TransactionRecord
                                  INVALID KEY
                                       DISPLAY
                                       "Writing transaction failed."


                                       CLOSE TrxFile
                                   not INVALID KEY
                                       display "Added TransactionRecord"
                               END-WRITE
                               close TrxFile
                       END-REWRITE
                   END-IF
               END-READ
           END-PERFORM
           close userfile
           .

       TRXID-GENERATE.

           ADD 1 TO UTrxCount

           STRING
               UTrxCount DELIMITED BY SIZE
               WS-TrxReciPrefix DELIMITED BY SIZE
               UId DELIMITED BY SIZE
               INTO TrxID
           END-STRING

           DISPLAY "Generated TrxID: " TrxID.


       END PROGRAM Maintenance.
