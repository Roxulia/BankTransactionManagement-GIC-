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

       DATA DIVISION.
        FILE SECTION.
       FD userfile.
       01 userdata.
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(25).
           05 UEncodedPassword PIC X(32).
           05 UAddress PIC X(20).
           05 UPhone PIC 9(9).
           05 UBalance PIC 9(10)V99.
           05 UTrxCount PIC 9(5).
           05 UDate PIC 9(8).
           05 UTime PIC 9(6).
       WORKING-STORAGE SECTION.
       01 WS-UID          PIC x(25).
       01 ws-fs pic x(2).
       01  statusCode pic xx.
       01 WS-UserData.
           05 WS-RET-UID      PIC 9(5).
           05 WS-RET-UName    PIC X(20).
           05 WS-RET-ULoginName PIC X(25).
           05 RET-UEncodedPassword PIC X(32).
           05 WS-RET-UAddress PIC X(20).
           05 RET-UPhone PIC x(9).
           05 RET-UBalance PIC 9(10)V99.
           05 RET-TrxCount PIC 9(5).
           05 RET-UDate PIC 9(6).
           05 RET-UTime PIC 9(6).
           05 WS-RET-Found    PIC X(1).
       01 WS-INPUT-DATA.
           05 WS-INPUT-DATE      PIC 9(6).
           05 WS-INITIAL-AMOUNT  PIC 9(10)V99.
            05 WS-INPUT-TIME PIC 9(6).
       01 WS-OUTPUT-DATA.
           05 WS-UPDATED-DATE.
               10 WS-UPD-YEAR    PIC 9(2).
               10 WS-UPD-MONTH   PIC 9(2).
               10 WS-UPD-DAY     PIC 9(2).
           05 WS-FINAL-AMOUNT    PIC 9(10)V99.
           05 WS-INTEREST-AMOUNT PIC 9(6)V99.
           05 WS-TOTAL-MONTHS    PIC 9(4).
           05 WS-OUTPUT-TIME PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           DISPLAY "Enter User ID to search: "
           ACCEPT WS-UID
           IF WS-UID NOT = 0
           CALL "getUserByLoginName"
           USING by REFERENCE WS-UID, WS-UserData,statusCode
           IF statusCode = "00"
               MOVE RET-UDate TO WS-INPUT-DATE
               MOVE RET-UTime TO WS-INPUT-TIME
      *>          COMPUTE RET-UBalance = RET-UBalance + 10000
               MOVE RET-UBalance TO WS-INITIAL-AMOUNT
      *>          DISPLAY "OG : " RET-UBalance
      *>          Testing
      *>          MOVE 250621 TO WS-INPUT-DATE
               CALL "INTEREST-CALC" USING WS-INPUT-DATA, WS-OUTPUT-DATA
               MOVE WS-UPDATED-DATE TO RET-UDate
               MOVE WS-FINAL-AMOUNT TO RET-UBalance
               MOVE WS-OUTPUT-TIME TO RET-UTime

           *> Display results
                DISPLAY "-------------------------"
               DISPLAY "UID: " WS-RET-UID
               DISPLAY "Name: " WS-RET-UName
               DISPLAY "Login Name: " WS-RET-ULoginName
               DISPLAY "Encoded Password:"RET-UEncodedPassword
               DISPLAY "ADDRESS: " WS-RET-UAddress
               DISPLAY "Phone Number:  " RET-UPhone
               DISPLAY "Balance:  " RET-UBalance
               DISPLAY "Trx Count:  " RET-TrxCount
               DISPLAY "Date:  " RET-UDate
               DISPLAY "Total Monts for Interest: " WS-TOTAL-MONTHS
               DISPLAY "Time:  " RET-UTime
               DISPLAY "-----------------------------------------"
               MOVE WS-UserData TO userdata
                open i-o userfile
                   REWRITE userdata
                       INVALID KEY
                           MOVE "99" TO WS-FS
                       NOT INVALID KEY
                           MOVE "00" TO WS-FS
                   END-REWRITE
                   close userfile
`
           ELSE
               DISPLAY "User not found!"

           END-IF
           END-IF
           STOP RUN.
       END PROGRAM Maintenance.
