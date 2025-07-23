      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. getUserByAccNumber.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT testfile
           ASSIGN TO "../../../data/UserAccounts.dat"
           ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD testfile.
       01 userdata.
           copy '../Utility Functions/userFile.cpy'.

       WORKING-STORAGE SECTION.
       01 EOF-Flag PIC X(1) VALUE 'N'.
       01 ws-fs pic x(2).
       01  ret-found pic x.
       01  ws-uid pic 9(16).

       LINKAGE SECTION.
       01 InputUID1 PIC 9(16).
       01  statusCode pic xx.
       01 ReturnUserData.
           05 RET-UID      PIC 9(5).
           05 RET-UName    PIC X(20).
           05 RET-ULoginName PIC X(25).
           05 RET-UAccNumber pic 9(16).
           05 RET-UEncodedPassword PIC X(32).
           05 RET-UNRC     PIC x(30).
           05 RET-UAddress PIC X(20).
           05 RET-UPhone PIC x(11).
           05 RET-UBalance PIC 9(10)V99.
           05 RET-TrxCount PIC 9(5).
           05 RET-UDate PIC 9(8).
           05 RET-UTime PIC 9(6).

       PROCEDURE DIVISION
       USING InputUID1, ReturnUserData , statusCode.
       MAIN-PROCEDURE.
            INITIALIZE ws-uid
            move InputUID1 to ws-uid
            MOVE 'N' TO RET-Found
      *
            OPEN INPUT testfile
            IF WS-FS NOT = "00"
               move "99" to statusCode
               go to back-to-main
            END-IF
            PERFORM UNTIL EOF-Flag = 'Y'
                READ testfile INTO userdata
                    AT END
                        MOVE 'Y' TO EOF-Flag
                        move "96" to statusCode
                    NOT AT END
                        *>DISPLAY userdata
                        IF UAccNo = ws-uid
                            *>DISPLAY userdata
                            MOVE UID         TO RET-UID
                            MOVE UName       TO RET-UName
                            MOVE ULoginName  TO RET-ULoginName
                            MOVE UNRC        TO RET-UNRC
                            move UAccNo  to RET-UAccNumber
                            MOVE UEncpsw
                            TO RET-UEncodedPassword
                            MOVE UAddress  TO RET-UAddress
                            IF UPh NOT = 0
                                MOVE UPh TO RET-UPhone
                            END-IF
                            MOVE Balance TO RET-UBalance
                            move trxCount to RET-TrxCount
                            MOVE UDate TO RET-UDate
                            MOVE UTime TO RET-UTime
                            MOVE 'Y' TO RET-Found
                            MOVE 'Y' TO EOF-Flag  *> Stop after finding

                           move "00" to statusCode
                        END-IF
                END-READ
            END-PERFORM
                MOVE 'N' TO EOF-Flag
            CLOSE testfile
            .

       back-to-main.
           exit PROGRAM.

       END PROGRAM getUserByAccNumber.
