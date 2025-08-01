******************************************************************
      * Author: Sat Paing Thu
      * Date: 10.7.2025
      * Purpose: Pyaw Pya Woo
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. getUserByLoginName.
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
       01 Cached-UID      PIC X(25).
       01 Cached-Data.
           05 C-UID       PIC 9(5).
           05 C-UName     PIC X(20).
           05 C-ULogin    PIC X(25).
           05 C-UPassword PIC X(32).
           05 C-UAddress  PIC X(20).
           05 C-UPhone    PIC x(9).
           05 C-UBalance  PIC 9(10)V99.
           05 C-trxCount pic 9(5).
           05 C-UDate     PIC 9(8).
           05 C-UTime     PIC 9(6).
       01 EOF-Flag PIC X(1) VALUE 'N'.
       01 ws-fs pic x(2).
       01  ret-found pic x.

       LINKAGE SECTION.
       01 InputLoginName PIC x(25).
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
           05 RET-UBalance PIC s9(10)V99.
           05 RET-TrxCount PIC 9(5).
           05 RET-UDate PIC 9(8).
           05 RET-UTime PIC 9(6).

       PROCEDURE DIVISION
       USING InputLoginName, ReturnUserData , statusCode .
       MAIN-PROCEDURE.
            MOVE 'N' TO RET-Found
      *      IF InputLoginName = Cached-UID
               *>DISPLAY "NO NEED FILE READ"
      *         MOVE C-UID       TO RET-UID
      *         MOVE C-UName     TO RET-UName
      *         MOVE C-ULogin    TO RET-ULoginName
      *         MOVE C-UPassword TO RET-UEncodedPassword
      *         MOVE C-UAddress  TO RET-UAddress
      *         MOVE C-UPhone    TO RET-UPhone
      *         MOVE C-UBalance  TO RET-UBalance
      *         move C-trxCount to RET-trxCount
      *         MOVE C-UDate     TO RET-UDate
      *         MOVE C-UTime     TO RET-UTime
      *         MOVE 'Y' TO RET-Found
      *         move "00" to statusCode
      *         GOBACK
      *     END-IF
            OPEN INPUT testfile
            IF WS-FS NOT = "00"
               DISPLAY "File Status: " WS-FS
               DISPLAY "Error opening file!"
               move "99" to statusCode
               exit program
            END-IF
            PERFORM UNTIL EOF-Flag = 'Y'
                READ testfile INTO userdata
                    AT END
                        MOVE 'Y' TO EOF-Flag
                        move "96" to statusCode
                    NOT AT END
                        *>DISPLAY userdata
                        IF ULoginName = InputLoginName
                            MOVE UID         TO RET-UID
                            MOVE UName       TO RET-UName
                            MOVE ULoginName  TO RET-ULoginName
                            MOVE UNRC        TO RET-UNRC
                            move UAccno  to RET-UAccNumber
                            MOVE UEncpsw
                            TO RET-UEncodedPassword
                            MOVE UAddress  TO RET-UAddress
                            IF UPh NOT = 0
                                MOVE UPh TO RET-UPhone
                            END-IF
                            MOVE Balance TO RET-UBalance
                            move trxCount to RET-trxCount
                            MOVE UDate TO RET-UDate
                            MOVE UTime TO RET-UTime
                            MOVE 'Y' TO RET-Found
                            MOVE 'Y' TO EOF-Flag  *> Stop after finding
                           MOVE InputLoginName        TO Cached-UID
                           MOVE RET-UID          TO C-UID
                           MOVE RET-UName        TO C-UName
                           MOVE RET-ULoginName   TO C-ULogin
                           MOVE RET-UEncodedPassword TO C-UPassword
                           MOVE RET-UAddress     TO C-UAddress
                           MOVE RET-UPhone       TO C-UPhone
                           MOVE RET-UBalance     TO C-UBalance
                           move RET-trxCount to C-trxCount
                           MOVE RET-UDate        TO C-UDate
                           MOVE RET-UTime        TO C-UTime
                           move "00" to statusCode
                        END-IF
                END-READ
            END-PERFORM
                MOVE 'N' TO EOF-Flag
            CLOSE testfile
            GOBACK.

       END PROGRAM getUserByLoginName.
