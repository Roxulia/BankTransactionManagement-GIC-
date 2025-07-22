      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. getUserByNRC.
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
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(25).
           05 UAccNumber PIC 9(16).
           05 UEncodedPassword PIC X(32).
           05 UNRC     PIC x(30).
           05 UAddress PIC X(20).
           05 UPhone PIC x(9).
           05 UBalance PIC 9(10)V99.
           05 UtrxCount pic 9(5).
           05 UDate PIC 9(8).
           05 UTime PIC 9(6).  *> New field: NRC number
       WORKING-STORAGE SECTION.
       01 WS-FS             PIC XX.
       01 EOF-Flag          PIC X VALUE 'N'.
       01 InputNRC          PIC X(30).
       01 FoundFlag         PIC X VALUE 'N'.
       01 space_count       pic 99.
       LINKAGE SECTION.
       01 NRCInput.
           05 NRC           PIC X(30).
       01 ReturnUserData.
           05 RET-UID      PIC 9(5).
           05 RET-UName    PIC X(20).
           05 RET-ULoginName PIC X(25).
           05 RET-UAccNumber pic 9(16).
           05 RET-UEncodedPassword PIC X(32).
           05 RET-UNRC     PIC x(30).
           05 RET-UAddress PIC X(20).
           05 RET-UPhone PIC x(9).
           05 RET-UBalance PIC 9(10)V99.
           05 RET-TrxCount PIC 9(5).
           05 RET-UDate PIC 9(8).
           05 RET-UTime PIC 9(6).
       01 statusCode        PIC XX.
       PROCEDURE DIVISION USING NRCInput, ReturnUserData, statusCode.
       MAIN-PROCEDURE.

           MOVE 'N' TO FoundFlag
           MOVE 'N' TO EOF-Flag
           OPEN INPUT testfile
           IF WS-FS NOT = '00'
               MOVE '99' TO statusCode
               DISPLAY WS-FS
               GO TO EXIT-PROGRAM
           END-IF
           PERFORM UNTIL EOF-Flag = 'Y'
               *>DISPLAY "reading"
               READ testfile INTO userdata
                   AT END
                       DISPLAY "EOF"
                       MOVE 'Y' TO EOF-Flag
                   NOT AT END
                       *>DISPLAY unrc InputNRC "a"
                       IF UNRC EQUAL NRC
                           MOVE UID TO RET-UID
                           MOVE UName TO RET-UName
                           MOVE ULoginName TO RET-ULoginName
                            MOVE UNRC        TO RET-UNRC
                            move UAccNumber  to RET-UAccNumber
                           MOVE UEncodedPassword TO RET-UEncodedPassword
                           MOVE UAddress TO RET-UAddress
                           MOVE UPhone TO RET-UPhone
                           MOVE UBalance TO RET-UBalance
                           MOVE UTrxCount TO RET-TrxCount
                           MOVE UDate TO RET-UDate
                           MOVE UTime TO RET-UTime
                           MOVE UNRC TO RET-UNRC
                           MOVE 'Y' TO FoundFlag
                           MOVE 'Y' TO EOF-Flag  *> Stop after finding
                       END-IF
               END-READ
           END-PERFORM

            IF FoundFlag = 'Y'
               MOVE '00' TO statusCode
           ELSE
               MOVE '96' TO statusCode
           END-IF.
       EXIT-PROGRAM.
           CLOSE testfile
           EXIT PROGRAM.

       END PROGRAM getUserByNRC.
