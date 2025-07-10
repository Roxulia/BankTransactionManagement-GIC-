       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkValidation.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE
           ASSIGN TO "users.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-RECORD.
           05 FILE-USERNAME PIC X(20).
           05 FILE-PASSWORD PIC X(32).

       WORKING-STORAGE SECTION.
       01 WS-EOF PIC A(1).
       01 WS-FOUND PIC A(1).
       01 WS-ENCRYPTED-PASSWORD PIC X(32).

       LINKAGE SECTION.
       01 WS-USERNAME PIC X(20).
       01 WS-PASSWORD PIC X(32).

       PROCEDURE DIVISION USING WS-USERNAME, WS-PASSWORD.
       MAIN-PARA.
           CALL "HASHPW" USING WS-PASSWORD, WS-ENCRYPTED-PASSWORD.
           PERFORM VERIFY-USER
           STOP RUN.

       VERIFY-USER.
           MOVE "N" TO WS-FOUND
           OPEN INPUT USER-FILE
           PERFORM UNTIL WS-EOF = "Y"
               READ USER-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-USERNAME = FILE-USERNAME AND
                          WS-ENCRYPTED-PASSWORD = FILE-PASSWORD
                           MOVE "Y" TO WS-FOUND
                           MOVE "Y" TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE USER-FILE

           IF WS-FOUND = "Y"
               DISPLAY "Login Successful!"
           ELSE
               DISPLAY "Invalid Username or Password"
           END-IF.
       END PROGRAM checkValidation.
