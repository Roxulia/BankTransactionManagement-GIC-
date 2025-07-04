      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestuserPassVal.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-USERNAME     PIC X(20).
       01  WS-PASSWORD     PIC X(20).
       01  WS-RETURN-CODE  PIC 9(2).
           88 VALID-INPUT    VALUE 0.
           88 EMPTY-USER     VALUE 1.
           88 EMPTY-PASS     VALUE 2.
           88 LENGTH-ERROR   VALUE 3.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM GET-LOGIN-INFO
           PERFORM VALIDATE-INPUT
           PERFORM DISPLAY-RESULT
           STOP RUN.

       GET-LOGIN-INFO.
           DISPLAY "Enter Username: "
           ACCEPT WS-USERNAME
           DISPLAY "Enter Password: "
           ACCEPT WS-PASSWORD.

       VALIDATE-INPUT.
           CALL 'userNamePassVal' USING WS-USERNAME
                                      WS-PASSWORD
                                      WS-RETURN-CODE.
       DISPLAY-RESULT.
           EVALUATE WS-RETURN-CODE
               WHEN 0
                   DISPLAY "Valid Input - Proceeding with login"
               WHEN 1
                   DISPLAY "Error: Username cannot be empty"
               WHEN 2
                   DISPLAY "Error: Password cannot be empty"
               WHEN 3
                   DISPLAY "Error: Invalid length or format"
               WHEN 4
                   DISPLAY "Error: Password must contain at least "
                    WITH NO ADVANCING
                   DISPLAY "one uppercase letter"
               WHEN 5
                   DISPLAY "Error: Password must contain at least "
                    WITH NO ADVANCING
                   DISPLAY "one lowercase letter"
               WHEN 6
                   DISPLAY "Error: Password must contain at least "
                    WITH NO ADVANCING
                   DISPLAY "one number"
               WHEN 7
                   DISPLAY "Error: Password must contain at least "
                    WITH NO ADVANCING
                   DISPLAY "one special character"
               WHEN 8
                   DISPLAY "Error: Password must be at least 9"
                   WITH NO ADVANCING
                   DISPLAY "characters long"
               WHEN 9
                    DISPLAY "Error: Username cannot be longer than"
                    WITH NO ADVANCING
                   DISPLAY " 20 characters"
               WHEN 10
                DISPLAY "Error: Password cannot be longer than"
                    WITH NO ADVANCING
                   DISPLAY " 20 characters"
           END-EVALUATE.
