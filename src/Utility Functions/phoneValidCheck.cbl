      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-10-2025
      * Purpose:sub file for prompting for user phone and validate
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. phoneValidCheck.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "colorCodes.cpy".

       77  RAW-PHONE-IN     PIC X(20).
       77  DIGITS-ONLY      PIC X(20).
       77  DIGIT-COUNT      PIC 9(2) COMP.
       77  PHONE-LEN        PIC 9(2) COMP.
       77  I                PIC 9(4) COMP.
       77  VALID-PHONE         PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  UPh           PIC x(9).

       PROCEDURE DIVISION USING UPh.
           move 'N' to valid-phone
           PERFORM UNTIL VALID-PHONE = 'Y'
               DISPLAY "==========================================="
               DISPLAY "=  Enter new Phone Number: "
               ACCEPT RAW-PHONE-IN

               *> 2) Strip out non-digits
               MOVE SPACES TO DIGITS-ONLY
               MOVE FUNCTION TRIM(RAW-PHONE-IN) TO RAW-PHONE-IN
               display "Raw" raw-phone-in
              *> 2) Count numeric chars manually
               MOVE 0 TO DIGIT-COUNT
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF RAW-PHONE-IN
                   IF RAW-PHONE-IN(I:1) IS NUMERIC
                      ADD 1 TO DIGIT-COUNT
                   END-IF
               END-PERFORM

               *> 3) If exactly 9, build the digit string; else error
               IF DIGIT-COUNT = 9
                   *> reset and extract
                   MOVE SPACES TO DIGITS-ONLY
                   move raw-phone-in(1:9) to digits-only
                   DISPLAY digits-only
                   MOVE DIGITS-ONLy TO UPh
                   MOVE 'Y' TO VALID-PHONE
               ELSE
                  DISPLAY ESC REDX">> ERROR: You must enter "
                       WITH NO ADVANCING
                  DISPLAY "exactly 9 digits." ESC RESETX
               END-IF
           END-PERFORM.

       END PROGRAM phoneValidCheck.
