      ******************************************************************
      * Author: Myo Thein Chit
      * Date: 7-10-2025
      * Purpose: Sub file for prompting for user phone and validate
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. phoneValidCheck.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       *> For display colors
       COPY "colorCodes.cpy".

       77  RAW-PHONE-IN     PIC X(20).
       77  DIGITS-ONLY      PIC X(20).
       77  DIGIT-COUNT      PIC 9(2) COMP.
       77  PHONE-LEN        PIC 9(2) COMP.
       77  I                PIC 9(4) COMP.
       77  VALID-PHONE      PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  UPh              PIC X(11).

       PROCEDURE DIVISION USING UPh.
           MOVE 'N' TO VALID-PHONE
           PERFORM UNTIL VALID-PHONE = 'Y'
               DISPLAY "==========================================="
               DISPLAY "=  Enter new Phone Number: "
               ACCEPT RAW-PHONE-IN
               if Raw-phone-in EQUAL "exit" or raw-phone-in = "EXIT"
                   move "EXIT" to UPh
                   exit PROGRAM
               END-IF
               *> Strip non-digits
               MOVE SPACES TO DIGITS-ONLY
               MOVE FUNCTION TRIM(RAW-PHONE-IN) TO RAW-PHONE-IN
               MOVE 0 TO DIGIT-COUNT

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF RAW-PHONE-IN
                   IF RAW-PHONE-IN(I:1) IS NUMERIC
                       ADD 1 TO DIGIT-COUNT
                       MOVE RAW-PHONE-IN(I:1) TO
                           DIGITS-ONLY(DIGIT-COUNT:1)
                   END-IF
               END-PERFORM

               *> Validate
               IF DIGITS-ONLY(1:2) NOT = "09"
                   DISPLAY ESC REDX ">>ERROR: Phone must start with 09."
                   DISPLAY ESC RESETX
               ELSE
                   IF DIGITS-ONLY(3:1) = "5"
                       IF DIGIT-COUNT = 9
                           MOVE DIGITS-ONLY TO UPh
                           MOVE 'Y' TO VALID-PHONE
                       ELSE
                           DISPLAY ESC REDX
                           DISPLAY">> ERROR: phone number invalid"
                           DISPLAY ESC RESETX
                       END-IF
                   ELSE
                       IF DIGIT-COUNT = 11
                           MOVE DIGITS-ONLY TO UPh
                           MOVE 'Y' TO VALID-PHONE
                       ELSE
                           DISPLAY ESC REDX
                           DISPLAY">> ERROR: phone number invalid"
                           DISPLAY ESC RESETX
                       END-IF
                   END-IF
               END-IF

           END-PERFORM.

       END PROGRAM phoneValidCheck.
