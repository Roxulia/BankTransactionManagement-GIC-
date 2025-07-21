      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  nrc_string pic x(30).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            call 'userNRCVal' using by REFERENCE nrc_string
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
