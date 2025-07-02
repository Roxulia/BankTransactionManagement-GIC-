      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. encryption.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  password pic x(20).
       01  encPassword pic x(255).
       PROCEDURE DIVISION using REFERENCE password,encPassword.
           move password to encPassword
           exit PROGRAM.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM encryption.
