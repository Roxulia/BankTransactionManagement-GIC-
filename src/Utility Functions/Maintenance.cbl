      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Maintenance.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       DATA DIVISION.
<<<<<<< Updated upstream
       FILE SECTION.
=======
        FILE SECTION.
       FD userfile.
       01 userdata.
           05 UID      PIC 9(5).
           05 UName    PIC X(20).
           05 ULoginName PIC X(25).
           05 UEncodedPassword PIC X(32).
           05 UAddress PIC X(20).
           05 UPhone PIC x(9).
           05 UBalance PIC 9(10)V99.
           05 UTrxCount PIC 9(5).
           05 UDate PIC 9(6).
           05 UTime PIC 9(6).
>>>>>>> Stashed changes
       WORKING-STORAGE SECTION.
       linkage SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM Maintenance.
