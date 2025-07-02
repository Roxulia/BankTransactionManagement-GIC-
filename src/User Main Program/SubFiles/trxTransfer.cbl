      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxTransfer.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  receiverID pic 9(5).
       01  amount pic 9(10).
       01  optSucceed pic 9(1).
       PROCEDURE DIVISION USING REFERENCE receiverID,amount,optSucceed.
           DISPLAY receiverID "," amount
           move 1 to optSucceed
           exit program.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM trxTransfer.
