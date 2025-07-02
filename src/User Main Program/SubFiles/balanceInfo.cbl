      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. balanceInfo.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  UID pic 9(5).
       PROCEDURE DIVISION using REFERENCE UID.
           DISPLAY UID
           exit PROGRAM.
      *Procedures will be declare here*****

       END PROGRAM balanceInfo.
