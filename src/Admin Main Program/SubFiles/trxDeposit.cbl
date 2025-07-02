      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxDeposit.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE section.
       01  userId pic 9(5).
       01  amount pic 9(10).
       01  optStatus pic 9(1).
       PROCEDURE DIVISION using REFERENCE userId,amount,optStatus.
      *Code Here....
           exit PROGRAM.
       END PROGRAM trxDeposit.
