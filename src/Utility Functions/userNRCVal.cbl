      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userNRCVal.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       file-control.
       Select nrcfile
               assign to "../../../data/NRC.dat"
               ORGANIZATION is line SEQUENTIAL
               file status is ws-fs.
       DATA DIVISION.
       FILE SECTION.
       FD  nrcfile.
       01  nrclist.
           05  city_code pic xx.
           05  city_name pic x(10).
       WORKING-STORAGE SECTION.
       01  ws-fs pic xx.
       01  nrc_code pic xx.
       01  nrc_city pic x(10).
       01  nrc_number pic 9(6).
       LINKAGE SECTION.
       01  nrc-string pic x(30).
       01  statusCode pic xx.
       PROCEDURE DIVISION using nrc-string , statusCode .
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.
       END PROGRAM userNRCVal.
