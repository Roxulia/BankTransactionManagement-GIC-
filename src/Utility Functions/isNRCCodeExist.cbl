      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. isNRCCodeExist.ENVIRONMENT DIVISION.
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
       01  eof pic x.
       LINKAGE SECTION.
       01  nrc_code pic xx.
       01  nrc_city pic x(10).
       01  statusCode pic xx.
       PROCEDURE DIVISION using nrc_code nrc_city statusCode.
       MAIN-PROCEDURE.
           move 'n' to eof
           open INPUT nrcfile
           if ws-fs not EQUAL '00'
               move '99' to statusCode
               exit PROGRAM
           end-if
           perform until eof = 'y'
               read nrcfile into nrclist
               at end
                   move 'y' to eof
                   move '90' to statusCode
               not at END
                   if nrc_code equal city_code
                       and nrc_city EQUAL city_name
                       move '00' to statusCode
                       move 'y' to eof

                       close nrcfile
                       exit program
                   END-IF
               END-READ
           END-PERFORM
           close nrcfile
           exit PROGRAM
           .
       END PROGRAM isNRCCodeExist.
