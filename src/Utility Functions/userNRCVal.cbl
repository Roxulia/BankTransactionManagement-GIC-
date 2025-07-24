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

       01  ws-nrc-string pic x(30).
       01  ws-temp pic x(30).
       01  nrc_code pic xx.
       01  nrc_city pic x(10).
       01  nrc_number pic x(6).
       01  text-input  pic x(20).
       01  nrc_status pic x.
       01  ws-status1 pic xx.
       01  ws-status2 pic xx.
       01  has_slash pic x.
       01  has_paren1 pic x.
       01  has_paren2 pic x.
       01  slash pic 99.
       01  paren1 pic 99.
       01  paren2 pic 99.

       LINKAGE SECTION.
       01  nrc-string pic x(30).
       PROCEDURE DIVISION using nrc-string.
       MAIN-PROCEDURE.
            INITIALIZE ws-nrc-string
            INITIALIZE slash
            INITIALIZE paren1
            INITIALIZE paren2
            INITIALIZE has_slash
            INITIALIZE has_paren2
            INITIALIZE has_paren1
            INITIALIZE ws-status1
            INITIALIZE ws-status2

            perform
            UNTIL ws-status1 = "00"
             and ws-status2 = "00" and has_slash = 'y'
             and has_paren1='y' and has_paren2 = 'y'
             perform input-process
             END-PERFORM
            if ws-status1 = '99'
                DISPLAY "Error in NRC Data file"
                exit PROGRAM
            else
                move ws-temp
                to nrc-string
            END-IF
            exit PROGRAM.

       input-process.

            DISPLAY "Enter NRC : "
            ACCEPT ws-nrc-string
            if ws-nrc-string EQUAL "EXIT" or ws-nrc-string = "exit"
                move "EXIT" to nrc-string
                exit PROGRAM
           END-IF
            inspect ws-nrc-string tallying slash for all "/"
            inspect ws-nrc-string TALLYING paren1 for all "("
            if slash not equal 1
                move 'n' to has_slash
                DISPLAY "Must contain one '/' "

            ELSE
                move 'y' to has_slash
                *>DISPLAY "Check 1 passed"
            END-IF
            if paren1 not equal 1
                move 'n' to has_paren1
                DISPLAY "Must contain '('"

            ELSE
                move 'y' to has_paren1
                *>DISPLAY "Check 2 passed"
            END-IF

            inspect ws-nrc-string TALLYING paren2 for all ")"
            if paren2 not equal 1
                move 'n' to has_paren2
                *>DISPLAY "Must contain ')'"

            ELSE
                move 'y' to has_paren2
                *>DISPLAY "Check 3 passed"
            END-IF
            if has_slash = 'y' and has_paren1 = 'y' and has_paren2 = 'y'
                INITIALIZE ws-temp
                move ws-nrc-string to ws-temp
                UNSTRING ws-nrc-string DELIMITED by "/"
                into nrc_code ws-nrc-string
                UNSTRING ws-nrc-string DELIMITED by "("
                into nrc_city ws-nrc-string
                UNSTRING ws-nrc-string DELIMITED by ")"
                into nrc_status nrc_number

                MOVE FUNCTION UPPER-CASE(nrc_city) TO nrc_city


                CALL '../../Utility Functions/bin/isNRCCodeExist'
                   USING by REFERENCE
                nrc_code nrc_city ws-status1
                if ws-status1 EQUAL "90"
                    DISPLAY "INVALID CITY CODE"

                end-if
                INITIALIZE text-input
                move nrc_number to text-input
                call '../../Utility Functions/bin/numberCheck' using
                by REFERENCE text-input ws-status2

                if ws-status2 EQUAL "10"
                    DISPLAY "INVALID NRC NUMBER"
                END-IF
            end-if.
       END PROGRAM userNRCVal.
