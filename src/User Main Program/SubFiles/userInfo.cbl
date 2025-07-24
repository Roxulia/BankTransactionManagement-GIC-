      ******************************************************************
      * Author: Nyan Ye Thu
      * Date:4.7.2025
      * Purpose:Bank Transaction Management
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userInfo.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 InputUID PIC 9(5).
       01 Found    PIC X(1) VALUE 'N'.
       01 EOF-Flag PIC X(1) VALUE 'N'.
       01  optCode pic x.
       01  ws-fs pic x(2).
       01  statuscode pic xx.
       01  UserRecord.
          COPY "../../Utility Functions/userFile.cpy".
       01 ws-balance pic zzzzzzzzz9.99 USAGE DISPLAY.

       copy '../../Utility Functions/colorCodes.cpy'.
       linkage SECTION.
       01 InputUID1 PIC 9(5).

       PROCEDURE DIVISION using InputUID1.
       MAIN-PROCEDURE.
            INITIALIZE InputUID
            move InputUID1 to InputUID
            call '../../Utility Functions/bin/getUserByID'
               using by REFERENCE inputUID,UserRecord,statusCode
            EVALUATE statuscode
               when EQUAL "96"
                   display esc redx "User Not Found"
                   DISPLAY esc resetx

               when EQUAL "99"
                   DISPLAY esc redx
                   DISPLAY "Error Occurs"
                   DISPLAY esc resetx

               when EQUAL "00"
                   DISPLAY esc greenx
                   DISPLAY "========User Profile Info========"
                   DISPLAY "UserName : " UName
                   DISPLAY "Address : " UAddress
                   DISPLAY "Account No : " UAccno
                   DISPLAY "Phone : " UPh
                   DISPLAY "Balance : " WITH NO ADVANCING
                   MOVE balance to ws-balance 
                   DISPLAY  ws-balance 
                   DISPLAY "================================="
                   DISPLAY esc redx
                   DISPLAY "enter any key to exit : "
                   ACCEPT optCode
                   DISPLAY esc resetx

           END-EVALUATE
           exit PROGRAM.

       END PROGRAM userInfo.
