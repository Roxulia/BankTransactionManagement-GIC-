      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMain.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  homepageOpt pic 9(2).
       01  loginOpt pic 9(2).
       01  adminName pic X(20) value "test admin".
       01  adminRole pic 9(1) value 1.
       01  statusCode pic x(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *     call 'adminLogin'
            perform login-page
            STOP RUN.

       login-page.
           DISPLAY "==============================================="
           DISPLAY "=======Bank Transaction Management(Admin)======"
           DISPLAY "==============================================="
           DISPLAY "=                1.Login                      ="
           DISPLAY "=               99.Exit                       ="
           DISPLAY "==============================================="
           perform choice-opt-login.

       home-page.
           DISPLAY "=======Welcome " adminName "======"
           DISPLAY SPACE
           if adminRole = 1 THEN
               DISPLAY "=======Choose Options======="
               DISPLAY "=     1.Create User        ="
               DISPLAY "=     2.View User List     ="
               DISPLAY "=     3.Update User Info   ="
               DISPLAY "=     4.Deposit to User    ="
               DISPLAY "=     5.Create Admin       ="
               DISPLAY "=     6.View Admin List    ="
               DISPLAY "=     7.Update Admin Info  ="
               display "=    99.Log Out            ="
               display "============================"
               PERFORM choice-opt-home

           else
               DISPLAY "=======Choose Options======="
               DISPLAY "=     1.Create User        ="
               DISPLAY "=     2.View User List     ="
               DISPLAY "=     3.Update User Info   ="
               DISPLAY "=     4.Deposit to User    ="
               display "=    99.Log Out            ="
               display "============================"
               PERFORM choice-opt-home
           end-if.

       choice-opt-login.
           DISPLAY "Choosen Option Code : "
           accept loginOpt
           PERFORM UNTIL loginOpt = 99
               EVALUATE loginOpt
                   when EQUAL 1
                       PERFORM home-page
                   when OTHER
                       DISPLAY "INVALID OPTION CODE"
               END-EVALUATE
               DISPLAY "Choosen Option Code : "
               accept loginOpt
           END-PERFORM
           DISPLAY "Exitting the Program ...."
           stop run.

       choice-opt-home.
           DISPLAY "Choosen Option Code : "
           accept homepageOpt
           perform UNTIL homepageOpt = 99
               EVALUATE homepageOpt
                   when EQUAL 1
      *                call 'userCreate'
                       display "Create User Page"
                   when  EQUAL 2
      *                call 'userList'
                       display "View User List"
                   when  EQUAL 3
      *                call 'userUpdate'
                       display "Update User Info"
                   when  EQUAL 4
      *                call 'trxDeposit'
                       display "Deposit to User"
                   when EQUAL 5
      *                call 'adminCreate'
                       display "Create Admin Page"
                   when equal 6
      *                call 'adminList'
                       display "View Admin List"
                   when equal 7
      *                call 'adminUpdate'
                       display "Update Admin Info"
                   when OTHER
                       display "INVALID OPTION CODE"
               END-EVALUATE
               DISPLAY "Choosen Option Code : "
               accept homepageOpt
           END-PERFORM
           display "Logging Out..."
           perform login-page.
       END PROGRAM AMain.
