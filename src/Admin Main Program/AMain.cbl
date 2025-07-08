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
       01  userid pic X(5).
       01  statusCode pic x(2) value "00".
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
           DISPLAY "=======Welcome " adminName "======="
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

       update-info-page.
           if homepageOpt equal 3
               display "=============================="
               DISPLAY "=      Update User Info      ="
               DISPLAY "=============================="
               display "=============================="
               DISPLAY "=           NOTE:            ="
               display "=    U can enter user ID     ="
               DISPLAY "=            OR              ="
               DISPLAY "=    'EXIT' to go back       ="
               DISPLAY "=============================="
               move 3 to homepageOpt
               perform choice-opt-update-info
           else if homepageOpt equal 7
               display "=============================="
               DISPLAY "=      Update Admin Info     ="
               DISPLAY "=============================="
               display "=============================="
               DISPLAY "=           NOTE:            ="
               display "=    U can enter Admin ID    ="
               DISPLAY "=            OR              ="
               DISPLAY "=    'EXIT' to go back       ="
               DISPLAY "=============================="
               move 7 to homepageOpt
               perform choice-opt-update-info
           end-if.

       deposit-page.
           display "=============================="
           DISPLAY "=    Deposit to User Acc     ="
           DISPLAY "=============================="
           display "=============================="
           DISPLAY "=           NOTE:            ="
           display "=    U can enter user ID     ="
           DISPLAY "=            OR              ="
           DISPLAY "=    'EXIT' to go back       ="
           DISPLAY "=============================="
           perform choice-opt-deposit.

       choice-opt-deposit.
           DISPLAY "Enter ID to be deposited : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"
           EVALUATE statusCode
               when equal "00"
                   DISPLAY "Deposited Balance for ID ("userid")"
                   perform deposit-page
               when EQUAL "01"
                   DISPLAY "ERROR IN MAKING TRANSACTION"
                   PERFORM deposit-page
               when EQUAL "98"
                   DISPLAY "DEPOSIT AMOUNT INVALID"
                   PERFORM deposit-page
               when equal "99"
                   DISPLAY "INVALID USER ID"
                   perform deposit-page
               when OTHER
                   DISPLAY "CANNOT PERFORM DEPOSIT PROCESS"
                   perform deposit-page
           END-EVALUATE
           END-PERFORM
           DISPLAY "Going Back To Main Screen"
           perform home-page.

       choice-opt-update-info.
           DISPLAY "Enter ID to be updated : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"
      *     if homepageOpt equal 3
      *        call 'userUpdate' using userid , statusCode
      *     else if homepageOpt equal 7
      *        call 'adminUpdate' using userid,statusCode
      *     end-if
           EVALUATE statusCode
               when equal "00"
                   DISPLAY "Updated Info for ID ("userid")"
                   perform update-info-page
               when OTHER
                   DISPLAY "CANNOT PERFORM UPDATE PROCESS"
                   perform update-info-page
           END-EVALUATE
           END-PERFORM
           DISPLAY "Going Back To Main Screen"
           perform home-page.


       choice-opt-login.
           DISPLAY "Choosen Option Code : "
           accept loginOpt
           PERFORM UNTIL loginOpt = 99
               EVALUATE loginOpt
                   when EQUAL 1
                       if statusCode = "00" THEN
                           PERFORM home-page
                       ELSE
                           display "INVALID CREDENTIAL"
                           perform login-page
                   when OTHER
                       DISPLAY "INVALID OPTION CODE"
                       perform login-page
               END-EVALUATE
           END-PERFORM
           DISPLAY "Exitting the Program ...."
           stop run.

       choice-opt-home.
           DISPLAY "Choosen Option Code : "
           accept homepageOpt
           perform UNTIL homepageOpt = 99
               EVALUATE homepageOpt
                   when EQUAL 1
                       call 'userCreate' using statusCode
                       display "Create User Page"
                       perform home-page
                   when  EQUAL 2
      *                call 'userList'
                       display "View User List"
                       perform home-page
                   when  EQUAL 3
                       perform update-info-page
                   when  EQUAL 4
                       perform deposit-page
                   when EQUAL 5
                       call '../SubFiles/bin/adminCreate' 
                       using statusCode
                       display "Create Admin Page"
                       perform home-page
                   when equal 6
      *                call 'adminList'
                       display "View Admin List"
                       perform home-page
                   when equal 7
                       perform update-info-page
                   when OTHER
                       display "INVALID OPTION CODE"
                       perform home-page
               END-EVALUATE
           END-PERFORM
           display "Logging Out..."
           perform login-page.
       END PROGRAM AMain.
