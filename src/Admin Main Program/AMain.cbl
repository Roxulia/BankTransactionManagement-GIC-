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
       01  adminId pic 9(5).
       01  userid pic X(5).
       01  edit-id pic 9(5).
       01  statusCode pic x(2) value "00".
       *>For display colors
       77  RED-CODE            PIC X(6) VALUE "[1;31m".*> set red
       77  ESC                 PIC X    VALUE X"1B".   *> ASCII ESC
       77  RESET-CODE          PIC X(4) VALUE "[0m".   *> reset
       77  GREEN-CODE          PIC X(6) VALUE "[1;32m".*>set green
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
               DISPLAY "=     8.Generate Report    ="
               display "=    99.Log Out            ="
               display "============================"
               PERFORM choice-opt-home

           else
               DISPLAY "=======Choose Options======="
               DISPLAY "=     1.Create User        ="
               DISPLAY "=     2.View User List     ="
               DISPLAY "=     3.Update User Info   ="
               DISPLAY "=     4.Deposit to User    ="
               DISPLAY "=     5.Update Your Info   ="
               display "=     6.Generate Report    ="
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

       generate-report-page.
           display "=============================="
           DISPLAY "=    Generate Trx Report     ="
           DISPLAY "=============================="
           display "=============================="
           DISPLAY "=           NOTE:            ="
           display "=    U can enter user ID     ="
           DISPLAY "=            OR              ="
           DISPLAY "=    'EXIT' to go back       ="
           DISPLAY "=============================="
           perform choice-opt-genrp.

       choice-opt-genrp.
           DISPLAY "Enter User ID : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"
           call '../../Utility Functions/bin/numberCheck' USING
           by REFERENCE userid,statusCode
           if statusCode equal "00"
               move userid to edit-ID
               CALL '../../Utility Functions/bin/generateReport'
               USING REFERENCE edit-id
               perform generate-report-page
           Else
               DISPLAY esc RED-CODE
               DISPLAY "Invalid Input Type"
               DISPLAY esc RESET-CODE
               PERFORM generate-report-page
           end-if
           END-PERFORM
           DISPLAY "Going Back To Main Screen"
           perform home-page.

       choice-opt-deposit.
           DISPLAY "Enter ID to be deposited : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"
           call '../../Utility Functions/bin/numberCheck' USING
           by REFERENCE userid,statusCode
           if statusCode equal "00"
               move userid to edit-ID
               CALL '../SubFiles/bin/trxDeposit'
               USING REFERENCE edit-id statusCode
               perform deposit-page
           Else
               DISPLAY esc RED-CODE
               DISPLAY "Invalid Input Type"
               DISPLAY esc RESET-CODE
               PERFORM deposit-page
           end-if
           END-PERFORM
           DISPLAY "Going Back To Main Screen"
           perform home-page.

       choice-opt-update-info.
           DISPLAY "Enter ID to be updated : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"
           call '../../Utility Functions/bin/numberCheck' USING
           by REFERENCE userid,statusCode
           if statusCode equal "00"
               move userid to edit-ID
               if homepageOpt equal 3
                  call '../SubFiles/bin/userUpdate'
                  using by REFERENCE edit-id, statusCode
                  EVALUATE statusCode
                   when equal "00"
                       DISPLAY esc GREEN-CODE
                       DISPLAY "Updated Info for ID ("userid")"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
                   when equal "96"
                       DISPLAY esc GREEN-CODE
                       DISPLAY "NOT FOUND USER WITH ID ("userid")"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
                   when OTHER
                       DISPLAY ESC RED-CODE
                       DISPLAY "CANNOT PERFORM UPDATE PROCESS"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
               END-EVALUATE
               else if homepageOpt equal 7
                  call '../SubFiles/bin/adminUpdate'
                  using by REFERENCE edit-id,statusCode
                  EVALUATE statusCode
                   when equal "00"
                       DISPLAY esc GREEN-CODE
                       DISPLAY "Updated Info for ID ("userid")"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
                   when equal "96"
                       DISPLAY esc GREEN-CODE
                       DISPLAY "NOT FOUND ADMIN WITH ID ("userid")"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
                   when OTHER
                       DISPLAY esc RED-CODE
                       DISPLAY "CANNOT PERFORM UPDATE PROCESS"
                       DISPLAY esc RESET-CODE
                       perform update-info-page
               END-EVALUATE
               end-if
           Else
           DISPLAY esc RED-CODE
           DISPLAY "Invalid Input Type"
           DISPLAY esc RESET-CODE
           PERFORM update-info-page
           end-if
           END-PERFORM
           DISPLAY "Going Back To Main Screen"
           perform home-page.


       choice-opt-login.
           DISPLAY "Choosen Option Code : "
           accept loginOpt
           PERFORM UNTIL loginOpt = 99
               EVALUATE loginOpt
                   when EQUAL 1
                       call '../SubFiles/bin/adminLogin'
                           using adminId,
                           adminName , adminRole , statusCode
                       EVALUATE statusCode
                           when equal "00"
                               display ESC GREEN-CODE"LOG IN SUCCESSFUL"
                               DISPLAY ESC RESET-CODE
                               perform home-page
                           when equal "96"
                               DISPLAY ESC RED-CODE "ADMIN NOT FOUND!!"
                               DISPLAY esc RESET-CODE
                               perform login-page
                           when equal "95"
                               DISPLAY esc RED-CODE"INVALID CREDENTIAL!"
                               DISPLAY esc RESET-CODE
                               perform login-page
                       END-EVALUATE
                   when OTHER
                       DISPLAY esc RED-CODE "INVALID OPTION CODE"
                       DISPLAY esc RESET-CODE
                       perform login-page
               END-EVALUATE
           END-PERFORM
           DISPLAY esc RED-CODE
           DISPLAY "Exitting the Program ...."
           DISPLAY esc RESET-CODE
           stop run.

       choice-opt-home.
           DISPLAY "Choosen Option Code : "
           accept homepageOpt
           perform UNTIL homepageOpt = 99
               EVALUATE homepageOpt
                   when EQUAL 1
                       call '../SubFiles/bin/userCreate'
                       using statusCode
                       DISPLAY SPACE
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
                       if adminRole equal 1 then
                           call '../SubFiles/bin/adminCreate'
                           using statusCode
                           display SPACE
                           perform home-page
                       else
                           call '../SubFiles/bin/adminUpdate'
                               using adminId
                           perform home-page
                       END-IF
                   when equal 6
                       if adminRole equal 1 then
                           call 'adminList'
                           display "View Admin List"
                           perform home-page
                       else
                           perform generate-report-page
                       END-IF
      *
                   when equal 7
                       perform update-info-page
                   when equal 8
                       perform generate-report-page
                   when OTHER
                       display esc RED-CODE
                       display "INVALID OPTION CODE"
                       display esc RESET-CODE
                       perform home-page
               END-EVALUATE
           END-PERFORM
           DISPLAY esc RED-CODE
           display "Logging Out..."
           DISPLAY esc RESET-CODE
           perform login-page.
       END PROGRAM AMain.
