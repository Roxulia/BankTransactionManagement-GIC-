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
       01  userAccNum pic x(16).
       01  text-input pic x(20).
       01  unrc pic x(30).
       01  edit-id pic 9(5).
       01  dpAccNum pic 9(16).
       01  statusCode pic x(2) value "00".

       *>For display colors
       copy '../Utility Functions/colorCodes.cpy'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *     call 'adminLogin'
            perform login-page
            STOP RUN.

       login-page.
           DISPLAY color-blue
           DISPLAY "==============================================="
           DISPLAY "=======Bank Transaction Management(Admin)======"
           DISPLAY "==============================================="
           DISPLAY "=                1.Login                      ="
           DISPLAY "=               99.Exit                       ="
           DISPLAY "==============================================="
           display esc resetx
           perform choice-opt-login.

       home-page.
           DISPLAY color-blue
           DISPLAY "=======Welcome " ESC GREENX
                   FUNCTION TRIM(adminName) COLOR-BLUE"======="
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
               DISPLAY esc resetx
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
               DISPLAY esc resetx
               PERFORM choice-opt-home
           end-if.

       update-info-page.
           if homepageOpt equal 3
               DISPLAY color-blue
               display "=============================="
               DISPLAY "=      Update User Info      ="
               DISPLAY "=============================="
               display "=============================="
               DISPLAY "=           NOTE:            ="
               display "=    U can enter user NRC    ="
               DISPLAY "=            OR              ="
               DISPLAY "=    'EXIT' to go back       ="
               DISPLAY "=============================="
               DISPLAY esc resetx
               move 3 to homepageOpt
               perform choice-opt-update-info
           else if homepageOpt equal 7
               DISPLAY color-blue
               display "=============================="
               DISPLAY "=      Update Admin Info     ="
               DISPLAY "=============================="
               display "=============================="
               DISPLAY "=           NOTE:            ="
               display "=    U can enter Admin ID    ="
               DISPLAY "=            OR              ="
               DISPLAY "=    'EXIT' to go back       ="
               DISPLAY "=============================="
               DISPLAY esc resetx
               move 7 to homepageOpt
               perform choice-opt-update-info

           end-if.

       deposit-page.
           DISPLAY color-blue
           display "=============================="
           DISPLAY "=    Deposit to User Acc     ="
           DISPLAY "=============================="
           display "=============================="
           DISPLAY "=           NOTE:            ="
           display "=  U can enter user Acc Num  ="
           DISPLAY "=            OR              ="
           DISPLAY "=    'EXIT' to go back       ="
           DISPLAY "=============================="
           DISPLAY esc resetx
           perform choice-opt-deposit.

       generate-report-page.
           DISPLAY color-blue
           display "=============================="
           DISPLAY "=    Generate Trx Report     ="
           DISPLAY "=============================="
           display "=============================="
           DISPLAY "=           NOTE:            ="
           display "=    U can enter user ID     ="
           DISPLAY "=            OR              ="
           DISPLAY "=    'EXIT' to go back       ="
           DISPLAY "=============================="
           DISPLAY esc resetx
           perform choice-opt-genrp.

       choice-opt-genrp.
           DISPLAY "Enter User ID : "
           ACCEPT userid.
           perform until userid = "EXIT" or userid = "exit"

           INITIALIZE text-input
           move userid to text-input
           call '../../Utility Functions/bin/numberCheck' USING
           by REFERENCE text-input,statusCode
           if statusCode equal "00"
               move userid to edit-ID
               CALL '../../Utility Functions/bin/generateReport'
               USING REFERENCE edit-id
               perform generate-report-page
           Else
               DISPLAY esc REDx
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! Invalid Input Type  !"
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY esc RESETx
               PERFORM generate-report-page
           end-if
           END-PERFORM
           DISPLAY esc COLOR-pink
           DISPLAY "++++++++++++++++++++++++++++++++"
           DISPLAY "+ Going Back To Main Screen... +"
            DISPLAY "+++++++++++++++++++++++++++++++"
           DISPLAY esc resetx
           perform home-page.

       choice-opt-deposit.
           DISPLAY "Enter Acc Number to be deposited : "
           ACCEPT userAccNum
           INITIALIZE text-input
           move userAccNum to text-input
           perform until text-input = "EXIT" or text-input = "exit"
           call '../../Utility Functions/bin/numberCheck' USING
           by REFERENCE text-input,statusCode
           if statusCode equal "00"
               move userAccNum to dpAccNum
               *>DISPLAY dpAccNum
               CALL '../SubFiles/bin/trxDeposit'
               USING REFERENCE dpAccNum statusCode
               perform deposit-page
           Else
               DISPLAY esc REDx
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY "! Invalid Input Type  !"
               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
               DISPLAY esc RESETx
               PERFORM deposit-page
           end-if
           END-PERFORM
           DISPLAY color-pink
           DISPLAY "++++++++++++++++++++++++++++++++"
           DISPLAY "+ Going Back To Main Screen... +"
           DISPLAY "++++++++++++++++++++++++++++++++"
           DISPLAY esc resetx
           perform home-page.

       choice-opt-update-info.
           if homepageOpt EQUAL 3

               DISPLAY "Enter NRC to be updated : "
               ACCEPT unrc
               perform until unrc = "EXIT" or unrc = "exit"
                  call '../SubFiles/bin/userUpdate'
                  using by REFERENCE unrc, statusCode
                  EVALUATE statusCode
                   when equal "00"
                       DISPLAY esc GREENx
                       DISPLAY "*************************"
                       DISPLAY "* Updated Info for User *"
                       DISPLAY "*************************"
                       DISPLAY esc RESETx
                       perform update-info-page
                   when equal "96"
                       DISPLAY esc redx
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY "! NOT FOUND USER WITH NRC !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY esc RESETx
                       perform update-info-page
                   when OTHER
                       DISPLAY ESC REDx
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY "! CANNOT PERFORM UPDATE PROCESS !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY esc RESETx
                       perform update-info-page
                   END-EVALUATE
               END-PERFORM
               DISPLAY "Going Back To Main Screen"
               perform home-page
           else
               if homepageOpt equal 7
               DISPLAY "Enter ID to be updated : "
               ACCEPT userid
               INITIALIZE text-input
               move userid to text-input
               perform until text-input = "EXIT" or text-input = "exit"
               call '../../Utility Functions/bin/numberCheck' USING
               by REFERENCE text-input,statusCode
               if statusCode equal "00"
                   move userid to edit-ID
                   call '../SubFiles/bin/adminUpdate'
                   using by REFERENCE edit-id,statusCode
                  EVALUATE statusCode
                   when equal "00"
                       DISPLAY esc GREENx
                       DISPLAY "**********************************"
                       DISPLAY "* Updated Info for ID ("userid") *"
                       DISPLAY "**********************************"
                       DISPLAY esc RESETx
                       perform update-info-page
                   when equal "96"
                       DISPLAY esc REDX
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY "! NOT FOUND ADMIN WITH ID ("userid") !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY esc RESETx
                       perform update-info-page
                   when OTHER
                       DISPLAY esc REDx
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY "! CANNOT PERFORM UPDATE PROCESS !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY esc RESETx
                       perform update-info-page
               END-EVALUATE
               Else
                   DISPLAY esc REDx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "! Invalid Input Type !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc RESETx
                   PERFORM update-info-page
               END-IF
               END-PERFORM
               DISPLAY color-pink
               DISPLAY "+++++++++++++++++++++++++++++"
               DISPLAY "+ Going Back To Main Screen +"
               DISPLAY "+++++++++++++++++++++++++++++"
               DISPLAY ESC RESETX
               perform home-page
           end-if
           end-if.


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
                               display ESC GREENx
                               DISPLAY "*********************"
                               DISPLAY "* LOG IN SUCCESSFUL *"
                               DISPLAY "*********************"
                               DISPLAY ESC RESETx
                               perform home-page
                           when equal "96"
                               DISPLAY ESC REDx
                               DISPLAY "!!!!!!!!!!!!!!!!!!!"
                               DISPLAY "! ADMIN NOT FOUND !"
                               DISPLAY "!!!!!!!!!!!!!!!!!!!"
                               DISPLAY esc RESETx
                               perform login-page
                           when equal "95"
                               DISPLAY esc REDx
                               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!"
                               DISPLAY "! INVALID CREDENTIAL !"
                               DISPLAY "!!!!!!!!!!!!!!!!!!!!!!"
                               DISPLAY esc RESETx
                               perform login-page
                       END-EVALUATE
                   when OTHER
                       DISPLAY esc REDx
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY "! INVALID OPTION CODE !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                       DISPLAY esc RESETx
                       perform login-page
               END-EVALUATE
           END-PERFORM
           DISPLAY esc color-pink
           DISPLAY "++++++++++++++++++++++++++++"
           DISPLAY "+ Exitting the Program ... +"
           DISPLAY "++++++++++++++++++++++++++++"
           DISPLAY esc RESETx
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
                       call '../SubFiles/bin/userList'
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
                           call '../SubFiles/bin/adminList'

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
                       display esc REDx
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                       display "! INVALID OPTION CODE !"
                       DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                       display esc RESETx
                       perform home-page
               END-EVALUATE
           END-PERFORM
           DISPLAY color-pink
           display "++++++++++++++++++"
           display "+ Logging Out... +"
           display "++++++++++++++++++"
           DISPLAY esc RESETx
           perform login-page.
       END PROGRAM AMain.
