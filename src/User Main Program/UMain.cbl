       IDENTIFICATION DIVISION.
       PROGRAM-ID. UMain.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UID     PIC 9(5).

       01  username pic x(20).
       01 STATUSCODE PIC X(2) VALUE ZEROS.
       01 OPTION     PIC x(1) VALUE ZEROS.
       01 loginOpt pic x(2).

       COPY "../Utility Functions/colorCodes.cpy".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE uid

      *      CALL 'USERLOGIN' USING UID STATUSCODE.
           PERFORM login-page
           Stop run.

       login-page.
           DISPLAY color-pink
           "================ USER LOGIN ==================="
           DISPLAY "==============================================="
           DISPLAY "=======Bank Transaction Management(USER)======="
           DISPLAY "==============================================="
           DISPLAY "=                1.Login                      ="
           DISPLAY "=               99.Exit                       ="
           DISPLAY "==============================================="
           DISPLAY esc resetx
           perform choice-opt-login.


       MAIN-MENU.
           DISPLAY SPACE
           display esc greenx
           DISPLAY "****************  Welcome " color-blue
                   FUNCTION TRIM(username) esc greenx
                   "**********************"
           DISPLAY SPACE

           call '../SubFiles/bin/balanceInfo' using UID
           display esc resetx
           display  color-yellow
           DISPLAY "===== USER MENU =====".
           DISPLAY "1. Update Password".
           DISPLAY "2. User Profile".
           DISPLAY "3. Make Transaction".
           DISPLAY "4. View Passbook Report".
           DISPLAY "5. Exit".
           DISPLAY "====================="
           display esc resetx
      *     DISPLAY "Enter your option: " .
           perform choice-opt-home

           STOP RUN.
       END-PROGRAM.
           STOP RUN.
       *> ===================================================
       *> Sub-menu for Transactions
       *> ===================================================

       choice-opt-login.
           DISPLAY "Choosen Option Code : "
           accept loginOpt

           PERFORM UNTIL loginOpt EQUAL "99"
               EVALUATE loginOpt
                   when EQUAL "1"
                       call '../SubFiles/bin/userLogin'
                       using by REFERENCE uid , username , STATUSCODE
                       EVALUATE STATUSCODE
                       when EQUAL "00"

                           DISPLAY esc greenx
                           DISPLAY "***********************"
                           DISPLAY "* Login Successful... *"
                           DISPLAY "***********************"
                           DISPLAY esc resetx
                           PERFORM MAIN-MENU
                       when equal "95"
                           DISPLAY esc redx
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY "! Invalid Credentials !"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY esc resetx
                           PERFORM login-page
                       when EQUAL "96"

                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY "!    User Not Found   !"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           PERFORM login-page
                       when EQUAL "99"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY "!     Error Occurs    !"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           PERFORM login-page
                       END-EVALUATE
                   when OTHER
                       DISPLAY esc redx
                       DISPLAY "INVALID OPTION CODE"
                       DISPLAY esc resetx
                       perform login-page
               END-EVALUATE
           END-PERFORM
           DISPLAY esc redx
           DISPLAY "==========================="
           DISPLAY "= Exitting the Program ...."
           DISPLAY "==========================="
           DISPLAY esc resetx
           stop run.


       choice-opt-home.
           DISPLAY SPACE
           Display "Enter your option code :"
           ACCEPT OPTION
            EVALUATE OPTION
               WHEN "1"
                   CALL '../SubFiles/bin/updatePassword'
                   USING by REFERENCE UID STATUSCODE
                   EVALUATE STATUSCODE
                        when EQUAL "00"
                            DISPLAY esc greenx
                            DISPLAY "PASSWORD UPDATED SUCCESSFULLY"
                            DISPLAY esc resetx
                            PERFORM MAIN-MENU
                        when EQUAL "95"
                           DISPLAY esc redx
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY "! Invalid Credentials !"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY esc resetx
                           perform MAIN-MENU
                        when EQUAL "99"
                           DISPLAY esc redx
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                           display "!Error occurs in Updating password!"
                           DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                           DISPLAY esc resetx
                           perform MAIN-MENU
                   END-EVALUATE
               WHEN "2"
                        CALL '../SubFiles/bin/userInfo' USING UID
                        PERFORM MAIN-MENU
               WHEN "3"
                       PERFORM TRANSACTION-MENU
                        PERFORM MAIN-MENU
               WHEN "4"
                     CALL '../../Utility Functions/bin/generateReport'
                     USING UID
                        PERFORM MAIN-MENU
               WHEN "5"
                   DISPLAY esc redx
                   DISPLAY "Exiting User Menu..."
                   DISPLAY esc resetx
                   perform login-page
               WHEN OTHER
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "! Invalid Option. Try Again !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
           END-EVALUATE.

           GO TO MAIN-MENU.
           STOP RUN.

       TRANSACTION-MENU.
           DISPLAY SPACE
           DISPLAY color-blue "===== TRANSACTION MENU ====="
           display        esc resetx
           DISPLAY "1. Withdraw".
           DISPLAY "2. Transfer".
           DISPLAY "Enter your choice: ".
           ACCEPT OPTION

           EVALUATE OPTION
               WHEN "1"
                   CALL '../SubFiles/bin/trxWithdraw' USING
                   by REFERENCE UID
               WHEN "2"
                   call '../SubFiles/bin/trxTransfer'
                   using by REFERENCE uid STATUSCODE
               WHEN OTHER
                   DISPLAY esc redx
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY "! Invalid Option. Try Again !"
                   DISPLAY "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                   DISPLAY esc resetx
           END-EVALUATE.
       END-PARAGRAPH.
       end PROGRAM UMain.
