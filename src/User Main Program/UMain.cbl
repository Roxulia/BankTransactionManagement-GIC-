       IDENTIFICATION DIVISION.
       PROGRAM-ID. UMain.
       AUTHOR. YOUR-NAME.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UID     PIC 9(5) value 1.
       01 STATUSCODE PIC X(2) VALUE ZEROS.
       01 OPTION     PIC 9(1) VALUE ZEROS.
       01 loginOpt pic 9(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           DISPLAY "================ USER LOGIN ===================".
      *      CALL 'USERLOGIN' USING UID STATUSCODE.
             PERFORM MAIN-MENU
           Stop run.

       login-page.
           DISPLAY "==============================================="
           DISPLAY "=======Bank Transaction Management(USER)======"
           DISPLAY "==============================================="
           DISPLAY "=                1.Login                      ="
           DISPLAY "=               99.Exit                       ="
           DISPLAY "==============================================="
           perform choice-opt-login.

      *     IF STATUSCODE NOT EQUAL "00"
      *         DISPLAY "Login failed. Exiting..."
      *         GO TO END-PROGRAM
      *     END-IF.

           DISPLAY "Login successful!".

       MAIN-MENU.
           DISPLAY SPACE
           DISPLAY "****************  Welcome **********************"
           DISPLAY SPACE
           call '../SubFiles/bin/balanceInfo' using UID
           DISPLAY "===== USER MENU =====".
           DISPLAY "1. Update Password".
           DISPLAY "2. Check Balance".
           DISPLAY "3. Make Transaction".
           DISPLAY "4. View Passbook Report".
           DISPLAY "5. Exit".
           DISPLAY "====================="
      *     DISPLAY "Enter your option: " .
           perform choice-opt-home

           GO TO MAIN-MENU.
           STOP RUN.
       END-PROGRAM.
           STOP RUN.
       *> ===================================================
       *> Sub-menu for Transactions
       *> ===================================================

       choice-opt-login.
           DISPLAY "Choosen Option Code : "
           accept loginOpt
           display SPACE

           PERFORM UNTIL loginOpt = 99
               EVALUATE loginOpt
                   when EQUAL 1
                       if statusCode = "00" THEN
                           PERFORM MAIN-MENU
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
           DISPLAY SPACE
           Display "Enter your option code:"
           ACCEPT OPTION
            EVALUATE OPTION
               WHEN 1
     **              CALL 'UPDATEPASSWORD' USING UID
                        PERFORM MAIN-MENU
               WHEN 2
      **               CALL 'BALANCEINFO' USING UID
                        PERFORM MAIN-MENU
               WHEN 3
                       PERFORM TRANSACTION-MENU
                        PERFORM MAIN-MENU
               WHEN 4
     **                CALL 'GENERATEREPORT' USING UID
                        PERFORM MAIN-MENU
               WHEN 5
                   DISPLAY "Exiting User Menu..."
                   GO TO END-PROGRAM
               WHEN OTHER
                   DISPLAY "Invalid Option. Try Again."
                   DISPLAY SPACE
           END-EVALUATE.

           GO TO MAIN-MENU.
           STOP RUN.

       TRANSACTION-MENU.
           DISPLAY SPACE
           DISPLAY "===== TRANSACTION MENU =====".
           DISPLAY "1. Withdraw".
           DISPLAY "2. Transfer".
           DISPLAY "Enter your choice: ".
           ACCEPT OPTION

           EVALUATE OPTION
               WHEN 1
               display "fu"
     **             CALL 'TRXWITHDRAW' USING UID
               WHEN 2
                   display "fu"
  *****            CALL 'TRXTRANSFER' USING UID
               WHEN OTHER
                   DISPLAY "Invalid Option."
           END-EVALUATE.
       END-PARAGRAPH.
       end PROGRAM UMain.
