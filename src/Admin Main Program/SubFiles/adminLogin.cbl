      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adminLogin.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AdminFile
               ASSIGN TO '../../../data/AdminAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS AID
               FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       fd  AdminFile.
       01  admin.

       COPY "../../Utility Functions/adminFile.cpy".

       WORKING-STORAGE SECTION.
       01  loginname pic x(25).
       01  password pic x(20).
       01  enc_password pic x(32).
       01  ws-fs pic x(2).
       01  optcode pic 9.
       01  eof pic x value 'n'.
       LINKAGE SECTION.
       01  username pic x(20).
       01  arole pic 9.
       01  statusCode pic x(2).
       01  adminId pic 9(5).
       PROCEDURE DIVISION using adminId,username,arole,statusCode.
           INITIALIZE loginname
           INITIALIZE password
           move 'n' to eof
           perform display-prompt thru process_login.

       display-prompt.
           DISPLAY "======================================="
           DISPLAY "=              Login Form             ="
           DISPLAY "======================================="
           display "Enter Login Name : " ACCEPT loginname
           display "Enter Ur Password : " ACCEPT password.

       process_login.
           call '../../Utility Functions/bin/encryption'
           using REFERENCE password , enc_password
           OPEN INPUT AdminFile
           if ws-fs not equal '00' THEN
               move '99' to statusCode
               close AdminFile
               exit program
           end-if
           perform until eof equal 'y'
               read AdminFile into admin

               at end
                   move 'y' to eof
                   move "96" to statusCode
                   move SPACE to username
                   move 0 to arole
               not at end
                   *>display admin
                   if ALoginName = loginname THEN
                       if AEncPsw = enc_password THEN
                           move "00" to statusCode
                           move AID to adminId
                           move AName to username
                           move role to arole
                       ELSE
                           move "95" to statusCode
                           move SPACE to username
                           move Zero to arole
                       END-IF
                       move 'y' to eof
                   END-IF
               END-READ
           END-PERFORM
           close AdminFile
           exit program.

       END PROGRAM adminLogin.
