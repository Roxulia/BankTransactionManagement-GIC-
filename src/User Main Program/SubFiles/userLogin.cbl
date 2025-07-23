      ******************************************************************
      * Author: Nyan Ye Thu
      * Date:   7/7/2025
      * Purpose: User can login to Bank system.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. userLogin.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  loginname pic x(25).
       01  password pic x(20).
       01  enc_password pic x(32).
       01  ws-fs pic x(2).
       01  optcode pic 9.
       01  eof pic x value 'n'.
       01  userdata.
           05  UID        PIC 9(5).
           05  UName      PIC X(20).
           05  ULoginName PIC X(25).
           05  UEncPsw    PIC X(32).
           05  UAddress   PIC X(20).
           05  UPh        PIC X(9).
           05  Balance    PIC 9(10)V99.
           05  trxCount    pic 9(5).
           05  UDate      PIC 9(8).
           05  UTime      PIC 9(6).
       LINKAGE SECTION.
       01  username pic x(20).
       01  statusCode pic x(2).
       01  adminId pic 9(5).
       PROCEDURE DIVISION using adminId,username,statusCode.
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
           call '../../Utility Functions/bin/getUserByLoginName'
           using REFERENCE loginname,userdata,statusCode

           if statusCode EQUAL "00"
               if uEncPsw = enc_password THEN
                   move "00" to statusCode
                   move uID to adminId
                   move uName to username
               ELSE
                   move "95" to statusCode
                   move SPACE to username
               END-IF
           END-IF
           exit PROGRAM.

       END PROGRAM userLogin.
