******************************************************************
      * Author: Sat Paing Thu
      * Date: 4.7.2025
      * Purpose: Test Program for userInfo
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestUserInfo.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-UID          PIC X(25).
       01 WS-UserData.
           05 WS-RET-UID      PIC 9(5).
           05 WS-RET-UName    PIC X(20).
           05 WS-RET-ULoginName PIC X(25).
           05 RET-UEncodedPassword PIC X(32).
           05 WS-RET-UAddress PIC X(20).
           05 RET-UPhone PIC 9(9).
           05 RET-UBalance PIC 9(10)V99.
           05 RET-UDate PIC 9(8).
           05 RET-UTime PIC 9(6).
           05 WS-RET-Found    PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-UID = 0
           DISPLAY "Enter User ID to search: "
           ACCEPT WS-UID
           IF WS-UID NOT = 0
           CALL "getUser" USING WS-UID, WS-UserData

           IF WS-RET-Found = "Y"
               DISPLAY "User found!"
               DISPLAY "-------------------------"
               DISPLAY "UID: " WS-RET-UID
               DISPLAY "Name: " WS-RET-UName
               DISPLAY "Login Name: " WS-RET-ULoginName
               DISPLAY "Encoded Password:"RET-UEncodedPassword
               DISPLAY "ADDRESS: " WS-RET-UAddress
               DISPLAY "Phone Number:  " RET-UPhone
               DISPLAY "Balance:  " RET-UBalance
               DISPLAY "Date:  " RET-UDate
               DISPLAY "Time:  " RET-UTime
               DISPLAY "-----------------------------------------"

           ELSE
               DISPLAY "User not found!"

           END-IF
           END-IF
           END-PERFORM
           STOP RUN.
       END PROGRAM TestUserInfo.
