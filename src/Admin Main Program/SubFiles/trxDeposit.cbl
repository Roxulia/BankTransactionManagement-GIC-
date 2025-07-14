      ******************************************************************
      * Author:Myo Thein Chit
      * Date:7-11-2025
      * Purpose:making deposits for a user from admin side
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. trxDeposit.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.
           SELECT UserFile ASSIGN TO '../../../data/UserAccounts.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS UID
               FILE STATUS IS WS-FS.

           SELECT TrxFile ASSIGN TO '../../../data/Transactions.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TrxID
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  UserFile.
       01  UserRecord.
           05  UID         PIC 9(5).
           05  UName       PIC X(20).
           05  ULoginName  PIC X(25).
           05  UEncPsw     PIC X(32).
           05  UAddress    PIC X(20).
           05  UPh         PIC X(9).
           05  Balance     PIC 9(10)V99.
           05  TrxCount    PIC 9(5).
           05  UDate       PIC 9(6).
           05  UTime       PIC 9(6).

       FD  TrxFile.
       01  TransactionRecord.
           05  TrxID       PIC 9(10).
           05  ReceiverID  PIC 9(5).
           05  Description PIC X(30).
           05  Amount      PIC 9(10).
           05  TrxType     PIC 9.  *>  1 = SENT
                                   *>  2 = RECEIVED
                                   *>  3 = DEPOSIT
                                   *>  4 = WITHDRAW
           05  TimeStamp   PIC 9(16).

       WORKING-STORAGE SECTION.

       *>For display colors
       COPY "../../Utility Functions/colorCodes.cpy".

       01  WS-FS           PIC XX.
       01  depoAmo         PIC 9(10).

       01  CurrentDate     PIC 9(6).
       01  CurrentTime     PIC 9(6).

       *>For trxConstant VALUES
       *>01  minAmoDepo      PIC 9(10) VALUE 10000.
       *>01  maxAmoDepo      PIC 9(10) VALUE 2000000.
       COPY "../../Utility Functions/trxConstants.cpy".

       LINKAGE section.

       01  userId          PIC 9(5).
       01  optStatus       PIC 9(1).

       PROCEDURE DIVISION using REFERENCE userId,optStatus.
       Main-Section.
           PERFORM RECORD-POINTER
           PERFORM TRXID-GENERATE
           PERFORM AMOUNT-VALID-PROMPT-BOX
           PERFORM WRITE-TRX
           PERFORM BALANCE-UPDATE
           GOBACK.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Find the designated user record from the user file .
       RECORD-POINTER.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Generate a unique trxid ( TrxCount+1) + S,R,D,W + SenderID
       *> Sent, received , deposit , withdraw , D here since this sub file is for deposit
       TRXID-GENERATE.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Prompt box display for accepting amount deposit and validate Amount
       *>loop the amount prompt box, until the amount is valid or entered 'EXIT'
       AMOUNT-VALID-PROMPT-BOX.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Write a new record in TrxFile
       WRITE-TRX.

       *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
       *>Update the balance in UserRecord and increment the TrxCount
       BALANCE-UPDATE.
