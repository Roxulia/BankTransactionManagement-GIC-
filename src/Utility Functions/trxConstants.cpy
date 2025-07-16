       *>min values and max values for deposit

       01  minAmoDepo          PIC 9(10)v99   VALUE 10000.
       01  maxAmoDepo          PIC 9(10)v99   VALUE 2000000.
       01  minWithdraw         pic 9(10)v99   value 1000.
       01  minaccountbalance   pic 9(10)v99   value 1000.
       01  maxWithdraw         pic 9(10)v99   value 10000000.
       01  WS-TrxSentPrefix    PIC X       VALUE 'S'.
       01  WS-TrxDepoPrefix    PIC X       VALUE 'D'.
       01  WS-TrxWDPrefix      PIC X       VALUE 'W'.
       01  WS-TrxReciPrefix    PIC X       VALUE 'R'.
       01  payday              pic 99      value 16.
       01  paytime             pic 9(6)    value 120000.
