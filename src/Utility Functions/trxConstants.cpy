       *>min values and max values for deposit

       01  minAmoDepo          PIC 9(10)   VALUE 10000.
       01  maxAmoDepo          PIC 9(10)   VALUE 2000000.
       01  WS-TrxSentPrefix    PIC X       VALUE 'S'.
       01  WS-TrxDepoPrefix    PIC X       VALUE 'D'.
       01  WS-TrxWDPrefix      PIC X       VALUE 'W'.
       01  WS-TrxReciPrefix    PIC X       VALUE 'R'.
       01  WS-SentDescript     PIC x(20)   VALUE 'FUND TRANSFER SENT'.
       01  WS-DepoDescript     PIC x(20)   VALUE 'Admin Deposit'.
