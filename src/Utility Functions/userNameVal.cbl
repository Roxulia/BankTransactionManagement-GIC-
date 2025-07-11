       IDENTIFICATION DIVISION.
       PROGRAM-ID. userNameVal.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ERROR-FLAG    PIC X VALUE 'N'.
           88 NO-ERROR      VALUE 'N'.
           88 HAS-ERROR     VALUE 'Y'.


       LINKAGE SECTION.
       01  LS-USERNAME     PIC X(20).
       01  LS-RETURN-CODE  PIC x(2).
           88 VALID-INPUT    VALUE 0.
           88 EMPTY-USER     VALUE 1.

       PROCEDURE DIVISION USING LS-USERNAME,
       LS-RETURN-CODE.
       MAIN-PARA.
           PERFORM INIT-VALIDATION
           PERFORM CHECK-EMPTY-USERNAME
           GOBACK.

       INIT-VALIDATION.
           MOVE "00" TO LS-RETURN-CODE
           SET NO-ERROR TO TRUE.

       CHECK-EMPTY-USERNAME.
           IF LS-USERNAME = SPACES
              MOVE "01" TO LS-RETURN-CODE
              SET HAS-ERROR TO TRUE
           END-IF.
