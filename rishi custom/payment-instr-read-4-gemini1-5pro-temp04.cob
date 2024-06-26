       IDENTIFICATION DIVISION.
       PROGRAM-ID. FETCH-PAYMENT-INSTRUCTIONS.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * COPY DFHCOMMAREA AND COPYBOOK
       COPY DFHCOMMAREA.
       COPY PAYMENT-INSTRUCTIONS-COPYBOOK.

       * VARIABLES FOR GETMAIN
       01  WS-PAYMENT-INSTRUCTIONS-PTR  POINTER.
       01  WS-PAYMENT-INSTRUCTIONS-LEN  PIC S9(9) COMP.

       * VARIABLES FOR SQL
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.
       01  WS-DB2-ERROR-MSG            PIC X(100).
       01  WS-SQLCODE                  PIC S9(9) COMP.
       01  WS-SQLSTATE                 PIC X(5).

       * CURSOR DEFINITION
       EXEC SQL
           DECLARE PAYMENT-INSTRUCTIONS-CURSOR CURSOR WITH HOLD FOR
               SELECT CUST_CTRY, CUST_INSTT, CUST_ID, INSTR_REF_NUM,
                      AC_CTRY, AC_INSTT, AC_NO, AC_PROD_TYP,
                      BENE_NAM, BENE_ADDR1, BENE_ADDR2, BENE_ADDR3,
                      DEBTOR_NAME, DEBTOR_ADDR1, DEBTOR_ADDR2, DEBTOR_ADDR3,
                      INSTR_PTY
               FROM PAYMENTS.PAYMENT_INSTRUCTIONS
               WHERE CUST_CTRY = :WS-PAYMENT-INSTRUCTIONS.CUST-CTRY
                 AND CUST_INSTT = :WS-PAYMENT-INSTRUCTIONS.CUST-INSTT
                 AND CUST_ID = :WS-PAYMENT-INSTRUCTIONS.CUST-ID
                 AND INSTR_REF_NUM = :WS-PAYMENT-INSTRUCTIONS.INSTR-REF-NUM
                 AND AC_CTRY = :WS-PAYMENT-INSTRUCTIONS.AC-CTRY
                 AND AC_INSTT = :WS-PAYMENT-INSTRUCTIONS.AC-INSTT
                 AND AC_NO = :WS-PAYMENT-INSTRUCTIONS.AC-NO
                 AND AC_PROD_TYP = :WS-PAYMENT-INSTRUCTIONS.AC-PROD-TYP
       END-EXEC.

       * ARRAY TO HOLD PAYMENT INSTRUCTIONS
       01  PAYMENT-INSTRUCTIONS-TABLE.
           05  PAYMENT-INSTRUCTIONS-ENTRY OCCURS 100 TIMES.
               10  PI-CUST-CTRY          PIC X(3).
               10  PI-CUST-INSTT         PIC X(10).
               10  PI-CUST-ID           PIC X(20).
               10  PI-INSTR-REF-NUM     PIC X(20).
               10  PI-AC-CTRY           PIC X(3).
               10  PI-AC-INSTT          PIC X(10).
               10  PI-AC-NO            PIC X(20).
               10  PI-AC-PROD-TYP       PIC X(10).
               10  PI-BENE-NAM          PIC X(100).
               10  PI-BENE-ADDR1         PIC X(100).
               10  PI-BENE-ADDR2         PIC X(100).
               10  PI-BENE-ADDR3         PIC X(100).
               10  PI-DEBTOR-NAME       PIC X(100).
               10  PI-DEBTOR-ADDR1      PIC X(100).
               10  PI-DEBTOR-ADDR2      PIC X(100).
               10  PI-DEBTOR-ADDR3      PIC X(100).
               10  PI-INSTR-PTY        PIC X(10).

       * INDEX FOR ARRAY
       01  WS-TABLE-INDEX              PIC S9(4) COMP VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PARA.
           * GET INPUT FROM DFHCOMMAREA
           EXEC CICS GETMAIN SET(WS-PAYMENT-INSTRUCTIONS-PTR)
                             LENGTH(LENGTH OF PAYMENT-INSTRUCTIONS-COPYBOOK)
           END-EXEC.
           MOVE LENGTH OF PAYMENT-INSTRUCTIONS-COPYBOOK
               TO WS-PAYMENT-INSTRUCTIONS-LEN.
           MOVE DFHCOMMAREA TO WS-PAYMENT-INSTRUCTIONS-PTR.

           * VALIDATE INPUT FIELDS
           PERFORM VALIDATE-INPUT-FIELDS.

           * FETCH PAYMENT INSTRUCTIONS FROM DATABASE
           PERFORM FETCH-PAYMENT-INSTRUCTIONS.

           * PROCESS PAYMENT INSTRUCTIONS (Not implemented here)

           * FREE THE STORAGE ALLOCATED BY GETMAIN
           EXEC CICS FREEMAIN DATA(WS-PAYMENT-INSTRUCTIONS-PTR)
           END-EXEC.

           EXEC CICS RETURN END-EXEC.

       VALIDATE-INPUT-FIELDS.
           IF WS-PAYMENT-INSTRUCTIONS.CUST-CTRY = SPACES OR LOW-VALUES THEN
               * HANDLE ERROR - CUST-CTRY IS INVALID
           END-IF.
           * SIMILARLY VALIDATE OTHER FIELDS
           ...

       FETCH-PAYMENT-INSTRUCTIONS.
           EXEC SQL
               OPEN PAYMENT-INSTRUCTIONS-CURSOR
           END-EXEC.
           IF SQLCODE NOT = 0 THEN
               MOVE 'ERROR OPENING CURSOR' TO WS-DB2-ERROR-MSG
               * HANDLE ERROR - COULD NOT OPEN CURSOR
           END-IF.

           PERFORM RETRIEVE-PAYMENT-INSTRUCTIONS UNTIL SQLCODE NOT = 0.

           EXEC SQL
               CLOSE PAYMENT-INSTRUCTIONS-CURSOR
           END-EXEC.
           IF SQLCODE NOT = 0 THEN
               MOVE 'ERROR CLOSING CURSOR' TO WS-DB2-ERROR-MSG
               * HANDLE ERROR - COULD NOT CLOSE CURSOR
           END-IF.

       RETRIEVE-PAYMENT-INSTRUCTIONS.
           EXEC SQL
               FETCH PAYMENT-INSTRUCTIONS-CURSOR INTO
                   :PI-CUST-CTRY, :PI-CUST-INSTT, :PI-CUST-ID,
                   :PI-INSTR-REF-NUM, :PI-AC-CTRY, :PI-AC-INSTT,
                   :PI-AC-NO, :PI-AC-PROD-TYP, :PI-BENE-NAM,
                   :PI-BENE-ADDR1, :PI-BENE-ADDR2, :PI-BENE-ADDR3,
                   :PI-DEBTOR-NAME, :PI-DEBTOR-ADDR1, :PI-DEBTOR-ADDR2,
                   :PI-DEBTOR-ADDR3, :PI-INSTR-PTY
           END-EXEC.

           IF SQLCODE = 0 THEN
               ADD 1 TO WS-TABLE-INDEX
               IF WS-TABLE-INDEX > 100 THEN
                   * HANDLE ERROR - ARRAY OVERFLOW
               END-IF.
           END-IF.
       END PROGRAM.