       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMNAME.
       AUTHOR. Bard.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       *--- COPYBOOK FOR INPUT/OUTPUT ---*
       01  WS-INPUT-AREA.
           05 WS-CUST-CTRY          PIC X(03).
           05 WS-CUST-INSTT         PIC X(05).
           05 WS-CUST-ID            PIC X(10).
           05 WS-INSTR-REF-NUM     PIC X(12).
           05 WS-AC-CTRY           PIC X(03).
           05 WS-AC-INSTT          PIC X(05).
           05 WS-AC-NO             PIC X(15).
           05 WS-AC-PROD-TYP      PIC X(02).

       *--- COPYBOOK FOR DATABASE RECORD ---*
       01  PAYMENT-INSTR-REC.
           05 PAY-CUST-CTRY        PIC X(03).
           05 PAY-CUST-INSTT       PIC X(05).
           05 PAY-CUST-ID          PIC X(10).
           05 PAY-INSTR-REF-NUM   PIC X(12).
           05 PAY-AC-CTRY         PIC X(03).
           05 PAY-AC-INSTT        PIC X(05).
           05 PAY-AC-NO           PIC X(15).
           05 PAY-AC-PROD-TYP    PIC X(02).
           05 PAY-BENE-NAM        PIC X(50).
           05 PAY-BENE-ADDR1       PIC X(50).
           05 PAY-BENE-ADDR2       PIC X(50).
           05 PAY-BENE-ADDR3       PIC X(50).
           05 PAY-DEBTOR-NAME      PIC X(50).
           05 PAY-DEBTOR-ADDR1     PIC X(50).
           05 PAY-DEBTOR-ADDR2     PIC X(50).
           05 PAY-DEBTOR-ADDR3     PIC X(50).
           05 PAY-INSTR-PTY      PIC X(50).

       *--- ARRAY TO STORE PAYMENT INSTRUCTIONS ---*
       01  PAYMENT-TABLE.
           05  PAYMENT-ENTRY OCCURS 100 TIMES.
               10  PAY-TBL-CUST-CTRY        PIC X(03).
               10  PAY-TBL-CUST-INSTT       PIC X(05).
               10  PAY-TBL-CUST-ID          PIC X(10).
               10  PAY-TBL-INSTR-REF-NUM   PIC X(12).
               10  PAY-TBL-AC-CTRY         PIC X(03).
               10  PAY-TBL-AC-INSTT        PIC X(05).
               10  PAY-TBL-AC-NO           PIC X(15).
               10  PAY-TBL-AC-PROD-TYP    PIC X(02).
               10  PAY-TBL-BENE-NAM        PIC X(50).
               10  PAY-TBL-BENE-ADDR1       PIC X(50).
               10  PAY-TBL-BENE-ADDR2       PIC X(50).
               10  PAY-TBL-BENE-ADDR3       PIC X(50).
               10  PAY-TBL-DEBTOR-NAME      PIC X(50).
               10  PAY-TBL-DEBTOR-ADDR1     PIC X(50).
               10  PAY-TBL-DEBTOR-ADDR2     PIC X(50).
               10  PAY-TBL-DEBTOR-ADDR3     PIC X(50).
               10  PAY-TBL-INSTR-PTY      PIC X(50).

       01  WS-POINTER            PIC S9(04) COMP VALUE +1.
       01  WS-SQLCODE            PIC S9(09) COMP-3 VALUE +0.
       01  WS-MESSAGE           PIC X(80) VALUE SPACES.

       LINKAGE SECTION.
       01  DFHCOMMAREA          PIC X(255).

       PROCEDURE DIVISION.
       MAIN SECTION.

           *--- STEP 1: RECEIVE INPUT FROM DFHCOMMAREA ---*
           MOVE DFHCOMMAREA TO WS-INPUT-AREA.

           *--- STEP 2: GETMAIN FOR INPUT COPYBOOK ---*
           EXEC CICS GETMAIN SET(ADDRESS OF WS-INPUT-AREA)
                             LENGTH(LENGTH OF WS-INPUT-AREA)
           END-EXEC.

           *--- STEP 3: INPUT VALIDATION ---*
           PERFORM VALIDATE-INPUT.

           *--- STEP 4: FETCH DATA FROM DATABASE ---*
           EXEC SQL
               DECLARE PAYMENT-CURSOR CURSOR FOR
               SELECT CUST_CTRY, 
                      CUST_INSTT, 
                      CUST_ID, 
                      INSTR_REF_NUM, 
                      AC_CTRY, 
                      AC_INSTT, 
                      AC_NO, 
                      AC_PROD_TYP, 
                      BENE_NAM, 
                      BENE_ADDR1,
                      BENE_ADDR2, 
                      BENE_ADDR3, 
                      DEBTOR_NAME,
                      DEBTOR_ADDR1, 
                      DEBTOR_ADDR2, 
                      DEBTOR_ADDR3, 
                      INSTR_PTY 
               FROM PAYMENTS.PAYMENT_INSTRUCTIONS 
               WHERE CUST_CTRY = :WS-CUST-CTRY 
                 AND CUST_INSTT = :WS-CUST-INSTT 
                 AND CUST_ID = :WS-CUST-ID 
                 AND INSTR_REF_NUM = :WS-INSTR-REF-NUM
                 AND AC_CTRY = :WS-AC-CTRY 
                 AND AC_INSTT = :WS-AC-INSTT
                 AND AC_NO = :WS-AC-NO 
                 AND AC_PROD_TYP = :WS-AC-PROD-TYP
               FOR UPDATE OF PAY-INSTR-PTY
           END-EXEC.

           *--- STEP 5: OPEN CURSOR AND FETCH DATA INTO ARRAY ---*
           EXEC SQL OPEN PAYMENT-CURSOR END-EXEC.
           PERFORM FETCH-PAYMENT-DATA UNTIL SQLCODE NOT = 0.
           EXEC SQL CLOSE PAYMENT-CURSOR END-EXEC.

           *--- STEP 6: PROCESS THE PAYMENT TABLE (Example: Display) ---*
           PERFORM VARYING WS-POINTER FROM 1 BY 1 UNTIL WS-POINTER > 100
               DISPLAY 'Payment Record:' WS-POINTER
               DISPLAY PAYMENT-ENTRY(WS-POINTER)
           END-PERFORM.

           *--- STEP 7: RELEASE STORAGE AND EXIT ---*
           EXEC CICS FREEMAIN DATA(ADDRESS OF WS-INPUT-AREA) END-EXEC.
           EXEC CICS RETURN END-EXEC.

       ****---- INTERNAL SUBROUTINES ----****
       VALIDATE-INPUT SECTION.
           IF WS-CUST-CTRY = SPACES OR LOW-VALUES
              MOVE 'Invalid Customer Country' TO WS-MESSAGE
              PERFORM ERROR-ROUTINE
           END-IF.
           *--- Repeat for other fields in WS-INPUT-AREA ---*
       .

       FETCH-PAYMENT-DATA SECTION.
           EXEC SQL FETCH PAYMENT-CURSOR INTO 
                        :PAY-CUST-CTRY,
                        :PAY-CUST-INSTT,
                        :PAY-CUST-ID,
                        :PAY-INSTR-REF-NUM,
                        :PAY-AC-CTRY,
                        :PAY-AC-INSTT,
                        :PAY-AC-NO,
                        :PAY-AC-PROD-TYP,
                        :PAY-BENE-NAM,
                        :PAY-BENE-ADDR1,
                        :PAY-BENE-ADDR2,
                        :PAY-BENE-ADDR3,
                        :PAY-DEBTOR-NAME,
                        :PAY-DEBTOR-ADDR1,
                        :PAY-DEBTOR-ADDR2,
                        :PAY-DEBTOR-ADDR3,
                        :PAY-INSTR-PTY
           END-EXEC.

           IF SQLCODE = 0
              MOVE PAY-CUST-CTRY TO PAY-TBL-CUST-CTRY(WS-POINTER)
              MOVE PAY-CUST-INSTT TO PAY-TBL-CUST-INSTT(WS-POINTER)
              *--- Move remaining fields from PAYMENT-INSTR-REC to PAYMENT-TABLE ---*
              ADD +1 TO WS-POINTER
           END-IF.
       .

       ERROR-ROUTINE SECTION.
           DISPLAY 'ERROR: ' WS-MESSAGE
           EXEC CICS ABEND ABCODE('ERR1') END-EXEC
       .
       END PROGRAM PGMNAME.