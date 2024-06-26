       *****************************************************************
       * DCLGEN TABLE(TABACCNT)
       *         LIBRARY(RISHI.PDS)
       *         ACTION(REPLACE)
       *           LANGUAGE(COBOL)
       *         QUOTE
       *....IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS
       *****************************************************************

           EXEC SQL 
           DECLARE TABACCNT TABLE
           (
               ACC_CTRY                CHAR(2) NOT NULL,
               ACC_INSTT                CHAR(4) NOT NULL,
               ACC_NO                    CHAR(35) NOT NULL,
               ACC_PRODU_TYP            CHAR(20) NOT NULL,
               MST_PND_COD                CHAR(10) NOT NULL,
               ACC_NUM_FMTD                CHAR(35) NOT NULL,
               ACC_CCY                    CHAR(3) NOT NULL,
               ACC_STAT                    CHAR(1) NOT NULL,
               CREAT_TS                    TIMESTAMP NOT NULL,
               LAST_UPDT_TS                TIMESTAMP NOT NULL,
               LST_UPDT_ID                CHAR(20) NOT NULL
           )
           END-EXEC.
       *****************************************************************
       * COBOL DECLARATION FOR TABLE TABACCNT
       *****************************************************************
       01 TABACCNT-WORK.
         05 TABACCNT-WORK-REC.
             10 TABACCNT-ACC-CTRY      PIC X(2).
             10 TABACCNT-ACC-INSTT     PIC X(4).
             10 TABACCNT-ACC-NO        PIC X(35).
             10 TABACCNT-ACC-PRODU-TYP PIC X(20).
             10 TABACCNT-MST-PND-COD   PIC X(10).
             10 TABACCNT-ACC-NUM-FMTD  PIC X(35).
             10 TABACCNT-ACC-CCY       PIC X(3).
             10 TABACCNT-ACC-STAT      PIC X(1).
             10 TABACCNT-CREAT-TS      PIC X(26).
             10 TABACCNT-LAST-UPDT-TS  PIC X(26).                             
             10 TABACCNT-LST-UPDT-ID   PIC X(20).
       *****************************************************************
       * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11
       *****************************************************************