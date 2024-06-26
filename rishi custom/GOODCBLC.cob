       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOODCBLC.
       AUTHOR. RISHI SHARMA
       DATE-WRITTEN. JUN,2024
       DATE-COMPILED. JUN,2024
       INSTALLATION. Nowhere
       VERSION. v1.0
       *****************************************************************
       * THIS COBOL ROUTINE IS DEVELOPED TO SHOWCASE AI BASED COBOL    *
       * DOCUMENTATION IN BUSINESS DOMAIN LANGUAGE                     *
       *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER. IBM-370.
       SOURCE-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT IBRI         
                                    ASSIGN TO IBRI
                                    ORGANIZATION IS SEQUENTIAL
                                    ACCESS IS SEQUENTIAL
                                    FILE STATUS IS INFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.

       FD IBRI
           LABEL RECORDS STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 4096 CHARACTERS
           BLOCK CONTAINS 0
           DATA RECORD IS IBRI-REC-IN.
       01 IBRI-REC-IN                 PIC X(4096).
       WORKING-STORAGE SECTION.
       77  FILLER                     PIC X(40) VALUE
            'WORKING STORAGE SECTION STARTS HERE'.
       01  WS-MISC.
           05 INFILE-STATUS           PIC X(2).
           05 WS-CANCEL-PGM           PIC X(8) VALUE 'EXITPGM'.
           05 WS-SUBPGM-NAM           PIC X(8).
               88 WS-SUBPGM-LOG       VALUE 'LOGPGMC'.
           05 WS-DEPT-ID              PIC X(8).
               88 IB-DEPT             VALUE 'INETBKNG'.
       *****************************************************************
       *   COPYBOOKS                                                   *
       *****************************************************************
       COPY IBRFDCW.

       *****************************************************************
       *      DB2 TABLE DECLARATION                                    *
       *****************************************************************
           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.
           EXEC SQL 
               INCLUDE TABACCW
           END-EXEC.
           EXEC SQL 
               INCLUDE TABACPFW 
           END-EXEC.
           EXEC SQL 
               INCLUDE TABCSTW 
           END-EXEC.
           EXEC SQL 
               INCLUDE SQLCA 
           END-EXEC.                                            
       *--------------------------------------*
       *      DB2 CURSOR DECLARATION          *
       *--------------------------------------*  
       *****************************************************************
       * SELECT CUSTOMERS WITH ACH PAYMENTS ENABLED ACCOUNTS           *
       *****************************************************************       
       *****************************************************************
       *****************************************************************              
           EXEC SQL 
               DECLARE CSR-AP-ACPF CURSOR WITH HOLD
               SELECT CST_CTRY,
                      CST_INSTT,
                      CST_ID,
                      ACPF_STAT
               FROM   TABACCPF
               WHERE  ACC_CTRY = :TABACPF-AC-CTRY
                AND   ACC_INSTT = :TABACPF-AC-CTRY
                AND   ACC_NO    = :TABACPF-AC-CTRY
                AND   MST_PND_COD = 'Master'
                AND   CST_TYP_COD  = 'Global'
               FOR FETCH ONLY
           END-EXEC.                                      
       77 FILLER                PIC X(30) VALUE
               'END OF WORKING STORAGE SECTION'.
       *****************************************************************
       * LINKAGE SECTION                                               *
       *****************************************************************
       LINKAGE SECTION.
       /
       PROCEDURE DIVISION.
           PERFORM A000-INIT
           PERFORM B000-MAIN
           PERFORM C000-FINAL
           .
       /
       *****************************************************************
       * Initialization                                                *
       *****************************************************************
       A000-INIT.
           INITIALIZE DCLCDE-GEN-REF
                       GIB-RPT-DNLD-HIST
           MOVE SPACES                 TO WS-PREV-IBR-RPT-CTRY
                                          WS-PREV-IBR-RPT-INST
                                          WS-PREV-IBR-RPT-ID
                                          WS-RPT-TYPE-BIN
           MOVE ZEROS                  TO WS-STREAM-NUM-BIN
                                          WS-STREAM-BIN-MAX
                                          WS-STREAM-BIN
           PERFORM A010-WRT-START-MSG
           PERFORM A020-OPEN-IBRI
           .
       *****************************************************************
       * Main Process Routine                                          *
       *****************************************************************
       B000-MAIN.           
           
       *****************************************************************
       * Write Informational Message to set correct Logging Program    *
       *****************************************************************           
       A010-WRT-START-MSG.    
           PERFORM X100-ERR-COMMON-SETUP
           
           SET WS-PARA-A010
               WS-MSG-000              TO TRUE
           MOVE WS-PARA-NAM            TO LOG-PARA-NAM
           MOVE WS-MSG-NO              TO LOG-MSG-NO
           MOVE WS-MSG00-TXT           TO LOG-ERR-TEXT-LONG

           PERFORM X690-PROC-INFO-MSG
           .
       *****************************************************************
       * Open Input File                                               *
       *****************************************************************           
       A020-OPEN-IBRI.           
           OPEN INPUT IBRI
           IF INFILE-STATUS NOT = '00'
               DISPLAY 'FILE OPEN ERROR, FILE STATUS IS :' INFILE-STATUS
               PERFORM X290-PROC-DATA-ERROR
           END-IF
           .
       *****************************************************************
       *  Initial Common fields required for Logging Routine           *
       *****************************************************************           
       X100-ERR-COMMON-SETUP. 
           MOVE SPACE                  TO WE-ERROR-FLDS
           INITIALIZE                     WE-ERROR-FLDS 
                                        REPLACING NUMERIC DATA BY ZERO
           MOVE WS-PRJ-ID              TO LOG-PRJ-ID
           MOVE WS-PROG-NAM            TO LOG-PROG-NAM
           MOVE WS-SYSTM-ID            TO LOG-SYSTM-ID
           MOVE WS-DEPTT-ID            TO LOG-DEPTT-ID
           .

       *****************************************************************
       *  Application Error Processing                                 *
       *****************************************************************           
       X290-PROC-DATA-ERROR.
           PERFORM X790-PROC-WRNG-MSG
           PERFORM X999-PGM-EXIT
           .
       *****************************************************************
       * DB2 Error Processing Para                                     *
       *****************************************************************           
       X490-PROC-SQL-ERROR.
           PERFORM X790-PROC-WRNG-MSG
           PERFORM X999-PGM-EXIT
           .       
       *****************************************************************
       * Log Informational Messages                                    *
       *****************************************************************           
       X690-PROC-INFO-MSG.  
           SET LOG-INFM-ONLY
               LOG-INFM-MSG
               LOG-NO-ACTN
               IB-DEPT                 TO TRUE
           MOVE WS-DEPT-ID             TO LOG-DEPT-ID
           PERFORM X920-ERROR-LOG-RETN
           .           
       *****************************************************************
       * Log Warning Messages                                          *
       *****************************************************************           
       X790-PROC-WRNG-MSG.  
           SET LOG-DATA-ERROR
               LOG-WRNG-MSG
               IB-DEPT
               LOG-NO-ACTN             TO TRUE
           MOVE WS-DEPT-ID             TO LOG-DEPT-ID
           PERFORM X920-ERROR-LOG-RETN
           .
       *****************************************************************
       *                             *
       *****************************************************************           
       X920-ERROR-LOG-RETN.
           SET WS-SUBPGM-LOG TO TRUE
           MOVE WS-SUBPGM-NAM TO LOG-ERR-PGM
           .
       *****************************************************************
       * ABEND COBOL PROGRAM ON SERIOUS ERROR                          *
       *****************************************************************           
       X999-PGM-EXIT.
           DISPLAY '****Program Exited in X999****'
           SET PROG-ABEND              TO TRUE
           PERFORM C010-CLOSING-MSG
           MOVE 16                     TO RETURN-CODE
           CALL WS-CANCEL-PGM
           .