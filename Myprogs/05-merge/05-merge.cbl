       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPANY-MERGE.
       AUTHOR. FABIO COSTA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MY-COMPANY 
            ASSIGN TO "ACME.DAT"
            FILE STATUS IS MC-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT OTHER-COMPANY
            ASSIGN TO "FUSESINC.DAT"
            FILE STATUS IS OC-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT NEW-COMPANY
            ASSIGN TO "NEW-ACME.DAT"
            FILE STATUS IS NC-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.

       SELECT MERGE-TEMP
            ASSIGN TO "TEMP.DAT".

       SELECT ED-REPORT
        ASSIGN TO "EES-NEW.RPT"
        FILE STATUS IS ER-STATUS
        ORGANIZATION IS LINE SEQUENTIAL.
        
       DATA DIVISION.

       FILE SECTION.

       FD MY-COMPANY.
       01 MC-DATA     PIC X(48).

       FD OTHER-COMPANY.
       01 OC-DATA     PIC X(48).

       FD NEW-COMPANY.
       01 NC-DATA.
            88 NC-ENDOFFILE                     VALUE  HIGH-VALUES.
            02 FD-NC-SSN           PIC 9(9).
            02 FD-NC-NAME.
               03 FD-NC-LASTNAME   PIC X(10).
               03 FD-NC-FIRSTNAME  PIC X(10).
            02 FD-NC-BIRTHDAY.
               03 FD-NC-YEAR       PIC 9(4).
               03 FD-NC-MONTH      PIC 9(2).
               03 FD-NC-DAY        PIC 9(2).
            02 FD-NC-SALARY        PIC 9(9).
            02 FD-NC-GENDER        PIC X(1).

       SD MERGE-TEMP.
       01 MERGE-DATA.
            02 MG-SSN           PIC 9(9).
            02 FILLER           PIC X(39).

       FD ED-REPORT.
       01 PRINTDATA.
           02 FILLER      PIC X(132).
       
       WORKING-STORAGE SECTION.

       01 OTHERS-FLAGS.
          02 OC-STATUS    PIC X(2).
          02 MC-STATUS    PIC X(2).
          02 NC-STATUS    PIC X(2).
          02 ER-STATUS    PIC X(2).
       01  WS-MONTHS-DATA.
           05 FILLER      PIC X(05) VALUE '01JAN'.
           05 FILLER      PIC X(05) VALUE '02FEB'.
           05 FILLER      PIC X(05) VALUE '03MAR'.
           05 FILLER      PIC X(05) VALUE '04APR'.
           05 FILLER      PIC X(05) VALUE '05MAY'.
           05 FILLER      PIC X(05) VALUE '06JUN'.
           05 FILLER      PIC X(05) VALUE '07JUL'.
           05 FILLER      PIC X(05) VALUE '08AUG'.
           05 FILLER      PIC X(05) VALUE '09SEP'.
           05 FILLER      PIC X(05) VALUE '10OCT'.
           05 FILLER      PIC X(05) VALUE '11NOV'.
           05 FILLER      PIC X(05) VALUE '12DEC'.
            
       01  WS-MONTH-MAP         REDEFINES WS-MONTHS-DATA.
           05 WS-MONTH-ITEM     OCCURS 12 TIMES. 
           10 WS-MONTH-NUM      PIC 9(02).
           10 WS-MONTH-NAME     PIC X(03).
		     	   
       01  WS-DATE.
           05  WS-YEAR  PIC 9999.
           05  WS-MONTH PIC 99.
           05  WS-DAY   PIC 99.
		   
	   01  WS-WORK-AREAS.
           05  WS-RAISE            PIC 9v99 VALUE 0.03.
           05  WS-TOT-SALARY       PIC 9(12) VALUE ZERO. 

       01  HEADING-LINE.
            05 FILLER	        PIC X(11)  VALUE 'EMPLOYEE ID'.
            05 FILLER	        PIC X(2)   VALUE SPACES.
            05 FILLER	        PIC X(16)  VALUE 'EMPLOYEE NAME'.
            05 FILLER	        PIC X(4)   VALUE SPACES.
            05 FILLER	        PIC X(10)  VALUE 'START DATE'.
            05 FILLER	        PIC X(7)   VALUE SPACES.
            05 FILLER	        PIC X(11)  VALUE 'SALARY'.
			05 FILLER           PIC X(71)  VALUE SPACES.

			
		01  DETAIL-LINE.
			05 DET-EMP-SSN       PIC 9(9).
			05 FILLER           PIC X(4) VALUE SPACES.
			05 DET-FNAME        PIC X(10) VALUE SPACES.
			05 DET-LNAME        PIC X(10) VALUES SPACES.
			05 DET-START-DATE.
			   07 DET-START-MON PIC X(3).
			   07 FILLER        PIC X VALUE '-'.
			   07 DET-START-DAY PIC XX.
			   07 FILLER        PIC X VALUE '-'.
			   07 DET-START-YEAR PIC X(4).
            05 FILLER           PIC X VALUE SPACE.
			05 DET-SALARY       PIC $$$,$$$,$$9.
			05 FILLER           PIC X(76).
			
        01  TOTAL-LINE1.
			05 FILLER           PIC X(41) VALUE SPACES.
			05 FILLER           PIC X(15) VALUE 
			      "===============".
			05 FILLER           PIC X(75).
			
	    01  TOTAL-LINE2.
			05 FILLER           PIC X(29) VALUE SPACES.
			05 FILLER           PIC X(10) VALUE "TOTAL".
			05 TOT-SALARY       PIC $,$$$,$$$,$$$,$$9.
			05 FILLER           PIC X(76).

       PROCEDURE DIVISION.

       0100-MERGE-COMPANIES.

            OPEN INPUT MY-COMPANY
            IF MC-STATUS NOT = "00" THEN 
                DISPLAY "ERROR OPENING ORIGINAL COMPANY FILE - ",
                    MC-STATUS
                GO TO 9999-END-PROGRAM
            END-IF.

            OPEN INPUT OTHER-COMPANY
            IF OC-STATUS NOT = "00" THEN 
                DISPLAY "ERROR OPENING THE MERGING COMPANY FILE - ",
                    OC-STATUS
                GO TO 9999-END-PROGRAM
            END-IF.

            MERGE MERGE-TEMP ON ASCENDING KEY
                MG-SSN
                USING MY-COMPANY
                    OTHER-COMPANY
                GIVING NEW-COMPANY.

        0150-REPORTING-NEW-COMPANY.

            OPEN INPUT NEW-COMPANY
            IF NC-STATUS NOT = "00" THEN
                DISPLAY "ERROR OPENING NEW COMPANY FILE - ",
                    NC-STATUS
                GO TO 9999-END-PROGRAM
            END-IF.

            OPEN OUTPUT ED-REPORT.

            READ NEW-COMPANY
                AT END SET NC-ENDOFFILE TO TRUE
                END-READ.
            
            WRITE PRINTDATA FROM HEADING-LINE 
                AFTER ADVANCING PAGE.

            PERFORM 0200-REPORT-EMPLOYEES UNTIL NC-ENDOFFILE.

		    MOVE WS-TOT-SALARY TO TOT-SALARY.

		    WRITE PRINTDATA FROM TOTAL-LINE1 
		        AFTER ADVANCING 1 LINES.
		    WRITE PRINTDATA FROM TOTAL-LINE2 
		        AFTER ADVANCING 1 LINES.

            PERFORM 9999-END-PROGRAM.
               
       0200-REPORT-EMPLOYEES.

	        MOVE FD-NC-SSN TO DET-EMP-SSN.
	        MOVE FD-NC-LASTNAME TO DET-LNAME.
			MOVE FD-NC-FIRSTNAME TO DET-FNAME.
			MOVE WS-MONTH-NAME(FD-NC-MONTH) TO 
			   DET-START-MON.
			MOVE FD-NC-DAY TO DET-START-DAY.
			MOVE FD-NC-YEAR TO DET-START-YEAR.
			MOVE FD-NC-SALARY TO DET-SALARY.

            ADD FD-NC-SALARY TO WS-TOT-SALARY.

			WRITE PRINTDATA FROM DETAIL-LINE 
			   AFTER ADVANCING 1 LINES.

      *     DISPLAY DETAIL-LINE.
			READ NEW-COMPANY
			  AT END SET NC-ENDOFFILE TO TRUE
			END-READ.

       9999-END-PROGRAM.

           CLOSE MY-COMPANY.
           CLOSE NEW-COMPANY.
           CLOSE OTHER-COMPANY.
           CLOSE ED-REPORT.
           STOP RUN.

