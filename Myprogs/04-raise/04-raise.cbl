       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-RAISE.
       AUTHOR. FABIO COSTA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ED-DATA 
            ASSIGN TO "EES.DAT"
            FILE STATUS IS ED-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT NEWED-DATA 
        ASSIGN TO "EES-NEW.DAT"
        FILE STATUS IS NED-STATUS
        ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT ED-REPORT
        ASSIGN TO "EES-NEW.RPT"
        FILE STATUS IS ER-STATUS
        ORGANIZATION IS LINE SEQUENTIAL.
        
       DATA DIVISION.

       FILE SECTION.

       FD ED-DATA.
       01 EMPLOYEEDETAILS.
            88 ED-ENDOFFILE                     VALUE  HIGH-VALUES.
            02 FD-ED-SSN           PIC 9(9).
            02 FD-ED-NAME.
               03 FD-ED-LASTNAME  PIC X(10).
               03 FD-ED-FIRSTNAME PIC X(10).
            02 FD-ED-BIRTHDAY.
               03 FD-ED-YEAR      PIC 9(4).
               03 FD-ED-MONTH     PIC 9(2).
               03 FD-ED-DAY       PIC 9(2).
            02 FD-ED-SALARY           PIC 9(9).
            02 FD-ED-GENDER           PIC X(1).

       FD NEWED-DATA.
       01 PRINT-NEWED-DATA.
           02 FILLER      PIC X(80).

       FD ED-REPORT.
       01 PRINTDATA.
           02 FILLER      PIC X(132).
       
       WORKING-STORAGE SECTION.

       01 WS-FLAGS.
          02 NED-STATUS PIC X(2).
          02 ED-STATUS    PIC X(2).
          02 ER-STATUS  PIC X(2).

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
           05  WS-YEAR PIC 99.
           05  WS-MONTH PIC 99.
           05  WS-DAY   PIC 99.
		   
	   01  WS-WORK-AREAS.
	       05  WS-SALARY-ORIGINAL  PIC 9(9) VALUE ZERO. 
           05  WS-SALARY-NEW       PIC 9(9) VALUE ZERO.
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

        01 NEWEMPLOYEEDETAILS.
            02 FD-NED-SSN           PIC 9(9).
            02 FD-NED-NAME.
               03 FD-NED-LASTNAME  PIC X(10).
               03 FD-NED-FIRSTNAME PIC X(10).
            02 FD-NED-BIRTHDAY.
               03 FD-NED-YEAR      PIC 9(4).
               03 FD-NED-MONTH     PIC 9(2).
               03 FD-NED-DAY       PIC 9(2).
            02 FD-NED-SALARY           PIC 9(9).
            02 FD-NED-GENDER           PIC X(1).


       PROCEDURE DIVISION.

       0100-OPEN-FILES.

           OPEN INPUT ED-DATA
               IF ED-STATUS NOT = "00"
                   DISPLAY "ERROR ON READ THE DATA INPUT FILE - ",
                       ED-STATUS
                   GO TO 9999-END-PROGRAM
               END-IF.
           OPEN OUTPUT ED-REPORT.
           OPEN OUTPUT NEWED-DATA.

           READ ED-DATA
               AT END SET ED-ENDOFFILE TO TRUE
               END-READ.
               WRITE PRINTDATA FROM HEADING-LINE 
			      AFTER ADVANCING PAGE.
               PERFORM 0200-LOAD-UPDATE-REPORT UNTIL ED-ENDOFFILE.

		   MOVE WS-TOT-SALARY TO TOT-SALARY.
		   WRITE PRINTDATA FROM TOTAL-LINE1 
		      AFTER ADVANCING 1 LINES.
		   WRITE PRINTDATA FROM TOTAL-LINE2 
		      AFTER ADVANCING 1 LINES.

            PERFORM 9999-END-PROGRAM.
               
       0200-LOAD-UPDATE-REPORT.

	        MOVE FD-ED-SSN TO DET-EMP-SSN.
	        MOVE FD-ED-LASTNAME TO DET-LNAME.
			MOVE FD-ED-FIRSTNAME TO DET-FNAME.
			MOVE WS-MONTH-NAME(FD-ED-MONTH) TO 
			   DET-START-MON.
			MOVE FD-ED-DAY TO DET-START-DAY.
			MOVE FD-ED-YEAR TO DET-START-YEAR.
			MOVE FD-ED-SALARY TO WS-SALARY-ORIGINAL.

            COMPUTE WS-SALARY-NEW = WS-SALARY-ORIGINAL + 
               (WS-SALARY-ORIGINAL * WS-RAISE).


			ADD WS-SALARY-NEW TO WS-TOT-SALARY.

	        MOVE FD-ED-SSN TO FD-NED-SSN.
	        MOVE FD-ED-NAME TO FD-NED-NAME.
            MOVE FD-ED-BIRTHDAY TO FD-NED-BIRTHDAY.
            MOVE WS-SALARY-NEW TO FD-NED-SALARY.
            MOVE FD-ED-GENDER TO FD-NED-GENDER.

            MOVE WS-SALARY-NEW TO DET-SALARY.

			WRITE PRINTDATA FROM DETAIL-LINE 
			   AFTER ADVANCING 1 LINES.
            WRITE PRINT-NEWED-DATA FROM NEWEMPLOYEEDETAILS
			   AFTER ADVANCING 1 LINES.

      *     DISPLAY DETAIL-LINE.
			READ ED-DATA
			  AT END SET ED-ENDOFFILE TO TRUE
			END-READ.

       9999-END-PROGRAM.

           CLOSE NEWED-DATA.
           CLOSE ED-REPORT.
           CLOSE ED-DATA.
           STOP RUN.

