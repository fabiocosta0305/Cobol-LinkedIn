       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH6SOLUTION.
      * READS AN INDEXED FILE USING EITHER REF ID OR TOPIC
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   
	   SELECT WIKIFILE ASSIGN TO "WIKIIDX.DAT"
        FILE STATUS IS FILE-CHECK-KEY
		ORGANIZATION IS INDEXED
		ACCESS MODE IS DYNAMIC
		RECORD KEY IS WIKI-ID-IDX
		ALTERNATE RECORD KEY IS WIKI-TOPIC-IDX
		   WITH DUPLICATES.
		   
    
       DATA DIVISION.
       FILE SECTION.
	   FD WIKIFILE.
	   01 WIKIRECORDIDX.
	      05 WIKI-TOPIC-IDX         PIC X(50).
		  05 WIKI-ID-IDX            PIC 9(8).
		  05 WIKI-COMMENT-IDX       PIC X(100).
		
       WORKING-STORAGE SECTION.
       01  WS-WORKING-STORAGE.
           05 FILLER               PIC X(27) VALUE 
		      'WORKING STORAGE STARTS HERE'.
	       
     
   
	   01  WS-WORK-AREAS.
	       05  FILE-CHECK-KEY      PIC X(2).
		       88 RECORDFOUND      VALUE "00".
			   
		   05  READTYPE             PIC 9.
		       88 WIKI-ID-KEY       VALUE 1.
			   88 WIKI-TOPIC-KEY    VALUE 2.
			   
	       05  PRINTRECORD.
               10  PRTWIKICODE     PIC 9(8).
               10  PRTWIKITOPIC    PIC BBBBX(50).


       PROCEDURE DIVISION.
       0100-START.

		   OPEN INPUT WIKIFILE.
		   DISPLAY "SELECT RECORD BY WIKI ID, ENTER 1". 
		   DISPLAY "SELECT RECORD BY TOPIC, ENTER 2".
			  
		   ACCEPT READTYPE.
		   
		   IF WIKI-ID-KEY
		      DISPLAY "ENTER WIKI CODE KEY (8 DIGITS): " 
			    WITH NO ADVANCING
			    
			  ACCEPT WIKI-ID-IDX
			  READ WIKIFILE
			    KEY IS WIKI-ID-IDX
			    INVALID KEY DISPLAY "WIKI STATUS: ",
				  FILE-CHECK-KEY
			  END-READ			 
           END-IF	

           IF WIKI-TOPIC-KEY
		      DISPLAY "ENTER WIKI TOPIC (50 CHARACTERS): " 
			    WITH NO ADVANCING
			  ACCEPT WIKI-TOPIC-IDX
              READ WIKIFILE
                KEY IS WIKI-TOPIC-IDX			  
                INVALID KEY DISPLAY "WIKI STATUS: ",
				  FILE-CHECK-KEY
              END-READ
			END-IF
			
			IF RECORDFOUND
			   MOVE WIKI-ID-IDX TO PRTWIKICODE
			   MOVE WIKI-TOPIC-IDX TO PRTWIKITOPIC
			   
			   DISPLAY PRINTRECORD
			END-IF.
			
			
		   PERFORM 9000-END-PROGRAM.
		   
	   0100-END.
	   
	 
	   9000-END-PROGRAM.
           CLOSE WIKIFILE.    	   
		                 
           STOP RUN.
           
          END PROGRAM CH6SOLUTION.
