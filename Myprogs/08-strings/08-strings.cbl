        IDENTIFICATION DIVISION.
        PROGRAM-ID. MAILING-LIST.
        AUTHOR. FABIO COSTA.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

        SELECT CUSTOMERS
            ASSIGN TO "Customers.csv"
            FILE STATUS IS WS-CUST-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.

        SELECT MAILING
            ASSIGN TO "MailingList.rpt"
            ORGANIZATION IS LINE SEQUENTIAL.
        
        DATA DIVISION.

        FILE SECTION.

        FD CUSTOMERS.
        01 CUSTOMERS-DATA.
            88 ENDOFLINE        VALUE HIGH-VALUES.
            02 CUSTOMER-INFO    PIC X(255) VALUE SPACES.
        
        FD MAILING.
        01 MAILING-DATA.
            02 FILLER           PIC X(80) VALUE SPACES.
        

        WORKING-STORAGE SECTION.

        01 MEM-VARS.
            02 WS-CUST-STATUS    PIC X(2).
            02 STRINGEND         PIC 999.
            02 WRITELINE         PIC X(80) VALUE SPACES.
        
        01 WS-CUSTOMER-DATA.
            02 WSC-FIRST-NAME     PIC X(10) VALUE SPACES.
            02 WSC-LAST-NAME      PIC X(10) VALUE SPACES.
            02 WSC-STREET-NUMBER  PIC X(4) VALUE SPACES.
            02 WSC-STREET         PIC X(40) VALUE SPACES.
            02 WSC-CITY           PIC X(40) VALUE SPACES.
            02 WSC-STATE          PIC X(2) VALUE SPACES.
            02 WSC-ZIP            PIC X(5) VALUE SPACES.
        
        PROCEDURE DIVISION.

        0100-INIT.

            OPEN INPUT CUSTOMERS
            IF WS-CUST-STATUS NOT = "00" THEN 
                DISPLAY "ERROR OPENING ORIGINAL COMPANY FILE - ",
                    WS-CUST-STATUS
                GO TO 9999-END-RUN
            END-IF.

            OPEN OUTPUT MAILING.

            READ CUSTOMERS
                AT END SET ENDOFLINE TO TRUE
            END-READ.

            PERFORM 0200-PROCESS-LINES UNTIL ENDOFLINE.

            GO TO 9999-END-RUN.

        0200-PROCESS-LINES.

            PERFORM VARYING STRINGEND FROM 255 BY -1
                UNTIL CUSTOMER-INFO(STRINGEND:1) NOT = SPACE
            END-PERFORM.

            UNSTRING CUSTOMER-INFO(1:STRINGEND) DELIMITED BY ","
                INTO WSC-LAST-NAME
                     WSC-FIRST-NAME
                     WSC-STREET-NUMBER
                     WSC-STREET
                     WSC-CITY
                     WSC-STATE
                     WSC-ZIP
            END-UNSTRING.

            MOVE SPACES TO WRITELINE.

            STRING WSC-FIRST-NAME DELIMITED BY SPACES 
                   ' ' DELIMITED BY SIZE
                   WSC-LAST-NAME  DELIMITED BY SPACES
                   INTO WRITELINE.

            DISPLAY WRITELINE.

            WRITE MAILING-DATA FROM WRITELINE
                AFTER ADVANCING 1 LINES.
            
            MOVE SPACES TO WRITELINE.

            STRING WSC-STREET-NUMBER DELIMITED BY SPACES 
                   ' ' DELIMITED BY SIZE
                   WSC-STREET DELIMITED BY SIZE
                   INTO WRITELINE.

            DISPLAY WRITELINE.

            WRITE MAILING-DATA FROM WRITELINE
                AFTER ADVANCING 1 LINES.
            
            MOVE SPACES TO WRITELINE.

            STRING WSC-CITY DELIMITED BY SPACES 
                   ',' DELIMITED BY SIZE
                   WSC-STATE DELIMITED BY SPACES
                   ' ' DELIMITED BY SIZE
                   WSC-ZIP DELIMITED BY SPACES
                   INTO WRITELINE.

            DISPLAY WRITELINE.

            WRITE MAILING-DATA FROM WRITELINE
                AFTER ADVANCING 1 LINES.
            
            WRITE MAILING-DATA FROM SPACES AFTER ADVANCING 1 LINES.
            MOVE SPACES TO WRITELINE.

            READ CUSTOMERS
                AT END SET ENDOFLINE TO TRUE
            END-READ.

        9999-END-RUN.

            CLOSE CUSTOMERS.
            CLOSE MAILING.
            STOP RUN.
