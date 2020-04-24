        IDENTIFICATION DIVISION.
        PROGRAM-ID. WEATHER.
        AUTHOR. FABIO COSTA.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

        SELECT WEATHER-DATA 
            ASSIGN TO "weather.dat"
            FILE STATUS IS WS-WD-STATUS
            ORGANIZATION IS LINE SEQUENTIAL.
        
        DATA DIVISION.

        FILE SECTION.

        FD WEATHER-DATA.
        01 WD-DATA.
            88 ENDOFFILE                VALUE HIGH-VALUES.
            02 WD-ST               PIC 9(6).
            02 WD-DATE.
                03 WD-MONTH             PIC 9(2).
                03 WD-DAY               PIC 9(2).
                03 WD-YEAR              PIC 9(4).
            02 WD-MEAN-TEMP             PIC 9(2).
            02 WD-WEATHER-CONDITION.
                03 WDW-FOG              PIC 9(5).
                03 WDW-RAIN             PIC 9(5).
                03 WDW-SNOW             PIC 9(5).
                03 WDW-HAIL             PIC 9(5).
                03 WDW-THUNDER          PIC 9(5).
                03 WDW-TORNADO          PIC 9(5).
        
        WORKING-STORAGE SECTION.

        01  MEM-VALUES.
            02 WS-WD-STATUS             PIC X(2).
            02 WD-INDEX                 PIC 9(6).
            02 WD-SUBIDX                PIC 999.
            02 WDD-SUBIDX               PIC 999.
            02 WS-OPTION                PIC 9.

            02 WD-TABLE OCCURS 100 TIMES 
                ASCENDING KEY IS WDT-STATION
                INDEXED BY STATION-INDEX.
                03 WDT-STATION                    PIC 9(6).
                03 WDT-DATE.
                    04 WDT-MONTH             PIC 9(2).
                    04 WDT-DAY               PIC 9(2).
                    04 WDT-YEAR              PIC 9(4).
                03 WDT-MEAN-TEMP             PIC 9(2).
                03 WDT-CONDITION.
                    04 WDTW-FOG              PIC X(5).
                    04 WDTW-RAIN             PIC X(5).
                    04 WDTW-SNOW             PIC X(5).
                    04 WDTW-HAIL             PIC X(5).
                    04 WDTW-THUNDER          PIC X(5).
                    04 WDTW-TORNADO          PIC X(5).
                
        01 INFO-LINE1.
            02  FILLER             PIC X(10) VALUE "STATION #".
            02  INFO-STATION       PIC 9(6).
            02  FILLER             PIC XXX   VALUE " - ".
            02  INFO-MONTH         PIC 9(2).
            02  FILLER             PIC X     VALUE "/".
            02  INFO-DAY           PIC 9(2).
            02  FILLER             PIC X     VALUE "/".
            02  INFO-YEAR          PIC 9(4).
            02  FILLER             PIC X(14) 
                                   VALUE " - MEAN TIME: ".
            02  INFO-MEAN-TEMP     PIC 9(2).
            02  FILLER             PIC X     VALUE "F".

        01 INFO-HEADER.
            02  FILLER             PIC   X(20)   
                                   VALUE "WEATHER CONDITIONS:".
        
        01 INFO-CONDITIONS.
            02  FILLER                PIC X(5) VALUE SPACES.
            02  INFO-CONDITION-KIND   PIC X(9).
            02  FILLER                PIC XX   VALUE ": ".
            02  INFO-CONDITION-STATUS PIC X(5).

      *  01 PRINT-LINE PIC X(80) VALUE SPACES.

        PROCEDURE DIVISION.

        0100-START.

            OPEN INPUT WEATHER-DATA
            IF WS-WD-STATUS NOT = "00" THEN 
                DISPLAY "ERROR OPENING ORIGINAL COMPANY FILE - ",
                    WS-WD-STATUS
                GO TO 9999-END
            END-IF.

            COMPUTE WD-SUBIDX = 1.

            READ WEATHER-DATA
                AT END SET ENDOFFILE TO TRUE
            END-READ.

            PERFORM 0200-PROCESS-DATA UNTIL ENDOFFILE.

            DISPLAY "ENTER 1 TO REPORT OR 2 FOR SPECIFIC STATION:".

            ACCEPT WS-OPTION.

            IF WS-OPTION = 1
                PERFORM 0250-DISPLAY-DATA
                ELSE IF WS-OPTION = 2
                    PERFORM 0150-SEARCH-STATION
                END-IF
            END-IF.

            GO TO 9999-END.

      *      PERFORM 0250-DISPLAY-DATA.

        0150-SEARCH-STATION.

            DISPLAY "INSERT DESIRED STATION: ".
            ACCEPT WD-INDEX.

            SEARCH WD-TABLE
                AT END
                    PERFORM 9990-ERROR-END
                WHEN WDT-STATION(STATION-INDEX) = WD-INDEX
                    PERFORM 0300-LOAD-INFO
            END-SEARCH.
            
            GO TO 9999-END.

        0200-PROCESS-DATA.

      *     DISPLAY WD-DATA.
      *     DISPLAY WD-SUBIDX.

            MOVE WD-ST           TO WDT-STATION(WD-SUBIDX).
            MOVE WD-DATE              TO WDT-DATE(WD-SUBIDX).
            MOVE WD-MEAN-TEMP         TO WDT-MEAN-TEMP(WD-SUBIDX).
            MOVE WD-WEATHER-CONDITION TO WDT-CONDITION(WD-SUBIDX).
            ADD 1 TO WD-SUBIDX GIVING WD-SUBIDX.

      *      DISPLAY WD-ST.
      *      DISPLAY WDT-STATION(WD-SUBIDX).

            READ WEATHER-DATA
                AT END SET ENDOFFILE TO TRUE
            END-READ.

        0250-DISPLAY-DATA.

            PERFORM VARYING WDD-SUBIDX FROM 1 BY 1
                UNTIL WDD-SUBIDX > 100

                MOVE WDT-STATION(WDD-SUBIDX)
                    TO INFO-STATION
                MOVE WDT-DAY(WDD-SUBIDX)
                    TO INFO-DAY
                MOVE WDT-MONTH(WDD-SUBIDX)
                    TO INFO-MONTH
                MOVE WDT-YEAR(WDD-SUBIDX)
                    TO INFO-YEAR
                MOVE WDT-MEAN-TEMP(WDD-SUBIDX)
                    TO INFO-MEAN-TEMP

                DISPLAY INFO-LINE1

                DISPLAY INFO-HEADER

                MOVE "      FOG"  TO INFO-CONDITION-KIND
                MOVE WDTW-FOG(WDD-SUBIDX)     TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

                MOVE "     RAIN"  TO INFO-CONDITION-KIND
                MOVE WDTW-RAIN(WDD-SUBIDX)    TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

                MOVE "     SNOW"  TO INFO-CONDITION-KIND
                MOVE WDTW-SNOW(WDD-SUBIDX)    TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

                MOVE "     HAIL"  TO INFO-CONDITION-KIND
                MOVE WDTW-HAIL(WDD-SUBIDX)    TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

                MOVE "  THUNDER"  TO INFO-CONDITION-KIND
                MOVE WDTW-THUNDER(WDD-SUBIDX) TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

                MOVE "  TORNADO"  TO INFO-CONDITION-KIND
                MOVE WDTW-TORNADO(WDD-SUBIDX) TO INFO-CONDITION-STATUS
                DISPLAY INFO-CONDITIONS

            END-PERFORM.

        0300-LOAD-INFO.

            MOVE WDT-STATION(STATION-INDEX)   TO INFO-STATION.
            MOVE WDT-DAY(STATION-INDEX)       TO INFO-DAY.
            MOVE WDT-MONTH(STATION-INDEX)     TO INFO-MONTH.
            MOVE WDT-YEAR(STATION-INDEX)      TO INFO-YEAR.
            MOVE WDT-MEAN-TEMP(STATION-INDEX) TO INFO-MEAN-TEMP.

            DISPLAY INFO-LINE1.

            DISPLAY INFO-HEADER.

            MOVE "      FOG"  TO INFO-CONDITION-KIND.
            MOVE WDTW-FOG(STATION-INDEX)     TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

            MOVE "     RAIN"  TO INFO-CONDITION-KIND.
            MOVE WDTW-RAIN(STATION-INDEX)    TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

            MOVE "     SNOW"  TO INFO-CONDITION-KIND.
            MOVE WDTW-SNOW(STATION-INDEX)    TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

            MOVE "     HAIL"  TO INFO-CONDITION-KIND.
            MOVE WDTW-HAIL(STATION-INDEX)    TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

            MOVE "  THUNDER"  TO INFO-CONDITION-KIND.
            MOVE WDTW-THUNDER(STATION-INDEX) TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

            MOVE "  TORNADO"  TO INFO-CONDITION-KIND.
            MOVE WDTW-TORNADO(STATION-INDEX) TO INFO-CONDITION-STATUS.
            DISPLAY INFO-CONDITIONS.

        9990-ERROR-END.

            DISPLAY "STATION NOT FOUND".
            GO TO 9999-END.

        9999-END.
            CLOSE WEATHER-DATA.

            STOP RUN.
