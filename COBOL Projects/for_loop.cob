       IDENTIFICATION DIVISION.
           PROGRAM-ID. TWOARRAY.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT OUTFILE ASSIGN TO "COURSE.DAT".

       DATA DIVISION.
           FILE SECTION.
               FD OUTFILE
                   LABEL RECORDS ARE STANDARD
                   DATA RECORD IS COURSE-REC.
               01 COURSE-REC PIC X(80).

           WORKING-STORAGE SECTION.
               01 I                 PIC 9 VALUE ZERO.
               01 J                 PIC 9 VALUE ZERO.
               01 YEAR-LEVEL.
                   02 YR-LEVEL OCCURS 4 TIMES PIC X(10).
               01 TOTAL-EVERY-YEAR.
                   02 TOTAL-PER-YEAR-IN OCCURS 4 TIMES PIC 9(10).

               01 CCIS.
                   02 YEAR OCCURS 4 TIMES.
                       03 NO-STUD OCCURS 2 TIMES PIC 99.
               01 HDG-TOP.
                   02 FILLER PIC X(9) VALUE SPACES.
                   02 FILLER PIC X(20) VALUE "COLLEGE OF COMPUTER".
                   02 FILLER PIC X(23) VALUE "AND INFORMATION SCIENCE".
               01 HDG-0.
                   02 FILLER PIC X(19) VALUE SPACES.
                   02 FILLER PIC X(18) VALUE "STUDENT POPULATION".
               01 HDG-1.
                   02 FILLER PIC X(4) VALUE "YEAR".
                   02 FILLER PIC X(10) VALUE SPACES.
                   02 FILLER PIC X(4) VALUE "BSIT".
                   02 FILLER PIC X(10) VALUE SPACES.
                   02 FILLER PIC X(4) VALUE "BSCS".
                   02 FILLER PIC X(10) VALUE SPACES.
                   02 FILLER PIC X(24) VALUE "TOTAL NUMBER OF STUDENTS".
                01 HDG-2.
                   02 COLLEGE-YEAR-OUT PIC X(10).
                   02 FILLER PIC X(4) VALUE SPACES.
                   02 ITCS-OUT OCCURS 2 TIMES.
                       03 ITCSOUT PIC ZZ.
                       03 FILLER PIC X(14) VALUE SPACES.
                   02 TOTAL-PER-YEAR-OUT PIC Z(10).
               01 FTR-01.
                   02 FILLER PIC X(7) VALUE "TOTAL: ".
                   02 FILLER PIC X(4) VALUE SPACES.
                   02 TOTAL-BSIT-OUT PIC Z(5).
                   02 FILLER PIC X(11) VALUE SPACES.
                   02 TOTAL-BSCS-OUT PIC Z(5).
               01 TOTAL-BSIT PIC 9(5).
               01 TOTAL-BSCS PIC 9(5).

               01 L                 PIC 9 VALUE ZERO.
               01 KORS              PIC X(4) VALUE SPACES.

           SCREEN SECTION.
               01 SCRN.
                   02 BLANK SCREEN.

       PROCEDURE DIVISION.
           MOVE "FRESHMEN" TO YR-LEVEL (1).
           MOVE "SOPHOMORE" TO YR-LEVEL (2).
           MOVE "JUNIOR" TO YR-LEVEL (3).
           MOVE "SENIOR" TO YR-LEVEL (4).

           OPEN OUTPUT OUTFILE.
           PERFORM HDG-RTN.
           PERFORM PROCESS-RTN.
           PERFORM FIN-RTN.
       
           
       HDG-RTN.
           WRITE COURSE-REC FROM HDG-TOP.
           WRITE COURSE-REC FROM HDG-0.
           WRITE COURSE-REC FROM HDG-1.
           MOVE SPACES TO COURSE-REC.
           WRITE COURSE-REC AFTER 1 LINE.

       PROCESS-RTN.
           DISPLAY SCRN.
           DISPLAY "ENTER NUMBER OF STUDENTS FOR BSCS AND BSIT: "
               LINE 5 COLUMN 5.
           MOVE 6 TO L.
           PERFORM IN-RTN VARYING I FROM 1 BY 1 UNTIL I > 4
               AFTER J FROM 1 BY 1 UNTIL J > 2.

       IN-RTN.
           DISPLAY "ENTER NUMBER OF STUDENTS FOR : " LINE L COLUMN 5.
           IF J = 1
               MOVE "BSIT" TO KORS
           END-IF
           IF J = 2
               MOVE "BSCS" TO KORS
           END-IF
           DISPLAY KORS LINE L COLUMN 34.
           DISPLAY "YEAR LEVEL " LINE L COLUMN 39.
           DISPLAY I LINE L COLUMN 50.
           DISPLAY " : " LINE L COLUMN 51.
           ACCEPT NO-STUD (I, J) LINE L COLUMN 55.
           
           IF J = 1
               ADD NO-STUD (I, 1) TO TOTAL-BSIT.
           IF J = 2
               ADD NO-STUD (I, 2) TO TOTAL-BSCS.
           ADD NO-STUD (I, J) TO TOTAL-PER-YEAR-IN (I).
           MOVE TOTAL-PER-YEAR-IN(I) TO TOTAL-PER-YEAR-OUT.
           MOVE NO-STUD (I, J) TO ITCSOUT (J).
           MOVE YR-LEVEL(I) TO COLLEGE-YEAR-OUT.
           MOVE TOTAL-BSIT TO TOTAL-BSIT-OUT.
           MOVE TOTAL-BSCS TO TOTAL-BSCS-OUT.
           
           IF J = 2
               PERFORM OUT-RTN
           END-IF
           ADD 1 TO L.

       OUT-RTN.
           WRITE COURSE-REC FROM HDG-2.
           MOVE 0 TO TOTAL-PER-YEAR-IN (I).
           MOVE 5 TO L.
           MOVE 0 TO NO-STUD(I, J).

       FIN-RTN.
           WRITE COURSE-REC FROM FTR-01.
           CLOSE OUTFILE.
           STOP RUN.
           