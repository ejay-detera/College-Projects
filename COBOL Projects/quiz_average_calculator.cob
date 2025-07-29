       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUIZAVERAGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENTFILE ASSIGN TO 'STUDENT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD STUDENTFILE.
                   01 STUDREC.
                       05 STUDENTLINE  PIC X(100).

       WORKING-STORAGE SECTION.
           01 STUDENT-DATA.
               05 NAME OCCURS 99 TIMES PIC X(25).
               05 QUIZ1 OCCURS 99 TIMES PIC 999.
               05 QUIZ2 OCCURS 99 TIMES PIC 999.
               05 QUIZ3 OCCURS 99 TIMES PIC 999.
               05 QUIZ4 OCCURS 99 TIMES PIC 999.
               05 QUIZ5 OCCURS 99 TIMES PIC 999.               
               05 SUM-QUIZZES OCCURS 99 TIMES PIC 999.
               05 QUIZ-AVERAGE OCCURS 99 TIMES PIC 999V99.
               05 GRADE-EQUIV OCCURS 99 TIMES PIC 999V99.
           01 INCREMENTIONS.
               05 I                 PIC 9 VALUE ZERO.
           01 HDG-0.
               02 FILLER PIC X(23) VALUE SPACES.
               02 FILLER PIC X(26) VALUE "POLYTECHNIC UNIVERSITY OF".
               02 FILLER PIC X(15) VALUE "THE PHILIPPINES".
           01 HDG-1.
               02 FILLER PIC X(34) VALUE SPACES.
               02 FILLER PIC X(18) VALUE "Quezon City Branch".
           01 HDG-2.
               02 FILLER PIC X(20) VALUE SPACES.       
           01 HDG-3.
               02 FILLER PIC X(39) VALUE SPACES.
               02 FILLER PIC X(8) VALUE "BSIT 2-1".
           01 HDG-4.
               02 FILLER PIC X(33) VALUE SPACES.
               02 FILLER PIC X(21) VALUE "Quiz Grade Equivalent".
           01 HDG-5.
               02 FILLER PIC X(20) VALUE SPACES. 
           01 HDG-6.
               02 FILLER PIC X(12) VALUE "Student Name".
               02 FILLER PIC X(13) VALUE SPACES.
               02 FILLER PIC X(6) VALUE "Quiz 1".
               02 FILLER PIC X(3) VALUE SPACES.
               02 FILLER PIC X(6) VALUE "Quiz 2".
               02 FILLER PIC X(3) VALUE SPACES.
               02 FILLER PIC X(6) VALUE "Quiz 3".
               02 FILLER PIC X(3) VALUE SPACES.
               02 FILLER PIC X(6) VALUE "Quiz 4".
               02 FILLER PIC X(3) VALUE SPACES.
               02 FILLER PIC X(6) VALUE "Quiz 5".
               02 FILLER PIC X(6) VALUE SPACES.
               02 FILLER PIC X(7) VALUE "Average".
               02 FILLER PIC X(5) VALUE SPACES.
               02 FILLER PIC X(5) VALUE "Grade".
           01 HDG-7.
               02 FILLER PIC X(20) VALUE SPACES. 
           01 DATALINE.
               02 NAMEOUT          PIC X(25).
               02 SPACES1           PIC X(2).
               02 QUIZ1OUT         PIC Z(3).
               02 SPACES2           PIC X(6).
               02 QUIZ2OUT         PIC Z(3).
               02 SPACES3           PIC X(6).
               02 QUIZ3OUT         PIC Z(3).
               02 SPACES4           PIC X(6).
               02 QUIZ4OUT         PIC Z(3).
               02 SPACES5           PIC X(6).
               02 QUIZ5OUT         PIC Z(3).
               02 SPACES6           PIC X(2).
               02 QUIZ-AVERAGEOUT PIC ZZ9.99.
               02 SPACES7           PIC X(2).
               02 GRADE-EQUIVOUT   PIC 9.99.
           01 NUMERIC-GRADE        PIC 9V99.
           01 CNTR                 PIC 9 VALUE 0. 
           01 VALID-INPUT            PIC X(3) VALUE "NO".
           01 FIELD-TO-CHECK         PIC X(10).
           01 ESCAPE-CODE PIC X VALUE X"1B".
           01 CLEAR-SCREEN-SEQUENCE PIC X(4) VALUE "[2J".  
           01 CURSOR-HOME-SEQUENCE PIC X(3) VALUE "[H".       
               
       PROCEDURE DIVISION.
           OPEN OUTPUT STUDENTFILE.
           PERFORM WRITEHEADINGS.
           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "Enter number of students: " WITH NO ADVANCING
               ACCEPT CNTR
               
               IF FUNCTION TEST-NUMVAL(CNTR) = 0
                AND CNTR IS NUMERIC
                AND CNTR IS POSITIVE
                AND CNTR <= 9
                AND CNTR >= 1
                AND FUNCTION INTEGER(CNTR) = CNTR
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 INITIALIZE CNTR
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CNTR
               PERFORM INPUTDATA
               PERFORM WRITESTUDENTDATA
           END-PERFORM.
           PERFORM DISPLAY-RTN
           CLOSE STUDENTFILE.
           STOP RUN.

       INPUTDATA.
           DISPLAY "Enter student name: " WITH NO ADVANCING
           ACCEPT NAME(I).
           PERFORM UNTIL NAME(I) IS ALPHABETIC
               DISPLAY "Invalid input. Please enter a valid name "
               "(letters only): " WITH NO ADVANCING
                   ACCEPT NAME(I)
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
               DISPLAY "Enter Quiz 1 score: " WITH NO ADVANCING
               ACCEPT QUIZ1(I)
               
               IF FUNCTION TEST-NUMVAL(QUIZ1(I)) = 0
                AND QUIZ1(I) IS NUMERIC
                AND QUIZ1(I) <= 999
                AND QUIZ1(I) >= 1
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
               DISPLAY "Enter Quiz 2 score: " WITH NO ADVANCING
               ACCEPT QUIZ2(I)
               
               IF FUNCTION TEST-NUMVAL(QUIZ2(I)) = 0
                AND QUIZ2(I) IS NUMERIC
                AND QUIZ2(I) <= 999
                AND QUIZ2(I) >= 1
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
               DISPLAY "Enter Quiz 3 score: " WITH NO ADVANCING
               ACCEPT QUIZ3(I)
               
               IF FUNCTION TEST-NUMVAL(QUIZ3(I)) = 0
                AND QUIZ3(I) IS NUMERIC
                AND QUIZ3(I) <= 999
                AND QUIZ3(I) >= 1
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
               DISPLAY "Enter Quiz 4 score: " WITH NO ADVANCING
               ACCEPT QUIZ4(I)
               
               IF FUNCTION TEST-NUMVAL(QUIZ4(I)) = 0
                AND QUIZ4(I) IS NUMERIC
                AND QUIZ4(I) <= 999
                AND QUIZ4(I) >= 1
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
               PERFORM CLEAR-SCREEN
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
               DISPLAY "Enter Quiz 5 score: " WITH NO ADVANCING
               ACCEPT QUIZ5(I)
               
               IF FUNCTION TEST-NUMVAL(QUIZ5(I)) = 0
                AND QUIZ5(I) IS NUMERIC
                AND QUIZ5(I) <= 999
                AND QUIZ5(I) >= 1
               
                MOVE "YES" TO VALID-INPUT
                DISPLAY "Input Accepted!"
                DISPLAY SPACES
               ELSE
                 DISPLAY "Invalid input!"
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.
           PERFORM CLEAR-SCREEN
           PERFORM CALCULATE.

       CLEAR-SCREEN.
           *> Send escape sequences to clear the screen and reset cursor position
           DISPLAY ESCAPE-CODE UPON CONSOLE.
           DISPLAY CLEAR-SCREEN-SEQUENCE UPON CONSOLE.
           DISPLAY ESCAPE-CODE UPON CONSOLE.
           DISPLAY CURSOR-HOME-SEQUENCE UPON CONSOLE.
           EXIT.

       CALCULATE.
           COMPUTE SUM-QUIZZES(I) = QUIZ1(I) + QUIZ2(I) + QUIZ3(I) 
                + QUIZ4(I) + QUIZ5(I).
           COMPUTE QUIZ-AVERAGE(I) = SUM-QUIZZES(I) / 5.
    
           EVALUATE TRUE
               WHEN QUIZ-AVERAGE(I) >= 96
                   MOVE 1.00 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 91
                   MOVE 1.25 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 86
                   MOVE 1.50 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 81
                   MOVE 1.75 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 76
                   MOVE 2.00 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 71
                   MOVE 2.25 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 66
                   MOVE 2.50 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 61
                   MOVE 2.75 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 60
                   MOVE 3.00 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 52
                   MOVE 3.25 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 44
                   MOVE 3.50 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 36
                   MOVE 3.75 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 28
                   MOVE 4.00 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 20
                   MOVE 4.25 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 12
                   MOVE 4.50 TO NUMERIC-GRADE
               WHEN QUIZ-AVERAGE(I) >= 4
                   MOVE 4.75 TO NUMERIC-GRADE
               WHEN OTHER
                   MOVE 5.00 TO NUMERIC-GRADE
           END-EVALUATE.
           MOVE NUMERIC-GRADE TO GRADE-EQUIV(I).
       DISPLAY-RTN.
           DISPLAY "                   POLYTECHNIC UNIVERSITY OF THE "
               "PHILIPPINES".
           DISPLAY "                             Quezon City Branch".
           DISPLAY " ".
           DISPLAY "                                   BSIT 2-1".
           DISPLAY "                            Quiz Grade Equivalent".
           DISPLAY " ".
           DISPLAY "   Student Name             Quiz 1   Quiz 2   "
               "Quiz 3   Quiz 4   Quiz 5   Average   Grade".
           DISPLAY "----------------------------------------------"
               "-----------------------------------------------".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CNTR
               DISPLAY "   " NAME(I) "  "
                   QUIZ1(I) "      "
                   QUIZ2(I) "      "
                   QUIZ3(I) "      "
                   QUIZ4(I) "      "
                   QUIZ5(I) "    "
                   QUIZ-AVERAGE(I) "    "
                   GRADE-EQUIV(I)
           END-PERFORM.
       WRITEHEADINGS.
           MOVE HDG-0 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-1 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-2 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-3 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-4 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-5 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-6 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
           MOVE HDG-7 TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.

       WRITESTUDENTDATA.
           MOVE NAME(I) TO NAMEOUT.
           MOVE QUIZ1(I) TO QUIZ1OUT.
           MOVE QUIZ2(I) TO QUIZ2OUT.
           MOVE QUIZ3(I) TO QUIZ3OUT.
           MOVE QUIZ4(I) TO QUIZ4OUT.
           MOVE QUIZ5(I) TO QUIZ5OUT.
           MOVE QUIZ-AVERAGE(I) TO QUIZ-AVERAGEOUT.
           MOVE GRADE-EQUIV(I) TO GRADE-EQUIVOUT.
           MOVE DATALINE TO STUDENTLINE.
           WRITE STUDREC FROM STUDENTLINE.
