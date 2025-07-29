       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT REPORTFILE ASSIGN TO 'REPORT.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT SALESFILE ASSIGN TO 'SALES.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT MARKETINGFILE ASSIGN TO 'MARKETING.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT RNDFILE ASSIGN TO 'RND.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD  REPORTFILE.
               01  REPORTRECORD.
                   05  REPORTLINE         PIC X(100).

           FD  SALESFILE.
               01  SALESRECORD.
                   05  SALESLINE          PIC X(100).

           FD  MARKETINGFILE.
               01  MARKETINGRECORD.
                   05  MARKETINGLINE      PIC X(100).

           FD  RNDFILE.
               01  RNDRECORD.
                   05  RNDLINE            PIC X(100).

       WORKING-STORAGE SECTION.
           01  EMPLOYEERECORD.
               05  EMPLOYEENUMBER     OCCURS 9 TIMES PIC X(10).
               05  EMPLOYEENAME       OCCURS 9 TIMES PIC X(25).
               05  DEPARTMENT         OCCURS 9 TIMES PIC X(15).
               05  HOUR1              OCCURS 9 TIMES PIC 99V99.
               05  HOUR2              OCCURS 9 TIMES PIC 99V99.
               05  HOUR3              OCCURS 9 TIMES PIC 99V99.
               05  HOUR4              OCCURS 9 TIMES PIC 99V99.
               05  RATEPERHOUR        OCCURS 9 TIMES PIC 999V99.
               05  SSSPREMIUM         OCCURS 9 TIMES PIC 9999V99.
               05  HDMFPREMIUM        OCCURS 9 TIMES PIC 999V99.
               05  PHILHEALTHPREMIUM  OCCURS 9 TIMES PIC 999V99.
               05  TAX                OCCURS 9 TIMES PIC 9999V99.

           01  COMPUTEDFIELDS.
               05  BASICPAY           OCCURS 9 TIMES PIC 9(7)V99.
               05  OVERTIMEPAY        OCCURS 9 TIMES PIC 9(7)V99.
               05  TOTALDEDUCTIONS    OCCURS 9 TIMES PIC 9(7)V99.
               05  NETPAY             OCCURS 9 TIMES PIC 9(7)V99.
               05  OVERTIMEHOURS      OCCURS 9 TIMES PIC 99V99.
               05  OVERTIMERATE       OCCURS 9 TIMES PIC 999V99.
               05  MONTHLYWORKED      OCCURS 9 TIMES PIC 999V99.

           01  USERINPUTFIELDS.
               05  I                 PIC 9 VALUE 0.
               05  CNTR              PIC 9 VALUE 0.      

           01 HDG-0.
               02 FILLER PIC X(31) VALUE SPACES.
               02 FILLER PIC X(13) VALUE "BIG Company".
           01 HDG-1.
               02 FILLER PIC X(28) VALUE SPACES.
               02 FILLER PIC X(16) VALUE "BGC, Taguig City".
           01 HDG-2.
               02 FILLER PIC X(33) VALUE SPACES.
               02 FILLER PIC X(7) VALUE "PAYROLL".
           01 HDG-DEPARTMENT.
               02 FILLER PIC X(25) VALUE SPACES.
               02 DEPARTMENT-NAME PIC X(35).
           01 HDG-3.
               02 FILLER PIC X(15) VALUE "Employee Number".
               02 FILLER PIC X(2) VALUE SPACES.
               02 FILLER PIC X(13) VALUE "Employee Name".
               02 FILLER PIC X(4) VALUE SPACES.
               02 FILLER PIC X(5) VALUE "Dept.".
               02 FILLER PIC X(7) VALUE SPACES.
               02 FILLER PIC X(9) VALUE "Basic Pay".
               02 FILLER PIC X(2) VALUE SPACES.
               02 FILLER PIC X(8) VALUE "Overtime".
               02 FILLER PIC X(2) VALUE SPACES.
               02 FILLER PIC X(10) VALUE "Deductions".
               02 FILLER PIC X(2) VALUE SPACES.
               02 FILLER PIC X(7) VALUE "Net Pay".
            
           01  FORMATTEDLINE.
               05  EMPNUMOUT        PIC X(10).
               05  FILLER           PIC X(5) VALUE SPACES.
               05  EMPNAMEOUT       PIC X(25).
               05  FILLER           PIC X(2) VALUE SPACES.
               05  DEPARTMENTCN     PIC X(9).
               05  BASICPAYOUT      PIC Z(6)9.99.
               05  FILLER           PIC X(2) VALUE SPACES.
               05  OVERTIMEPAYOUT   PIC Z(6)9.99.
               05  FILLER           PIC X(2) VALUE SPACES.
               05  DEDUCTIONSOUT    PIC Z(6)9.99.
               05  FILLER           PIC X(2) VALUE SPACES.
               05  NETPAYOUT        PIC Z(6)9.99.

           01 L                           PIC 99 VALUE ZERO.
           01  SALES-HEADING-WRITTEN      PIC X VALUE "N".
           01  MARKETING-HEADING-WRITTEN  PIC X VALUE "N".
           01  RND-HEADING-WRITTEN        PIC X VALUE "N".
           01  VALID-INPUT                PIC X(5).

       SCREEN SECTION.
           01 SCRN.
               02 BLANK SCREEN.

       PROCEDURE DIVISION.
           OPEN OUTPUT REPORTFILE SALESFILE MARKETINGFILE RNDFILE.
           PERFORM WRITEHEADINGS.
           PERFORM PROCESS-RTN.

       PROCESS-RTN.
           DISPLAY SCRN.
           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER NUMBER OF EMPLOYEE: " 
               LINE L COLUMN 5
           ACCEPT CNTR LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(CNTR) = 0
                AND CNTR IS NUMERIC
                AND CNTR IS POSITIVE
                AND CNTR <= 9
                AND CNTR >= 1
                AND FUNCTION INTEGER(CNTR) = CNTR
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 INITIALIZE CNTR
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CNTR
               PERFORM INPUTEMPLOYEEDETAILS
               PERFORM COMPUTEPAY
               PERFORM WRITEEMPLOYEEDATA
           END-PERFORM
           CLOSE REPORTFILE SALESFILE MARKETINGFILE RNDFILE.
           ADD 1 TO L.
           DISPLAY "PAYROLL REPORT GENERATED SUCCESSFULLY!"
               LINE L COLUMN 5.
           ADD 1 TO L.
           STOP RUN.

       WRITEHEADINGS.
           WRITE REPORTRECORD FROM SPACES.
           MOVE HDG-0 TO REPORTLINE.
           WRITE REPORTRECORD FROM REPORTLINE.
           MOVE HDG-1 TO REPORTLINE.
           WRITE REPORTRECORD FROM REPORTLINE.
           WRITE REPORTRECORD FROM SPACES.
           MOVE HDG-2 TO REPORTLINE.
           WRITE REPORTRECORD FROM REPORTLINE.
           WRITE REPORTRECORD FROM SPACES.
           MOVE HDG-3 TO REPORTLINE.
           WRITE REPORTRECORD FROM REPORTLINE.
           WRITE REPORTRECORD FROM SPACES.

       INPUTEMPLOYEEDETAILS.
           MOVE 3 TO L.
           ADD 1 TO L.

           DISPLAY "ENTER EMPLOYEE NUMBER: " LINE L COLUMN 5.
           ACCEPT EMPLOYEENUMBER(I) LINE L COLUMN 35.
           ADD 1 TO L.

           DISPLAY "ENTER EMPLOYEE NAME: " LINE L COLUMN 5.
           ACCEPT EMPLOYEENAME(I) LINE L COLUMN 35.
           ADD 1 TO L.

           DISPLAY "ENTER DEPARTMENT (SALES/MARKETING/R&D): " 
               LINE L COLUMN 5.
           ACCEPT DEPARTMENT(I) LINE L COLUMN 45.
           ADD 1 TO L.
           PERFORM VALIDATE-DEPARTMENT.


           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HOURLY RATE: " LINE L COLUMN 5
           ACCEPT RATEPERHOUR(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(RATEPERHOUR(I)) = 0
                AND RATEPERHOUR(I) IS NUMERIC
                AND RATEPERHOUR(I) IS POSITIVE
                AND RATEPERHOUR(I) <= 999.99
                AND RATEPERHOUR(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HOURS WORKED WEEK 1: " LINE L COLUMN 5
           ACCEPT HOUR1(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(HOUR1(I)) = 0
                AND HOUR1(I) IS NUMERIC
                AND HOUR1(I) IS POSITIVE
                AND HOUR1(I) <= 99
                AND HOUR1(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HOURS WORKED WEEK 2: " LINE L COLUMN 5
           ACCEPT HOUR2(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(HOUR2(I)) = 0
                AND HOUR2(I) IS NUMERIC
                AND HOUR2(I) IS POSITIVE
                AND HOUR2(I) <= 99
                AND HOUR2(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HOURS WORKED WEEK 3: " LINE L COLUMN 5
           ACCEPT HOUR3(I) LINE L COLUMN 35
           ADD 1 TO L

               
               IF FUNCTION TEST-NUMVAL(HOUR3(I)) = 0
                AND HOUR3(I) IS NUMERIC
                AND HOUR3(I) IS POSITIVE
                AND HOUR3(I) <= 99
                AND HOUR3(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HOURS WORKED WEEK 4: " LINE L COLUMN 5
           ACCEPT HOUR4(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(HOUR4(I)) = 0
                AND HOUR4(I) IS NUMERIC
                AND HOUR4(I) IS POSITIVE
                AND HOUR4(I) <= 99
                AND HOUR4(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER SSS PREMIUM: " LINE L COLUMN 5
           ACCEPT SSSPREMIUM(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(SSSPREMIUM(I)) = 0
                AND SSSPREMIUM(I) IS NUMERIC
                AND SSSPREMIUM(I) IS POSITIVE
                AND SSSPREMIUM(I) <= 9999.99
                AND SSSPREMIUM(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.
           
           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER HDMF PREMIUM: " LINE L COLUMN 5
           ACCEPT HDMFPREMIUM(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(HDMFPREMIUM(I)) = 0
                AND HDMFPREMIUM(I) IS NUMERIC
                AND HDMFPREMIUM(I) IS POSITIVE
                AND HDMFPREMIUM(I) <= 999.99
                AND HDMFPREMIUM(I) >= 1
               
                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.
           
           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER PHILHEALTH PREMIUM: " LINE L COLUMN 5
           ACCEPT PHILHEALTHPREMIUM(I) LINE L COLUMN 35
           ADD 1 TO L
               
               IF FUNCTION TEST-NUMVAL(PHILHEALTHPREMIUM(I)) = 0
                AND PHILHEALTHPREMIUM(I) IS NUMERIC
                AND PHILHEALTHPREMIUM(I) IS POSITIVE
                AND PHILHEALTHPREMIUM(I) <= 999.99
                AND PHILHEALTHPREMIUM(I) >= 1

                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.
           MOVE "NO" TO VALID-INPUT
           PERFORM UNTIL VALID-INPUT = "YES"
           DISPLAY "ENTER TAX: " LINE L COLUMN 5
           ACCEPT TAX(I) LINE L COLUMN 35
           ADD 2 TO L
               
               IF FUNCTION TEST-NUMVAL(TAX(I)) = 0
                AND TAX(I) IS NUMERIC
                AND TAX(I) IS POSITIVE
                AND TAX(I) <= 9999.99
                AND TAX(I) >= 1

                MOVE "YES" TO VALID-INPUT

                ADD 1 TO L
                DISPLAY SPACES
               ELSE
                 DISPLAY "--------INVALID INPUT--------" LINE L COLUMN 5
                 ADD 1 TO L
                 DISPLAY SPACES
                 MOVE "NO" TO VALID-INPUT
               END-IF
           END-PERFORM.

       VALIDATE-DEPARTMENT.
           PERFORM UNTIL DEPARTMENT(I) = "SALES" OR
                           DEPARTMENT(I) = "MARKETING" OR
                           DEPARTMENT(I) = "R&D"

               MOVE 9 TO L
               DISPLAY "INVALID DEPARTMENT! PLEASE ENTER AGAIN."
                   LINE L COLUMN 5
               ADD 1 TO L
               DISPLAY "ENTER DEPARTMENT (SALES/MARKETING/R&D): "
                   LINE L COLUMN 5
               ACCEPT DEPARTMENT(I) LINE L COLUMN 45
               ADD 2 TO L
           END-PERFORM.

       COMPUTEPAY.
           COMPUTE BASICPAY(I) = RATEPERHOUR(I) * 160.

           IF HOUR1(I) > 40
               COMPUTE OVERTIMEHOURS(1) = HOUR1(I) - 40
               COMPUTE OVERTIMERATE(1) = RATEPERHOUR(I) * 1.5
               COMPUTE OVERTIMEPAY(1) = OVERTIMEHOURS(1) * 
                                        OVERTIMERATE(1)
           ELSE
               MOVE 0 TO OVERTIMEPAY(1)
           END-IF.

           IF HOUR2(I) > 40
               COMPUTE OVERTIMEHOURS(2) = HOUR2(I) - 40
               COMPUTE OVERTIMERATE(2) = RATEPERHOUR(I) * 1.5
               COMPUTE OVERTIMEPAY(2) = OVERTIMEHOURS(2) * 
                                        OVERTIMERATE(2)
           ELSE
               MOVE 0 TO OVERTIMEPAY(2)
           END-IF.

           IF HOUR3(I) > 40
               COMPUTE OVERTIMEHOURS(3) = HOUR3(I) - 40
               COMPUTE OVERTIMERATE(3) = RATEPERHOUR(I) * 1.5
               COMPUTE OVERTIMEPAY(3) = OVERTIMEHOURS(3) * 
                                        OVERTIMERATE(3)
           ELSE
               MOVE 0 TO OVERTIMEPAY(3)
           END-IF.

           IF HOUR4(I) > 40
               COMPUTE OVERTIMEHOURS(4) = HOUR4(I) - 40
               COMPUTE OVERTIMERATE(4) = RATEPERHOUR(I) * 1.5
               COMPUTE OVERTIMEPAY(4) = OVERTIMEHOURS(4) * 
                       OVERTIMERATE(4)
           ELSE
               MOVE 0 TO OVERTIMEPAY(4)
           END-IF.
           

           COMPUTE OVERTIMEPAY(I) = OVERTIMEPAY(1) + OVERTIMEPAY(2) 
                                   + OVERTIMEPAY(3) + OVERTIMEPAY(4).
                                   
           COMPUTE MONTHLYWORKED(I) = HOUR1(I) + HOUR2(I) + HOUR3(I) 
                                      + HOUR4(I).


           COMPUTE TOTALDEDUCTIONS(I) = SSSPREMIUM(I) + HDMFPREMIUM(I) + 
                                        PHILHEALTHPREMIUM(I) + TAX(I).


           COMPUTE NETPAY(I) = BASICPAY(I) + OVERTIMEPAY(I) - 
                                TOTALDEDUCTIONS(I).


       IF DEPARTMENT(I) = "SALES" THEN
           MOVE "Sales" TO DEPARTMENTCN
       ELSE
           IF DEPARTMENT(I) = "MARKETING" THEN
               MOVE "Marketing" TO DEPARTMENTCN
           ELSE
               IF DEPARTMENT(I) = "R&D" THEN
                   MOVE "R&D" TO DEPARTMENTCN
               END-IF
           END-IF
       END-IF.


       WRITEEMPLOYEEDATA.
           MOVE EMPLOYEENUMBER(I) TO EMPNUMOUT.
           MOVE EMPLOYEENAME(I) TO EMPNAMEOUT.
           MOVE BASICPAY(I) TO BASICPAYOUT.
           MOVE OVERTIMEPAY(I) TO OVERTIMEPAYOUT.
           MOVE TOTALDEDUCTIONS(I) TO DEDUCTIONSOUT.
           MOVE NETPAY(I) TO NETPAYOUT.

           STRING EMPNUMOUT DELIMITED BY SIZE
                  EMPNAMEOUT DELIMITED BY SIZE
                  DEPARTMENTCN DELIMITED BY SIZE
                  BASICPAYOUT DELIMITED BY SIZE
                  OVERTIMEPAYOUT DELIMITED BY SIZE
                  DEDUCTIONSOUT DELIMITED BY SIZE
                  NETPAYOUT DELIMITED BY SIZE
                  INTO REPORTLINE.

           WRITE REPORTRECORD FROM REPORTLINE.

           IF DEPARTMENT(I) = "SALES"
               IF SALES-HEADING-WRITTEN = "N" THEN
                   MOVE HDG-0 TO SALESLINE
                   WRITE SALESRECORD FROM SALESLINE
                   MOVE HDG-1 TO SALESLINE
                   WRITE SALESRECORD FROM SALESLINE
                   MOVE HDG-2 TO SALESLINE
                   WRITE SALESRECORD FROM SALESLINE
                   MOVE "SALES DEPARTMENT" TO DEPARTMENT-NAME
                   MOVE HDG-DEPARTMENT TO SALESLINE
                   WRITE SALESRECORD FROM SALESLINE
                   WRITE SALESRECORD FROM SPACES
                   MOVE HDG-3 TO SALESLINE
                   WRITE SALESRECORD FROM SALESLINE
                   WRITE SALESRECORD FROM SPACES
                   MOVE "F" TO SALES-HEADING-WRITTEN
               END-IF
               WRITE SALESRECORD FROM REPORTLINE

           ELSE IF DEPARTMENT(I) = "MARKETING"
               IF MARKETING-HEADING-WRITTEN = "N" THEN
                   MOVE HDG-0 TO MARKETINGLINE
                   WRITE MARKETINGRECORD FROM MARKETINGLINE
                   MOVE HDG-1 TO MARKETINGLINE
                   WRITE MARKETINGRECORD FROM MARKETINGLINE
                   MOVE HDG-2 TO MARKETINGLINE
                   WRITE MARKETINGRECORD FROM MARKETINGLINE
                   MOVE "MARKETING DEPARTMENT" TO DEPARTMENT-NAME
                   MOVE HDG-DEPARTMENT TO MARKETINGLINE
                   WRITE MARKETINGRECORD FROM MARKETINGLINE
                   WRITE MARKETINGRECORD FROM SPACES
                   MOVE HDG-3 TO MARKETINGLINE
                   WRITE MARKETINGRECORD FROM MARKETINGLINE
                   WRITE MARKETINGRECORD FROM SPACES
                   MOVE "F" TO MARKETING-HEADING-WRITTEN
               END-IF
               WRITE MARKETINGRECORD FROM REPORTLINE

           ELSE IF DEPARTMENT(I) = "R&D"
               IF RND-HEADING-WRITTEN = "N" THEN
                   MOVE HDG-0 TO RNDLINE
                   WRITE RNDRECORD FROM RNDLINE
                   MOVE HDG-1 TO RNDLINE
                   WRITE RNDRECORD FROM RNDLINE
                   MOVE HDG-2 TO RNDLINE
                   WRITE RNDRECORD FROM RNDLINE
                   MOVE "RESEARCH AND DEV DEPARTMENT" TO DEPARTMENT-NAME
                   MOVE HDG-DEPARTMENT TO RNDLINE
                   WRITE RNDRECORD FROM RNDLINE
                   WRITE RNDRECORD FROM SPACES
                   MOVE HDG-3 TO RNDLINE
                   WRITE RNDRECORD FROM RNDLINE
                   WRITE RNDRECORD FROM SPACES
                   MOVE "F" TO RND-HEADING-WRITTEN
               END-IF
               WRITE RNDRECORD FROM REPORTLINE
           END-IF.

       END PROGRAM PAYROLL.
