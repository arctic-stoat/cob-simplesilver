000000* MIT License
      * Copyright (c) 2018 Christer Stig Åke Landstedt
      * 
      * Permission is hereby granted, free of charge, to any person obtaining a copy
      * of this software and associated documentation files (the "Software"), to deal
      * in the Software without restriction, including without limitation the rights
      * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      * copies of the Software, and to permit persons to whom the Software is
      * furnished to do so, subject to the following conditions:
      * 
      * The above copyright notice and this permission notice shall be included in all
      * copies or substantial portions of the Software.
      * 
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      * SOFTWARE.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob-simplesilver01.
       AUTHOR.  "Christer Stig Åke Landstedt".

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT INFILE ASSIGN TO "cob-simplesilver01.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS KEY1DATE 
             ALTERNATE RECORD KEY IS KEY2ACCOUNT WITH DUPLICATES.
           SELECT INFILETOTAL ASSIGN TO "cob-simplesilver01total.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS KEY1TOTAL.
           SELECT INFILE-APPINFO ASSIGN TO "cob-simplesilver01info.txt"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
         FILE SECTION.
         FD INFILE
           RECORD CONTAINS 110 CHARACTERS.
           01 INFILEFD.
             05 KEY1DATE PIC 9(14).
             05 KEY2ACCOUNT PIC 9(15).
             05 ACCOUNTNAME PIC X(22).
             05 TRANSACTIONDESCRIPTION PIC X(40).
             05 TRANSACTIONAMMOUNT PIC S9(7)9V99.



         FD INFILETOTAL
           RECORD CONTAINS 100 CHARACTERS.
           01 INFILETOTALFD.
             05 KEY1TOTAL PIC 9(15).
             05 KEY1TOTALCURRENCY PIC X(3).
             05 KEY1TOTALTOTAL PIC S9(9)9V99.

         FD INFILE-APPINFO
           DATA RECORD IS APPINFOFILE.
           01 APPINFOFILE.
             05 APPINFOFILEDATA PIC X(55).

         WORKING-STORAGE SECTION.
         01 WS-ENDOFFILE PIC 9 VALUE ZERO.
         01 WS-APPINFOFILE.
           05 WS-APPINFOFILEDATA PIC X(54).
         01 WS-INFILEFD.
             05 WS-KEY1DATE  PIC 9(14).
             05 WS-KEY2ACCOUNT PIC 9(15).
             05 WS-ACCOUNTNAME PIC X(22).
             05 WS-TRANSACTIONDESCRIPTION PIC X(40).
             05 WS-TRANSACTIONAMMOUNT PIC S9(7)9V99.



         01 WS-INFILETOTALFD.
           05 WS-KEY1TOTAL PIC 9(15).
           05 WS-KEY1TOTALCURRENCY PIC X(3).
           05 WS-KEY1TOTALTOTAL PIC S9(9)9V99.

         01 WS-ADDTRANSACTION PIC S9(7)V99.

         01 DATEANDTIME.
           05 CURRENTDATE.
             10 YYYY PIC 9999.
             10 MM PIC 99.
             10 DD PIC 99.
           05 CURRENTTIME.
             10 TIMEHH PIC 99.
             10 TIMEMM PIC 99.
             10 TIMESS PIC 99.

         LOCAL-STORAGE SECTION.
         01 USER-SELECTION PIC 9 VALUE ZERO.
         01 IID-SELECTION PIC 9(4) VALUE ZERO.
         01 LS-TOTAL PIC S9(7)V99 VALUE ZERO.
         01 LS-MENU.
           05 LS-KEY1DATE PIC X(14) VALUE "TRANSACTIONS".
           05 LS-KEY2ACCOUNT PIC X(15) VALUE "ACCOUNT".
           05 LS-ACCOUNTNAME PIC X(22) VALUE "ACCOUNT NAME".
           05 LS-TRANSACTIONDESCRIPTION PIC X(40) VALUE "DESCRIPTION".
           05 LS-TRANSACTIONAMMOUNT PIC X(12) VALUE "AMMOUNT".




       PROCEDURE DIVISION.
       MAIN-PROGRAM.
       0000SELECTIONSTART.
         MOVE 0 TO USER-SELECTION.
         ACCEPT CURRENTDATE FROM DATE yyyymmdd.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY "Cobol Simple Silver 0.1 "YYYY"-"MM"-"DD.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "MENU"
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "    1 : Account"
           DISPLAY "    2 : Information"
           DISPLAY "    3 : Change Currency"
           DISPLAY "    4 : Delete Database"
           DISPLAY "    5 : Exit Application"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000SELECTIONACCOUNT
             WHEN 2 PERFORM 0000SELECTIONINFO
             WHEN 3 GO TO 0000SELECTIONCHANGECURRENCY
             WHEN 4 GO TO 0000SELECTIONDELETEALL
             WHEN 5 GO TO 0000SELECTIONQUIT
             WHEN OTHER PERFORM 0000SELECTIONSTARTERROR
           END-EVALUATE
         END-PERFORM.

       0000SELECTIONSTARTERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONSTART.
       
       0000SELECTIONACCOUNT.
         ACCEPT CURRENTDATE FROM DATE yyyymmdd.
         MOVE 0 TO USER-SELECTION.

         OPEN INPUT INFILE.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILE INTO WS-INFILEFD
             AT END MOVE 1 TO WS-ENDOFFILE
           END-READ    
         END-PERFORM.
         CLOSE INFILE.
         MOVE 0 TO WS-ENDOFFILE.

         OPEN INPUT INFILETOTAL.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILETOTAL INTO WS-INFILETOTALFD
             KEY IS KEY1TOTAL
             AT END MOVE 1 TO WS-ENDOFFILE

           END-READ    
         END-PERFORM.
         CLOSE INFILETOTAL.
         MOVE 0 TO WS-ENDOFFILE.
         
         DISPLAY " ".
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY "Account History "YYYY"-"MM"-"DD" "
                 "Account: "KEY2ACCOUNT" "
                 "Name: "ACCOUNTNAME" "
                 "Account Total: "WS-KEY1TOTALTOTAL" "
                  WS-KEY1TOTALCURRENCY.
         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".
         DISPLAY 
           LS-KEY1DATE" | "
           LS-KEY2ACCOUNT" | "
           LS-ACCOUNTNAME" | "
           LS-TRANSACTIONDESCRIPTION" | "
           LS-TRANSACTIONAMMOUNT" | "


         DISPLAY "-----------------------------------------------------"
                 "-----------------------------------------------------"
                 "-----------".

         OPEN INPUT INFILE.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILE INTO WS-INFILEFD
             KEY IS KEY1DATE
             AT END MOVE 1 TO WS-ENDOFFILE
             NOT AT END

               DISPLAY 
                 WS-KEY1DATE " | "
                 WS-KEY2ACCOUNT " | "
                 WS-ACCOUNTNAME " | "
                 WS-TRANSACTIONDESCRIPTION " | "
                 WS-TRANSACTIONAMMOUNT " | "


           END-READ    
         END-PERFORM.
         CLOSE INFILE.
         MOVE 0 TO WS-ENDOFFILE.

         PERFORM UNTIL USER-SELECTION>0
           DISPLAY " "
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "MENU"
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------"
           DISPLAY "    1 : Add Transaction"
           DISPLAY "    2 : Edit Transaction"
           DISPLAY "    3 : Delete Transaction"
           DISPLAY "    4 : Go To Main Menu"
           DISPLAY "    5 : Exit Application"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000SELECTIONADD
             WHEN 2 PERFORM 0000SELECTIONEDIT
             WHEN 3 PERFORM 0000SELECTIONDELETE
             WHEN 4 PERFORM 0000SELECTIONSTART
             WHEN 5 GO TO 0000SELECTIONQUIT
             WHEN OTHER PERFORM 0000SELECTIONCONTACTSERROR
           END-EVALUATE
         END-PERFORM.

       0000SELECTIONCONTACTSERROR.

         DISPLAY " ".
         DISPLAY "!ERROR WRONG INPUT!".
         GO TO 0000SELECTIONACCOUNT.

       0000SELECTIONADD.
         MOVE 0 TO USER-SELECTION.
         OPEN I-O INFILE.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILE INTO WS-INFILEFD
             AT END MOVE 1 TO WS-ENDOFFILE
           END-READ    
         END-PERFORM.
         CLOSE INFILE.
         OPEN I-O INFILETOTAL.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILETOTAL INTO WS-INFILETOTALFD
             AT END MOVE 1 TO WS-ENDOFFILE
           END-READ    
         END-PERFORM.
         CLOSE INFILETOTAL.
         ACCEPT CURRENTDATE FROM DATE yyyymmdd.
         ACCEPT CURRENTTIME FROM TIME.
         MOVE DATEANDTIME TO WS-KEY1DATE.
         DISPLAY " ".
         DISPLAY "Enter TRANSACTION DESCRIPTION:".
         ACCEPT WS-TRANSACTIONDESCRIPTION.
         DISPLAY "Enter TRANSACTION AMMOUNT:".
         ACCEPT WS-TRANSACTIONAMMOUNT.

         MOVE WS-TRANSACTIONDESCRIPTION TO TRANSACTIONDESCRIPTION.
         MOVE WS-TRANSACTIONAMMOUNT TO TRANSACTIONAMMOUNT.

         MOVE WS-KEY1DATE TO KEY1DATE.
         MOVE WS-KEY2ACCOUNT TO KEY2ACCOUNT.


         COMPUTE KEY1TOTALTOTAL = 
                 WS-KEY1TOTALTOTAL + WS-TRANSACTIONAMMOUNT.

         OPEN I-O INFILE.
           WRITE INFILEFD
             INVALID KEY DISPLAY
               "!ERROR RECORD ALREADY EXIST!"
             NOT INVALID KEY DISPLAY 
               "Item Added."
           END-WRITE.
         CLOSE INFILE.

         OPEN I-O INFILETOTAL.
           REWRITE INFILETOTALFD
             INVALID KEY DISPLAY
               "!ERROR RECORD ALREADY EXIST!"
             NOT INVALID KEY DISPLAY 
               "Item Added."
           END-REWRITE.
         CLOSE INFILETOTAL.

         PERFORM 0000SELECTIONACCOUNT.

       0000SELECTIONEDIT.
         MOVE 0 TO USER-SELECTION.

         DISPLAY " ".
         DISPLAY "Enter Transaction Edit:".
         ACCEPT WS-KEY1DATE.

         MOVE WS-KEY1DATE TO KEY1DATE.

         OPEN I-O INFILE.
           READ INFILE INTO WS-INFILEFD
             KEY IS KEY1DATE
             INVALID KEY
               DISPLAY "!ERROR DOSE NOT EXIST!"
               PERFORM 0000SELECTIONEDITERROR2
           END-READ.
         CLOSE INFILE.

         OPEN I-O INFILETOTAL.
           READ INFILETOTAL INTO WS-INFILETOTALFD

             INVALID KEY
               DISPLAY "!ERROR DOSE NOT EXIST!"
               PERFORM 0000SELECTIONEDITERROR2
           END-READ.
         CLOSE INFILETOTAL.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "-----------"
         DISPLAY "MENU"
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "-----------"
         DISPLAY "    1 : Edit Description"
         DISPLAY "    2 : Edit AMMOUNT"
         DISPLAY "    3 : Cancel Edit"
         DISPLAY "Select number and press Enter: "
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 PERFORM 0000SELECTIONEDITDESCRIPTION
           WHEN 2 PERFORM 0000SELECTIONEDITTRANSACTION
           WHEN 3 GO TO 0000SELECTIONACCOUNT
           WHEN OTHER PERFORM 0000SELECTIONEDITERROR
         END-EVALUATE
       END-PERFORM.

         0000SELECTIONEDITDESCRIPTION.
           DISPLAY " ".
           DISPLAY "New DESCRIPTION:"
           ACCEPT WS-TRANSACTIONDESCRIPTION.
           GO TO 0000CONTINUEEDIT.

         0000SELECTIONEDITTRANSACTION.
           DISPLAY " ".
           COMPUTE 
             KEY1TOTALTOTAL = WS-KEY1TOTALTOTAL - WS-TRANSACTIONAMMOUNT.
           DISPLAY "New AMMOUNT:"
           ACCEPT WS-TRANSACTIONAMMOUNT.
           GO TO 0000CONTINUEEDIT.

         0000CONTINUEEDIT.

         OPEN I-O INFILE.
           MOVE WS-KEY1DATE TO KEY1DATE.
           MOVE WS-KEY2ACCOUNT TO KEY2ACCOUNT.
           MOVE WS-ACCOUNTNAME TO ACCOUNTNAME.
           MOVE WS-TRANSACTIONDESCRIPTION TO TRANSACTIONDESCRIPTION.
           MOVE WS-TRANSACTIONAMMOUNT TO TRANSACTIONAMMOUNT.

           REWRITE INFILEFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Item Updated."
           END-REWRITE.
         CLOSE INFILE.

         OPEN I-O INFILETOTAL.


           COMPUTE 
             KEY1TOTALTOTAL = WS-KEY1TOTALTOTAL + WS-TRANSACTIONAMMOUNT.

           REWRITE INFILETOTALFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Item Updated."
           END-REWRITE.
         CLOSE INFILETOTAL.

       GO TO 0000SELECTIONACCOUNT.

       0000SELECTIONEDITERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONEDIT.

       0000SELECTIONEDITERROR2.

         CLOSE INFILE.
         GO TO 0000SELECTIONACCOUNT.

       0000SELECTIONDELETE.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "---------------".
         DISPLAY "Enter TRANSACTION To Be Deleted:".
         ACCEPT WS-KEY1DATE.

         MOVE WS-KEY1DATE TO KEY1DATE.

         OPEN I-O INFILE.
           READ INFILE INTO WS-INFILEFD
             KEY IS KEY1DATE
             INVALID KEY
               DISPLAY "!ERROR PART NUMBER DOSE NOT EXIST!"
               PERFORM 0000SELECTIONDELETEERROR2
           END-READ.
         CLOSE INFILE.


         OPEN I-O INFILETOTAL.
           READ INFILETOTAL INTO WS-INFILETOTALFD

             INVALID KEY
               DISPLAY "!ERROR DOSE NOT EXIST!"
               PERFORM 0000SELECTIONEDITERROR2
           END-READ.
         CLOSE INFILETOTAL.


         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "Are you sure that you want to delete this item?"
           DISPLAY "    1 : Yes I want to delete this item"
           DISPLAY "    2 : No!"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000CONTINUEDELETE
             WHEN 2 PERFORM 0000SELECTIONACCOUNT
             WHEN OTHER PERFORM 0000SELECTIONDELETEERROR
           END-EVALUATE
         END-PERFORM.

         0000CONTINUEDELETE.

         OPEN I-O INFILE.
         DELETE INFILE
           INVALID KEY DISPLAY "!ERROR CONTACT DOSE NOT EXIST!"
           NOT INVALID KEY DISPLAY "Item Deleted."
         END-DELETE.
         CLOSE INFILE.

         OPEN I-O INFILETOTAL.


           COMPUTE 
             KEY1TOTALTOTAL = WS-KEY1TOTALTOTAL - WS-TRANSACTIONAMMOUNT.

           REWRITE INFILETOTALFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Item Updated."
           END-REWRITE.
         CLOSE INFILETOTAL.

       GO TO 0000SELECTIONACCOUNT.

       0000SELECTIONDELETEERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETE.

       0000SELECTIONDELETEERROR2.

         CLOSE INFILE.
         GO TO 0000SELECTIONDELETE.

       0000SELECTIONDELETEALL.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
           DISPLAY "---------------------------------------------------"
                   "---------------------------------------------------"
                   "---------------".
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "Are you sure that you want to DELETE ALL items?"
           DISPLAY "    1 : Yes I want to DELETE ALL item."
           DISPLAY "    2 : No!"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000CONTINUEDELETEALL
             WHEN 2 PERFORM 0000SELECTIONACCOUNT
             WHEN OTHER PERFORM 0000SELECTIONDELETEALLERROR
           END-EVALUATE
         END-PERFORM.

       0000CONTINUEDELETEALL.

         DELETE FILE
           INFILE
         END-DELETE.

         OPEN OUTPUT INFILE

           ACCEPT CURRENTDATE FROM DATE yyyymmdd.
           ACCEPT CURRENTTIME FROM TIME.
           MOVE DATEANDTIME TO KEY1DATE.
           MOVE 1 TO KEY2ACCOUNT.
           MOVE "SAVINGS" TO ACCOUNTNAME.
           MOVE "EXAMPLE TRANSACTION" TO TRANSACTIONDESCRIPTION.
           MOVE 0 TO TRANSACTIONAMMOUNT.

           
           WRITE INFILEFD
           END-WRITE.
         CLOSE INFILE.

         DELETE FILE
           INFILETOTAL
         END-DELETE.

         OPEN OUTPUT INFILETOTAL

           MOVE 1 TO KEY1TOTAL.
           MOVE "SEK" TO KEY1TOTALCURRENCY.
           MOVE 0 TO KEY1TOTALTOTAL.
           
           WRITE INFILETOTALFD
           END-WRITE.
         CLOSE INFILETOTAL.

         GO TO 0000SELECTIONSTART.

       0000SELECTIONDELETEALLERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETEALL.

       0000SELECTIONCHANGECURRENCY.
         MOVE 0 TO USER-SELECTION.

         OPEN INPUT INFILETOTAL.
         PERFORM UNTIL WS-ENDOFFILE = 1
           READ INFILETOTAL INTO WS-INFILETOTALFD
             KEY IS KEY1TOTAL
             AT END MOVE 1 TO WS-ENDOFFILE
     
           END-READ    
         END-PERFORM.
         CLOSE INFILETOTAL.
         MOVE 0 TO WS-ENDOFFILE.

         DISPLAY " ".
         DISPLAY "New Currency:".
         ACCEPT WS-KEY1TOTALCURRENCY.

         MOVE WS-KEY1TOTALCURRENCY TO KEY1TOTALCURRENCY.

         OPEN I-O INFILETOTAL
           REWRITE INFILETOTALFD

             INVALID KEY DISPLAY"!ERROR!"
             NOT INVALID KEY DISPLAY "Currency Changed."
           END-REWRITE.
         CLOSE INFILETOTAL.

       GO TO 0000SELECTIONSTART.

       0000SELECTIONINFO.
         MOVE 0 TO USER-SELECTION.
         DISPLAY "---------------------------------------------------"
                 "---------------------------------------------------"
                 "---------------"
         OPEN INPUT INFILE-APPINFO.
           PERFORM UNTIL WS-ENDOFFILE = 1
             READ INFILE-APPINFO INTO WS-APPINFOFILE
               AT END MOVE 1 TO WS-ENDOFFILE
               NOT AT END DISPLAY WS-APPINFOFILE
             END-READ
           END-PERFORM.
         CLOSE INFILE-APPINFO.
         MOVE 0 TO WS-ENDOFFILE.

         GO TO 0000SELECTIONSTART.
       
       0000SELECTIONQUIT.
       STOP-RUN.
