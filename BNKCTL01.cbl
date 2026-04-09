       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNKCTL01.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-ACTION-CODE              PIC X.
       01  WS-VALID-CODE               PIC X.

       PROCEDURE DIVISION.
       PROGRAM-START.
       MAIN-LOGIC.
           MOVE "N" TO WS-VALID-CODE.
           PERFORM DISPLAY-MENU
               UNTIL WS-VALID-CODE = "Y".
           PERFORM DISPATCH-ACTION.

       PROGRAM-END.
           STOP RUN.
      *----------------------------------------------------------------
       DISPLAY-MENU.
           DISPLAY "DEAR COSTUMER MAKE A CHOICE".
           DISPLAY " ".
           DISPLAY "1 --> CREATE BANK ACCOUNT".
           DISPLAY "2 --> MAKE PAYMENTS".
           DISPLAY "0 --> END PROGRAM ".
           DISPLAY " ".
           DISPLAY "YOUR CHOICE: ".
           ACCEPT WS-ACTION-CODE.
           PERFORM CHK-ACTION-CODE.

       CHK-ACTION-CODE.
           IF WS-ACTION-CODE NOT NUMERIC
               MOVE "N" TO WS-VALID-CODE.

           IF WS-ACTION-CODE < "0" OR WS-ACTION-CODE > "2"
               MOVE "N" TO WS-VALID-CODE
           ELSE MOVE "Y" TO WS-VALID-CODE.

           IF WS-VALID-CODE = "N"
               DISPLAY "INVALID OPTION - TRY AGAIN".

       DISPATCH-ACTION.
           EVALUATE WS-ACTION-CODE
               WHEN 1
                   CALL 'ADDACC01'
               WHEN 2
                   CALL 'ATMTRX01'
               WHEN 0
                   PERFORM PROGRAM-END
           END-EVALUATE.
