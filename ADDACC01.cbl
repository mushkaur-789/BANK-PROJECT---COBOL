       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDACC01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BNK-ACCT-RPT-FILE
               ASSIGN TO "BNK_ACCT_RPT.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT BNK-ACCT-MSTR-FILE
               ASSIGN TO "ACCOUNTS.dat"
               ORGANIZATION IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  BNK-ACCT-RPT-FILE.
       01  BNK-ACCT-RPT-REC            PIC X(132).

       FD  BNK-ACCT-MSTR-FILE.
       01  BNK-ACCT-MSTR-REC.
           05  CUST-NAME               PIC X(15).
           05  CUST-SURNAME            PIC X(20).
           05  CUST-DOB                PIC 9(8).
           *> YYYYMMDD, standard banking format
           05  CUST-PHONE              PIC X(15).
           *> allows +44, formatting, spaces
           05  ACCT-NUMBER             PIC 9(5).
           *> unique account number
           05  ACCT-TYPE               PIC X(16).
           *> e.g., saving, deposit, junior...
           05  ACCT-BALANCE            PIC -9(10).99.
           05  ACCT-CURRNCY            PIC X(3) VALUE "GBP".

       WORKING-STORAGE SECTION.
       01  WS-ACCT-MSTR-REC.
           05  WS-CUST-NAME            PIC X(15).
           05  WS-CUST-SURNAME         PIC X(20).
           05  WS-CUST-DOB.
               10  WS-YEAR             PIC 9(4).
               10  WS-MONTH            PIC 99.
               10  WS-DAY              PIC 99.
           05  WS-CUST-PHONE           PIC X(15).
           05  WS-ACCT-NUMBER          PIC 9(5).
           05  WS-ACCT-TYPE            PIC X(15).
           05  WS-ACCT-BALANCE-DISP    PIC -Z(10).99.

       01  WS-ACCT-BALANCE             PIC S9(10)V99 COMP-3.
       01  CHECK-ENTRY                 PIC X.
           88  ENTRY-RIGHT             VALUE "Y".
           88  ENTRY-WRONG             VALUE "N".

      *  01  WS-TEMP-FIELD               PIC X(35).
       01  WS-INVALID-NAME-NUM         PIC 99.
       01  WS-INVALID-NAME-UPP         PIC 99.
       01  WS-INVALID-NAME-LOW         PIC 99.
       01  WS-VALID-CHAR-COUNT         PIC 99.
       01  WS-VALID-CHAR-COUNT         PIC 99.
       01  WS-VALID-CHAR-COUNT         PIC 99.
       01  WS-VALID-CHAR-COUNT         PIC 99.
       01  WS-VALID-CHAR-COUNT         PIC 99.


       01  WS-TEMP-LENGHT              PIC 9(3).
       01  WS-RIGHT-LENGHT             PIC 9(3).
       01  WS-DIFF-CHAR                PIC 9(3).

       01  WS-Q                        PIC 9(3).99.
       01  WS-R1                       PIC Z9.99.
       01  WS-R2                       PIC Z9.99.
       01  WS-R3                       PIC Z9.99.
       01  LEAP-YEAR                   PIC X.
           88  IS-LEAP-YEAR            VALUE "Y".
           88  NOY-LEAP-YEAR           VALUE "N".

       01  CHARACTERS-LIST             PIC X(65).
           88  UPPERCASE-ALPHA         VALUE "A" THRU "Z".
           88  LOWERCASE-ALPHA         VALUE "a" THRU "z".
           88  NUMBER-LIST             VALUE "0" THRU "9".

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
       MAIN-LOGIC.
           DISPLAY "CREATE BANK ACCOUNT".
           PERFORM GET-CUST-INFO.
       PROGRAM-END.
           STOP RUN.

      *---------------------------------------------------------------

       GET-CUST-INFO.
           DISPLAY "------------CUSTOMER INFORMATION------------".
           PERFORM INITIALISE-CHECK-ENTRY.
           PERFORM GET-NAME UNTIL ENTRY-RIGHT.

       INITIALISE-CHECK-ENTRY.
           MOVE "N" TO CHECK-ENTRY.

       GET-NAME.
           DISPLAY "NAME: ".
           ACCEPT WS-CUST-NAME.
           DISPLAY "SURNAME: ".
           ACCEPT WS-CUST-SURNAME.
           PERFORM CHECK-NAME.

       CHECK-NAME.
           IF WS-CUST-NAME = SPACES OR  WS-CUST-SURNAME = SPACES
               DISPLAY "NO SPACE ALLOWED."
               MOVE "N" TO CHECK-ENTRY
           ELSE
               MOVE ZERO TO WS-INVALID-CHAR-COUNT
               INSPECT WS-CUST-NAME
                   TALLYING WS-INVALID-CHAR-COUNT FOR ALL NUMBER-LIST
                            WS-VALID-CHAR-COUNT FOR ALL UPPERCASE-ALPHA
                            WS-VALID-CHAR-COUNT FOR ALL LOWERCASE-ALPHA
               INSPECT WS-CUST-SURNAME
                   TALLYING WS-INVALID-CHAR-COUNT FOR ALL NUMBER-LIST
                            WS-VALID-CHAR-COUNT FOR ALL UPPERCASE-ALPHA
                            WS-VALID-CHAR-COUNT FOR ALL LOWERCASE-ALPHA
           END-IF.

           INSPECT FUNCTION REVERSE(WS-CUST-NAME)
               TALLYING WS-TEMP-LENGHT FOR LEADING SPACES.
           SUBTRACT WS-TEMP-LENGHT FROM FUNCTION LENGTH(WS-CUST-NAME)
               GIVING WS-RIGHT-LENGHT.
           COMPUTE WS-DIFF-CHAR = WS-RIGHT-LENGHT - WS-VALID-CHAR-COUNT.

           IF WS-INVALID-CHAR-COUNT > 0 OR WS-RIGHT-LENGHT NOT = 0
               DISPLAY "INVALID INPUT. TRY AGAIN"
               MOVE "N" TO CHECK-ENTRY
           ELSE MOVE "Y" TO CHECK-ENTRY
           END-IF.
