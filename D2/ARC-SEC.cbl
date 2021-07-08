      *MI PROGRAMA DE ARCHIVOS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARC-SEC.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
        
       DATA DIVISION.
       FILE SECTION.
       FD clientes.
       01 REG-CLI.
        03 ID-CLIENTE       PIC 9(04).
        03 NOMBRE-CLIENTE   PIC A(20).
        03 ID-TARGETA       PIC 99.
        03 TIPO-TARG        PIC A(10).
        03 ANUALIDAD        PIC 9(04)V99.
        03 FECHA-ALTA       PIC 9(08).
        
       WORKING-STORAGE SECTION.
       01 WKS-DEBITO        PIC A(10) VALUE "DEBITO".
       01 WKS-CREDITO       PIC A(10) VALUE "CREDITO".
       01 WKS-NOMINA        PIC A(10) VALUE "NOMINA".
       01 WKS-ANUALIDAD-D   PIC 9(04)V99 VALUE 550.
       01 WKS-ANUALIDAD-C   PIC 9(04)V99 VALUE 2500.
       01 WKS-ANUALIDAD-N   PIC 9(04)V99 VALUE 0.
       01 WKS-CLIENTES-LOOP PIC 9 VALUE 0.
       01 WKS-CLIENTES-RES PIC A(01).
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-CLIENTES-LOOP = 1.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           OPEN EXTEND CLIENTES.
           
       2000-PROCESO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           DISPLAY "INTRODUSCA SU ID DE CLIENTE:" LINE 03 POSITION 10.
           ACCEPT ID-CLIENTE LINE 03 POSITION 42.
           
           DISPLAY "INTRODUSCA SU NOMBRE:" LINE 04 POSITION 10.
           ACCEPT NOMBRE-CLIENTE LINE 04 POSITION 42.
           
           DISPLAY "INTRODUSCA SU ID DE TARGETA: " LINE 05 POSITION 10.
           ACCEPT ID-TARGETA LINE 05 POSITION 42.
           
           DISPLAY "INTRODUSCA SU FECHA DE ALTA:" LINE 06 POSITION 10.
           ACCEPT FECHA-ALTA LINE 06 POSITION 42.
           
           
           IF ID-TARGETA = 1
               MOVE WKS-DEBITO      TO TIPO-TARG
               MOVE WKS-ANUALIDAD-D TO ANUALIDAD
           ELSE
                IF ID-TARGETA = 2
                    MOVE WKS-CREDITO     TO TIPO-TARG
                    MOVE WKS-ANUALIDAD-C TO ANUALIDAD
                ELSE
                    IF ID-TARGETA = 3
                        MOVE WKS-NOMINA     TO TIPO-TARG
                        MOVE WKS-ANUALIDAD-N TO ANUALIDAD
                
            .
            
            WRITE REG-CLI.
           
            DISPLAY "DESEA AGREGAR OTRO CLIENTE S/N" LINE 07 POSITION 10.
            ACCEPT WKS-CLIENTES-RES LINE 07 POSITION 42.
            
            IF WKS-CLIENTES-RES = "N"
                MOVE 1 TO WKS-CLIENTES-LOOP.
           
       
       3000-FIN.
           CLOSE CLIENTES.
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           