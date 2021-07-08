      *MI PROGRAMA DE BUSQUEDA EN ARCHIVO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUS-SEC.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
        
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLI.
        03 ID-CLIENTE       PIC 9(04).
        03 NOMBRE-CLIENTE   PIC A(20).
        03 ID-TARGETA       PIC 99.
        03 TIPO-TARG        PIC A(10).
        03 ANUALIDAD        PIC 9(04)V99.
        03 FECHA-ALTA       PIC 9(08).
        
       WORKING-STORAGE SECTION.
       01 WKS-CLIENTES-LOOP     PIC 9 VALUE 0.
       01 WKS-ID-CLIENTE        PIC 9(04).
       01 WKS-CLIENTES-RES      PIC A(01).
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-CLIENTES-LOOP = 1.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           OPEN INPUT CLIENTES.
           PERFORM 1001-LEER-ARCHIVO.
           
       1001-LEER-ARCHIVO.
           READ CLIENTES AT END 
                MOVE 1 TO WKS-CLIENTES-LOOP 
                DISPLAY "NO EXISTE REGISTRO" 
                                        LINE 04 POSITION 6.
           
       
           
       2000-PROCESO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           DISPLAY "INTRODUSCA SU ID DE CLIENTE A BUSCAR" LINE 03 
                                                            POSITION 01.
           ACCEPT WKS-ID-CLIENTE LINE 03 POSITION 42.
           
           PERFORM 2001-BUSCAR UNTIL WKS-CLIENTES-LOOP = 1.
           
           DISPLAY "DESEAS BUSCAR OTRO CLIENTE S/N" LINE 05 POSITION 1.
           ACCEPT WKS-CLIENTES-RES LINE 05 POSITION 32.
            
           IF WKS-CLIENTES-RES = "N"
                MOVE 1 TO WKS-CLIENTES-LOOP
           ELSE
                MOVE 0 TO WKS-CLIENTES-LOOP
                PERFORM 3000-FIN
                PERFORM 1000-INICIO.
        
        2001-BUSCAR.
           IF WKS-ID-CLIENTE = ID-CLIENTE
                DISPLAY REG-CLI LINE 04 POSITION 6
                MOVE 1 TO WKS-BUSQUEDA-LOOP
           ELSE
                PERFORM 1001-LEER-ARCHIVO.

                       
       3000-FIN.
           CLOSE CLIENTES.
  