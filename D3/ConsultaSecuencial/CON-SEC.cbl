      *MI PROGRAMA DE CONSULTA DE ARCHIVOS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CON-SEC.
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
       01 WKS-CLIENTES-LOOP PIC 9 VALUE 0.
       
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
           READ CLIENTES AT END MOVE 1 TO WKS-CLIENTES-LOOP.
           
       2000-PROCESO.
           DISPLAY REG-CLI.
           PERFORM 1001-LEER-ARCHIVO.
       
       3000-FIN.
           CLOSE CLIENTES.
  