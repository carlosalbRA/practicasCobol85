      *MI PROGRAMA DE BUSQUEDA EN ARCHIVO DE FORMA DINAMICA Y  MUESTRA
      *TODOS SECUENCIALMENTE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUS-DS.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DCLIENTES ASSIGN TO DISK INDEXED ACCESS MODE IS
                DYNAMIC 
                RECORD KEY IS ID-CLIENTE-D
                ALTERNATE RECORD KEY IS ID-TARGETA-D WITH DUPLICATES
                ALTERNATE RECORD KEY IS FECHA-ALTA-D WITH DUPLICATES.
        
       DATA DIVISION.
       FILE SECTION.
       FD DCLIENTES.
       01 REG-CLI-D.
        03 ID-CLIENTE-D       PIC 9(04).
        03 NOMBRE-CLIENTE-D   PIC A(20).
        03 ID-TARGETA-D       PIC 99.
        03 TIPO-TARG-D        PIC A(10).
        03 ANUALIDAD-D        PIC 9(04)V99.
        03 FECHA-ALTA-D       PIC 9(08).
        
       WORKING-STORAGE SECTION.
       01 WKS-CLIENTES-LOOP     PIC 9 VALUE 0.
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-CLIENTES-LOOP = 1.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           OPEN INPUT DCLIENTES.
           PERFORM 1001-LEER-ARCHIVO.
           
       1001-LEER-ARCHIVO.
           READ DCLIENTES NEXT AT END MOVE 1 TO WKS-CLIENTES-LOOP.
           
       2000-PROCESO.
           DISPLAY REG-CLI-D.
           PERFORM 1001-LEER-ARCHIVO.
       
       3000-FIN.
           CLOSE DCLIENTES.
  