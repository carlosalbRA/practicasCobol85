      *MI PROGRAMA DE CONSULTA DE ARCHIVOS DINAMICOS CON BUSQUEDA 
      *RANDOM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIN3.
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
       01 WKS-CLIENTES-LOOP PIC 9 VALUE 0.
       01 WKS-INVALID-KEY   PIC 9 VALUE 0.
       01 WKS-RES           PIC A.
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-CLIENTES-LOOP = 1.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           OPEN INPUT DCLIENTES.

       2000-PROCESO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           
           DISPLAY "INGRESE EL ID DEL CLIENTE A BUSCAR:" LINE 03 
                                                            POSITION 03.
           ACCEPT ID-CLIENTE-D LINE 03 POSITION 39.
           
           READ DCLIENTES INVALID KEY MOVE 1 TO WKS-INVALID-KEY.
           
           IF WKS-INVALID-KEY = 1
                DISPLAY "NO EXISTE REGISTRO" LINE 04 POSITION 03
           ELSE 
                DISPLAY REG-CLI-D. 
                
           DISPLAY "¿DESEA BUSCAR OTRO CLIENTE? S/N :" LINE 05 
                                                            POSITION 03.
           ACCEPT WKS-RES LINE 05 POSITION 39.
           
           IF WKS-RES = "N"
                MOVE 1 TO WKS-CLIENTES-LOOP
           ELSE 
                MOVE 0 TO WKS-INVALID-KEY.
           
       
       3000-FIN.
           CLOSE DCLIENTES.
  