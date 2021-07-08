      *MI PROGRAMA DE BUSQUEDA EN ARCHIVO DE FORMA DINAMICA
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUS-INDD.
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
       01 WKS-ID-TARGETA        PIC 99.
       01 WKS-CLIENTES-RES      PIC A(01).
       
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
           DISPLAY "INTRODUSCA EL ID DE TARGETA:" LINE 03 POSITION 01.
           ACCEPT WKS-ID-TARGETA LINE 03 POSITION 32.
           
           MOVE WKS-ID-TARGETA TO ID-TARGETA-D.
           
           START DCLIENTES KEY IS = ID-TARGETA-D INVALID KEY 
                                            MOVE 1 TO WKS-CLIENTES-LOOP.
           
           IF WKS-CLIENTES-LOOP = 1
               DISPLAY "NO EXISTE REGISTROS"
           ELSE 
                MOVE 0 TO WKS-CLIENTES-LOOP
                PERFORM 2001-BUSCAR UNTIL WKS-CLIENTES-LOOP = 1.
           
           DISPLAY "DESEAS BUSCAR OTRO S/N".
           ACCEPT WKS-CLIENTES-RES.
            
           IF WKS-CLIENTES-RES = "N"
                MOVE 1 TO WKS-CLIENTES-LOOP
           ELSE
                MOVE 0 TO WKS-CLIENTES-LOOP.
        
        2001-BUSCAR.
           READ DCLIENTES NEXT AT END MOVE 1 TO WKS-CLIENTES-LOOP.
           
           IF WKS-CLIENTES-LOOP = 0 
                IF WKS-ID-TARGETA = ID-TARGETA-D
                    DISPLAY REG-CLI-D
                ELSE
                    MOVE 1 TO WKS-CLIENTES-LOOP.
          

                       
       3000-FIN.
           CLOSE DCLIENTES.
  