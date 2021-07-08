      *MI PROGRAMA DE ELIMINAR REGISTROS DE ARCHIVOS INDEXADOS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUS-RAN.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ICLIENTES ASSIGN TO DISK INDEXED ACCESS MODE IS
                                   RANDOM RECORD KEY IS ID-CLIENTE-I.
        
       DATA DIVISION.
       FILE SECTION.
       FD ICLIENTES.
       01 REG-CLI-I.
        03 ID-CLIENTE-I       PIC 9(04).
        03 NOMBRE-CLIENTE-I   PIC A(20).
        03 ID-TARGETA-I       PIC 99.
        03 TIPO-TARG-I        PIC A(10).
        03 ANUALIDAD-I        PIC 9(04)V99.
        03 FECHA-ALTA-I       PIC 9(08).
        
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
           OPEN I-O ICLIENTES.

       2000-PROCESO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           
           DISPLAY "INGRESE EL ID DEL CLIENTE A BUSCAR:" LINE 03 
                                                            POSITION 03.
           ACCEPT ID-CLIENTE-I LINE 03 POSITION 39.
           
           READ ICLIENTES INVALID KEY MOVE 1 TO WKS-INVALID-KEY.
           
           IF WKS-INVALID-KEY = 1
                DISPLAY "NO EXISTE REGISTRO" LINE 04 POSITION 03
           ELSE 
                DISPLAY REG-CLI-I LINE 04 POSITION 03
                PERFORM 20001-ELIMINAR-REG.
                
                
           DISPLAY "¿DESEA BUSCAR OTRO CLIENTE? S/N :" LINE 07 
                                                            POSITION 03.
           ACCEPT WKS-RES LINE 07 POSITION 39.
           
           IF WKS-RES = "N"
                MOVE 1 TO WKS-CLIENTES-LOOP
           ELSE 
                MOVE 0 TO WKS-INVALID-KEY.
       
       20001-ELIMINAR-REG.
           DISPLAY "¿DESEA ELIMIAR EL REGISTRO S/N?" LINE 05 POSITION 03
           ACCEPT WKS-RES LINE 05 POSITION 36.
           
           IF WKS-RES = "S"
            DELETE ICLIENTES
            DISPLAY "ELIMINADO EXITOSAMENTE" LINE 06 POSITION 03.
       
       3000-FIN.
           CLOSE ICLIENTES.
  