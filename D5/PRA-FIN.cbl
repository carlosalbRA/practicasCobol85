      *PRACTICA FINALL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRA-FIN.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DCLIENTE ASSIGN TO DISK INDEXED ACCESS MODE IS
                DYNAMIC 
                RECORD KEY IS ID-CLIENTE-D
                ALTERNATE RECORD KEY IS ID-TARJETA-D WITH DUPLICATES
                ALTERNATE RECORD KEY IS FECHA-ALTA-D WITH DUPLICATES.
		   
		   SELECT REP-CLI ASSIGN TO PRINTER.
		   SELECT REP-TAR ASSIGN TO PRINTER.
        
       DATA DIVISION.
       FILE SECTION.
       FD DCLIENTE.
       01 REG-CLI-D.
        03 ID-CLIENTE-D       PIC 9(04).
        03 NOMBRE-CLIENTE-D   PIC A(20).
        03 ID-TARJETA-D       PIC 99.
        03 TIPO-TARG-D        PIC A(10).
        03 ANUALIDAD-D        PIC 9(04)V99.
        03 FECHA-ALTA-D       PIC 9(08).
		
	   FD REP-CLI.
	   01  LINEA                 PIC X(132).
	   
	   FD REP-TAR.
	   01 LINEA-TAR              PIC X(132).
        
       WORKING-STORAGE SECTION.
       01 WKS-CLIENTES-LOOP           PIC 9 VALUE 0.
	   01 WKS-DETALLES-COUNT          PIC 9999 VALUE 0.
	   01 WKS-CLIENTES-COUNT    PIC 9999 VALUE 0.
	   01 WKS-ANUALIDA-PAR            PIC 9(07)V99.
	   01 WKS-ANUALIDAD-TOTAL         PIC 9(07)V99.
	   01 WKS-RES					  PIC X(01).
	   01 WKS-CORTE					  PIC 9 VALUE 0.
	   
	   01 HEADER-L3-R2.
		  03 FILLER           PIC X(05) VALUE SPACES.
		  03 FILLER           PIC X(15) VALUE "ID DE TARJETA: ".
		  03 WKS-ID-TARJETA   PIC 99.
			
	   01 HEADER-L1-R.
			03 FILLER PIC X(40) VALUE SPACES.
			03 FILLER PIC X(19) VALUE "REPORTE DE CLIENTES".
	   
	   01 HEADER-L1-R2.
			03 FILLER PIC X(40) VALUE SPACES.
			03 FILLER PIC X(19) VALUE "REPORTE DE TARJETAS".
			
	   01 HEADER-L2-R.
			03 FILLER 		 PIC X(05) VALUE SPACES.
			03 FILLER 		 PIC X(18) VALUE "FECHA DE PROCESO: ".
			03 WKS-FECHA-PRO PIC 99/99/99.
            03 FILLER 		 PIC X(50) VALUE SPACES.
			03 FILLER 		 PIC X(05) VALUE "PAG: ".
			03 WKS-PAG 		 PIC 99 VALUE 1.
	   
	   01 HEADER-CAMPOS-R1.
			03 FILLER PIC X(05) VALUE SPACES.
			03 FILLER PIC X(10) VALUE "ID-CLIENTE".
			03 FILLER PIC X(10) VALUE SPACES.
			03 FILLER PIC X(06) VALUE "NOMBRE".
			03 FILLER PIC X(15) VALUE SPACES.
			03 FILLER PIC X(15) VALUE "TIPO DE TARJETA".
			03 FILLER PIC X(05) VALUE SPACES.
			03 FILLER PIC X(14) VALUE "FEC DE INGRESO".
			03 FILLER PIC X(05) VALUE SPACES.
			03 FILLER PIC X(09) VALUE "ANUALIDAD".
			
	   01 DETALLES-CAMPOS-R.
			03 FILLER 		PIC X(08) VALUE SPACES.
			03 WKS-ID-CLI 	PIC 9(04).
			03 FILLER       PIC X(10) VALUE SPACES.
			03 WKS-NOMBRE   PIC A(20).
			03 FILLER       PIC X(08) VALUE SPACES.
			03 WKS-TIPO-TAR PIC A(10).
			03 FILLER       PIC X(09) VALUE SPACES.
			03 WKS-FECH-ING PIC 9(08).
			03 FILLER       PIC X(08) VALUE SPACES.
			03 WKS-ANUALIDAD PIC $Z,ZZ9.99.
	   
	   01 FOOTER-R1.
			03 FILLER PIC X(50) VALUE SPACES.
			03 FILLER PIC X(19) VALUE "TOTAL DE CLIENTES: ".
			03 WKS-CLIENTES-COUNT-R1 PIC ZZZZ.
			
	   01 FOOTER-ANUALIDAD-PAR-R2.
			03 FILLER PIC X(44) VALUE SPACES.
			03 FILLER PIC X(22) VALUE "TOTAL DE ANUALIDAD DE ".
			03 WKS-TIPO-TARF PIC A(10).
			03 FILLER PIC X(05) VALUE SPACES.
			03 WKS-ANUALIDA-PAR-R2 PIC $Z,ZZZ,ZZ9.99 .
		
	   01 FOOTER-ANUALIDAD-TOTAL-R2.
			03 FILLER PIC X(51) VALUE SPACES.
			03 FILLER PIC A(30) VALUE "TOTAL DE ANUALIDADES: ".
			03 WKS-ANUALIDA-TOTAL-R2 PIC $Z,ZZZ,ZZ9.99 .
	
       
       PROCEDURE DIVISION.
	   
	   INICIO.
		   PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-CLIENTES-LOOP = 3.
           PERFORM 3000-FIN.
           STOP RUN.
		   
	   1000-INICIO.
			OPEN INPUT DCLIENTE. 
	   
	   2000-PROCESO.
			DISPLAY SPACES  LINE 01 POSITION 01 ERASE.
			DISPLAY "REPORTE DE TARJETAS DE CLIENTES" LINE 01 
			                                          POSITION 7.
			DISPLAY "REPORTE POR CLIENTES (1) "       LINE 03 
			                                          POSITION 5.
			DISPLAY "REPORTE POR TARJETAS (2) "       LINE 04 
			                                          POSITION 5.
			DISPLAY "SALIR                (3) "       LINE 05 
			                                          POSITION 5.
			DISPLAY "ELEGIR OPCION : "                LINE 07 
			                                          POSITION 5.
			ACCEPT WKS-CLIENTES-LOOP LINE 07 POSITION 21.
			
			DISPLAY SPACES LINE 01 POSITION 01 ERASE.
			
			IF WKS-CLIENTES-LOOP = 1
			   MOVE 0 TO WKS-CLIENTES-LOOP
			   PERFORM 2100-GENERAR-REPORTE-CLIENTE
			   PERFORM 2001-MENSAJE-GENERADO
				ELSE
					IF WKS-CLIENTES-LOOP = 2
					    MOVE 0 TO WKS-CLIENTES-LOOP
					    PERFORM 2200-GENERAR-REPORTE-IDTARJETA
						PERFORM 2001-MENSAJE-GENERADO
						ELSE 
							IF WKS-CLIENTES-LOOP = 3
							    NEXT SENTENCE.
			
       2001-MENSAJE-GENERADO.
			DISPLAY "REPORTE GENERADO" LINE 01 POSITION 01.
			DISPLAY "PRESIONE CUALQUIER TECLA PARA REGRESAR" 
			                                  LINE 02 POSITION 01.
		    ACCEPT WKS-RES LINE 02 POSITION 40.
			
			
	   3000-FIN.
	        CLOSE DCLIENTE. 
			
	 
	  
	   2100-GENERAR-REPORTE-CLIENTE.
		   PERFORM 2110-GENERAR-REPORTE-INICIO.
           PERFORM 2120-DETALLE-R1 UNTIL WKS-CLIENTES-LOOP = 1.
           PERFORM 2130-FOOTER-R1.
		   
	   2110-GENERAR-REPORTE-INICIO.
	         OPEN OUTPUT REP-CLI.
			 
			 MOVE 0 TO ID-CLIENTE-D. 
             START DCLIENTE KEY IS > ID-CLIENTE-D.
			 
			 PERFORM 2111-LEER-ARCHIVO-R1.
			 
           IF WKS-CLIENTES-LOOP = 0
              PERFORM 2112-HEADER-R.
	   
	   2112-HEADER-R.
		   MOVE HEADER-L1-R TO LINEA.
		   
		   IF WKS-PAG = 1
				WRITE LINEA AFTER 1
		   ELSE 
				WRITE LINEA AFTER PAGE.
		   
		   ACCEPT WKS-FECHA-PRO FROM DATE.
		   MOVE HEADER-L2-R TO LINEA.
		   WRITE LINEA AFTER 1.
		   
		   MOVE HEADER-CAMPOS-R1 TO LINEA.
		   WRITE LINEA AFTER 2.
		   
           
       2111-LEER-ARCHIVO-R1.
           READ DCLIENTE NEXT AT END MOVE 1 TO WKS-CLIENTES-LOOP.
           
       2120-DETALLE-R1.
		   MOVE ID-CLIENTE-D       TO WKS-ID-CLI.
           MOVE NOMBRE-CLIENTE-D   TO WKS-NOMBRE.
           MOVE TIPO-TARG-D        TO WKS-TIPO-TAR.
           MOVE ANUALIDAD-D        TO WKS-ANUALIDAD.
           MOVE FECHA-ALTA-D       TO WKS-FECH-ING.
		   
           MOVE DETALLES-CAMPOS-R TO LINEA
		   WRITE LINEA AFTER 1.
		   
		   ADD 1 TO WKS-CLIENTES-COUNT.
		   ADD 1 TO WKS-DETALLES-COUNT.
		   
		   IF WKS-DETALLES-COUNT = 80
				MOVE 0 TO WKS-DETALLES-COUNT
				ADD  1 TO WKS-PAG
				PERFORM 2112-HEADER-R.
		    
		   
           PERFORM 2111-LEER-ARCHIVO-R1.
       
       2130-FOOTER-R1.
		   MOVE WKS-CLIENTES-COUNT TO WKS-CLIENTES-COUNT-R1.
		   MOVE FOOTER-R1 TO LINEA
		   WRITE LINEA AFTER 2.
		   
		   PERFORM 2131-REINICIALIZAR-VARIABLES.
		 
           CLOSE REP-CLI.
		   
	   2131-REINICIALIZAR-VARIABLES.
	       MOVE 0 TO WKS-CLIENTES-COUNT.
		   MOVE 0 TO WKS-DETALLES-COUNT.
		   MOVE 1 TO WKS-PAG.
		   MOVE 0 TO WKS-CLIENTES-LOOP.
	   
	   
	  
      *-PROCEDIMIENTO PARA GENERAR EL REPORTE POR TARJETA
	  
	   2200-GENERAR-REPORTE-IDTARJETA.
		   PERFORM 2210-GENERAR-REPORTE2-INICIO.
           PERFORM 2220-DETALLE-R2 UNTIL WKS-CLIENTES-LOOP = 1.
		   PERFORM 2230-FOOTER-ANUALIDAD-PAR-R2.
           PERFORM 2231-FOOTER-ANUALIDAD-TOTAL-R2.
		   
	   2210-GENERAR-REPORTE2-INICIO.
	       OPEN OUTPUT REP-TAR
			 
		   MOVE 0 TO ID-TARJETA-D. 
           START DCLIENTE KEY IS > ID-TARJETA-D.
			 
		   PERFORM 2211-LEER-ARCHIVO-R1.
		   
           IF WKS-CLIENTES-LOOP = 0
		      MOVE TIPO-TARG-D TO WKS-TIPO-TARF
              PERFORM 2212-HEADER-R.
	   
	   2212-HEADER-R.
		   MOVE HEADER-L1-R2 TO LINEA-TAR.

		   IF WKS-PAG = 1
				WRITE LINEA-TAR AFTER 1
		   ELSE 
				WRITE LINEA-TAR AFTER PAGE.
		   
		   ACCEPT WKS-FECHA-PRO FROM DATE.
		   MOVE HEADER-L2-R TO LINEA-TAR.
		   WRITE LINEA-TAR AFTER 1.
		   
		   MOVE ID-TARJETA-D TO WKS-ID-TARJETA.
		   MOVE HEADER-L3-R2 TO LINEA-TAR.
		   WRITE LINEA-TAR AFTER 2.
		   
		   MOVE HEADER-CAMPOS-R1 TO LINEA-TAR.
		   WRITE LINEA-TAR AFTER 2
		   
		   ADD  1 TO WKS-PAG
		   MOVE 0 TO WKS-DETALLES-COUNT.
		   
           
       2211-LEER-ARCHIVO-R1.
           READ DCLIENTE NEXT AT END MOVE 1 TO WKS-CLIENTES-LOOP.
           
       2220-DETALLE-R2.
		   MOVE ID-CLIENTE-D       TO WKS-ID-CLI.
           MOVE NOMBRE-CLIENTE-D   TO WKS-NOMBRE.
           MOVE TIPO-TARG-D        TO WKS-TIPO-TAR.
           MOVE ANUALIDAD-D        TO WKS-ANUALIDAD.
           MOVE FECHA-ALTA-D       TO WKS-FECH-ING.
		   
		   ADD 1 TO WKS-DETALLES-COUNT.
		   	   
		   
		   IF WKS-DETALLES-COUNT = 80
				PERFORM 2212-HEADER-R
				MOVE 1 TO WKS-CORTE.
		   
		   IF WKS-ID-TARJETA = ID-TARJETA-D
					NEXT SENTENCE
		   ELSE
				
				IF WKS-CORTE = 1
                    PERFORM 2230-FOOTER-ANUALIDAD-PAR-R2				
					PERFORM 2212-HEADER-R
				    MOVE 0 TO WKS-ANUALIDA-PAR
					MOVE 0 TO WKS-CORTE.
		    
			ADD ANUALIDAD-D TO WKS-ANUALIDA-PAR.
	        ADD ANUALIDAD-D TO WKS-ANUALIDAD-TOTAL.
		    
			MOVE DETALLES-CAMPOS-R TO LINEA-TAR
		    WRITE LINEA-TAR AFTER 1
		   

           PERFORM 2211-LEER-ARCHIVO-R1.
		   

	       
       
       2230-FOOTER-ANUALIDAD-PAR-R2.
		   MOVE WKS-ANUALIDA-PAR TO WKS-ANUALIDA-PAR-R2
		   MOVE FOOTER-ANUALIDAD-PAR-R2 TO LINEA-TAR
		   WRITE LINEA-TAR AFTER 2
		   MOVE TIPO-TARG-D TO WKS-TIPO-TARF.
	   
	   2231-FOOTER-ANUALIDAD-TOTAL-R2.
	       MOVE WKS-ANUALIDAD-TOTAL       TO WKS-ANUALIDA-TOTAL-R2.
		   MOVE FOOTER-ANUALIDAD-TOTAL-R2 TO LINEA-TAR.
		   WRITE LINEA-TAR AFTER 2.
		   
	       PERFORM 2232-REINICIALIZAR-VARIABLES.
           CLOSE REP-TAR.
		   
	   2232-REINICIALIZAR-VARIABLES.
		   MOVE 0 TO WKS-DETALLES-COUNT.
		   MOVE 1 TO WKS-PAG.
		   MOVE 0 TO WKS-CLIENTES-LOOP.
		   MOVE 0 TO WKS-ANUALIDAD-TOTAL.
		   MOVE 0 TO WKS-ANUALIDA-PAR.
  