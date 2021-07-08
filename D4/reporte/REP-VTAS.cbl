      ***************************************************************
      * TEMA    : REPORTE                                           *
      * OBJETIVO: ELABORACIÓN DE UN REPORTE                         *
      * PROGRAMA: GENERA UN ARCHIVO DE VENDEDORES Y ELABORA A PARTIR*
      *           DE ESTE ARCHIVO UN REPORTE                        *
      ***************************************************************
       IDENTIFICATION         DIVISION.
      *--------------------------------
       PROGRAM-ID.            REP-VTAS.
       AUTHOR.                INFOWARE, SC.
       DATE-WRITTEN.          JUNIO 2011.
      *
       ENVIRONMENT             DIVISION.
      *-------------------------------
      *
       CONFIGURATION           SECTION.
      *-------------------------------
      *
       INPUT-OUTPUT            SECTION.
      *--------------------------------
      *
       FILE-CONTROL.
      *-------------
      *
           SELECT VENDEDOR          ASSIGN TO DISK.

           SELECT  REPORTE          ASSIGN TO PRINTER.

      *
       DATA                   DIVISION.
      *-------------------------------
      *
       FILE                   SECTION.
      *-------------------------------
      *
       FD VENDEDOR.
      *================
       01 REG-VEN.
         03 CLAVE-VEN              PIC 9(03).
         03 NOMBRE-VEN             PIC X(30).
         03 VTAS-TOTALES           PIC 9(06)V99.

       FD  REPORTE.
      *=================
       01  LINEA                   PIC X(132).

      *
       WORKING-STORAGE        SECTION.
      *-------------------------------
      * --------------   VARIABLES DE TRABAJO  -------------------
       01  WKS-FIN                       PIC 9(01) VALUE ZEROES.
       01  WKS-FIN-ARC                   PIC 9(01) VALUE ZEROES.
       01  WKS-FIN-REP                   PIC 9(01) VALUE ZEROES.
       01  WKS-CONT-LIN                  PIC 9(02).
       01  WKS-CLAVE-VEN                 PIC 9(03).
       01  WKS-NOMBRE-VEN                PIC X(30).
       01  WKS-VTAS-TOTALES              PIC 9(05)V99.
       01  WKS-TOTAL                     PIC 9(07)V99.
       01  WKS-RES                       PIC X.
       01  WKS-WAIT                      PIC X.

      * --------------   VARIABLES DE REPORTE  --------------------
       01  WKS-ENC-1.
         03 FILLER                PIC X(24) VALUE SPACES.
         03 FILLER                PIC X(27)
                            VALUE "REPORTE DE VENTAS MENSUALES".
       01 WKS-ENC-2.
         03 FILLER                PIC X(60) VALUE SPACES.
         03 FILLER                PIC X(07) VALUE "PAG. : ".
         03 WKS-NUM-PAG           PIC 9(02).

       01  WKS-ENC-3.
         03 FILLER                PIC X(05)   VALUE SPACES.
         03 FILLER                PIC X(05)   VALUE "CLAVE".
         03 FILLER                PIC X(06)   VALUE SPACES.
         03 FILLER                PIC X(06)   VALUE "NOMBRE".
         03 FILLER                PIC X(28)   VALUE SPACES.
         03 FILLER                PIC X(12)   VALUE "VTAS TOTALES".

       01  WKS-DET-1.
        03 FILLER                PIC X(06)   VALUE SPACES.
        03 WKS-CLAVE             PIC 9(03).
        03 FILLER                PIC X(07)   VALUE SPACES.
        03 WKS-NOMBRE            PIC X(22).
        03 FILLER                PIC X(12)   VALUE SPACES.
        03 WKS-VTAS              PIC $ZZ,ZZZ.99.

       01  WKS-TOT-1.
        03 FILLER               PIC X(22)   VALUE SPACES.
        03 FILLER               PIC X(27)   VALUE
                     "TOTAL DE VENTAS MENSUALES: ".
        03 WKS-TOT-EDIT         PIC $ZZZ,ZZZ.99.
	   
	   01  WKS-LIN-FIN.
		03 FILLER PIC X(05) VALUE SPACES.
		03 FILLER PIC X(52) VALUE ALL "-".
		03 FILLER PIC X(05) VALUE SPACES.

      *
       PROCEDURE              DIVISION.
      *---------------------------------
      *
       100000-PROCESO-PRINCIPAL.
      *
           PERFORM 100000-PROCESO UNTIL WKS-FIN = 1
           STOP RUN.

       100000-PROCESO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE
           DISPLAY "PROG QUE GENERA ARCHIVO Y REP DE VTAS DE VENDEDORES"
                                                   LINE 02 POSITION 10
           DISPLAY "GENERAR ARCHIVO DE VENDEDOR (G)"
                                                   LINE 04 POSITION 05
           DISPLAY "GENERAR REPORTE DE VENTAS   (R)"
                                                   LINE 06 POSITION 05
           DISPLAY "PRESIONE CUALQUIER OTRA TECLA PARA SALIR..."
                                                   LINE 08 POSITION 05
           ACCEPT WKS-RES                          LINE 10 POSITION 05
           IF WKS-RES = "G"
              PERFORM 10000-GENERA-ARCHIVO
           ELSE
              IF WKS-RES = "R"
                 PERFORM 20000-GENERA-REPORTE
              ELSE
                 MOVE 1 TO WKS-FIN.

      ***************************************************
      **** COMIENZA PROCESO DE GENERACIÓN DE ARCHIVO ****
      **************************************************
      *
       10000-GENERA-ARCHIVO.
      *=======================
           PERFORM 11000-INICIO-GEN-ARC.
           PERFORM 12000-PROCESO-GEN-ARC UNTIL WKS-FIN-ARC = 1.
           PERFORM 13000-FIN-GEN-ARC.

      *
       11000-INICIO-GEN-ARC.
      *======================
           OPEN EXTEND VENDEDOR.

      *
       12000-PROCESO-GEN-ARC.
      *======================
           DISPLAY SPACE LINE 01  POSITION 01 ERASE.
           DISPLAY "PROCESO DE GENERACION DE ARCHIVO"
                                          LINE 03 POSITION 20.
           PERFORM 12500-CAPTURA-DATOS.
           PERFORM 12700-MUEVE-DATOS.
           DISPLAY "DESEA CAPTURAR MAS REGISTROS (S/N) "
           ACCEPT WKS-RES.
           IF WKS-RES = "N"
              MOVE 1 TO WKS-FIN-ARC.

      *
       12500-CAPTURA-DATOS.
      *=====================
           DISPLAY "INTRODUZCA LA CLAVE DEL VENDEDOR"
           ACCEPT WKS-CLAVE-VEN.
           DISPLAY "INTRODUZCA EL NOMBRE DEL VENDEDOR"
           ACCEPT WKS-NOMBRE-VEN.
           DISPLAY "INTRODUZCA LAS VENTAS MENSUALES:"
           ACCEPT WKS-VTAS-TOTALES.

      *
       12700-MUEVE-DATOS.
      *===================
           MOVE WKS-CLAVE-VEN    TO CLAVE-VEN.
           MOVE WKS-NOMBRE-VEN   TO NOMBRE-VEN.
           MOVE WKS-VTAS-TOTALES TO VTAS-TOTALES.
           WRITE REG-VEN.

      *
       13000-FIN-GEN-ARC.
      *==================
           CLOSE VENDEDOR.
           DISPLAY "ARCHIVO GENERADO..."  LINE 12 POSITION 05.

      ***************************************************
      **** COMIENZA PROCESO DE GENERACIÓN DE REPORTE ****
      ***************************************************
      *
       20000-GENERA-REPORTE.
      *=====================
           DISPLAY SPACE LINE 01  POSITION 01 ERASE.
           DISPLAY "PROCESO DE GENERACION DE REPORTE"
                                          LINE 03 POSITION 20.
           PERFORM 200100-INICIO-GEN-REP.
           PERFORM 200200-PROCESO-GEN-REP UNTIL WKS-FIN-REP = 1.
           PERFORM 200300-TERMINA-GEN-REP.

      *
       200100-INICIO-GEN-REP.
      *-----------------------
           OPEN INPUT VENDEDOR
           OUTPUT REPORTE.
           PERFORM 200150-LEE-ARCHIVO.
           IF WKS-FIN-REP = 0
              PERFORM 200170-GENERA-ENCABEZADO.
      *
       200150-LEE-ARCHIVO.
      *--------------------
           READ VENDEDOR
              AT END MOVE 1 TO WKS-FIN-REP.
      *
       200170-GENERA-ENCABEZADO.
      *-------------------------
      *
           ADD  1                TO WKS-NUM-PAG.
           MOVE SPACES           TO  LINEA.
           MOVE WKS-ENC-1        TO  LINEA.
           WRITE  LINEA AFTER PAGE.
           MOVE WKS-ENC-2        TO  LINEA.
           WRITE  LINEA AFTER 1.
           MOVE WKS-ENC-3        TO  LINEA.
           WRITE  LINEA AFTER 3.
           MOVE 7  TO WKS-CONT-LIN.
      *
       200200-PROCESO-GEN-REP.
      *----------------------
      **  DETALLE ARCHIVO **
           MOVE CLAVE-VEN          TO   WKS-CLAVE.
           MOVE NOMBRE-VEN         TO   WKS-NOMBRE.
           MOVE VTAS-TOTALES       TO   WKS-VTAS.
           ADD  VTAS-TOTALES       TO   WKS-TOTAL.
           IF WKS-CONT-LIN > 60
             PERFORM 200170-GENERA-ENCABEZADO.
           MOVE WKS-DET-1          TO   LINEA
           WRITE LINEA AFTER 1
           ADD 1 TO WKS-CONT-LIN.
           PERFORM  200150-LEE-ARCHIVO.
      *
       200300-TERMINA-GEN-REP.
      *--------------------
           MOVE WKS-TOTAL        TO  WKS-TOT-EDIT.
		   
		   MOVE WKS-LIN-FIN      TO LINEA.
           WRITE  LINEA AFTER 1.
	
		   
		   MOVE WKS-TOT-1        TO  LINEA.
		   WRITE  LINEA AFTER 1.
		   
           CLOSE VENDEDOR
                 REPORTE.
           DISPLAY "REPORTE GENERADO..."  LINE 12 POSITION 05.
           DISPLAY "ACEPTAR"              LINE 13 POSITION 05
           ACCEPT WKS-WAIT                LINE 13 POSITION 15.



















































