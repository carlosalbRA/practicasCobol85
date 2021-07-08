      *MI TERCER PROGRAMA
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VERARIT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WKS-SLDO-MEN PIC 9(05).
       01 WKS-DIAS-TRA PIC 9(02).
       01 WKS-SLDO-D   PIC 9(06)V99.
       01 WKS-SLDO-S   PIC $ZZZ,ZZZ.99.
       01 WKS-SLDO-N   PIC $ZZZ,ZZZ.99.
       01 WKS-D        PIC 9(02) VALUE 30.
       01 WKS-S        PIC 9(01) VALUE 7.
       01 WKS-FORMAT   PIC $ZZZ,ZZZ.99.
        
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           
           DISPLAY "INTRODUCIR TU SUELDO MENSUAL:" LINE 03 POSITION 01.
           ACCEPT WKS-SLDO-MEN LINE 03 POSITION 32.
           
           DISPLAY "INTRODUCIR DIAS TRABAJADOS:" LINE 04 POSITION 01.
           ACCEPT WKS-DIAS-TRA LINE 04 POSITION 32.
           
           COMPUTE WKS-SLDO-D = WKS-SLDO-MEN / WKS-D.
           MOVE WKS-SLDO-D TO WKS-FORMAT.
           
           DISPLAY "TU SUELDO DIARIO ES:" LINE 05 POSITION 01. 
           DISPLAY WKS-FORMAT LINE 05 POSITION 25.
           
           
           COMPUTE WKS-SLDO-S = WKS-SLDO-D * WKS-S.
           
           DISPLAY "TU SUELDO SEMANAL ES:" LINE 06 POSITION 01. 
           DISPLAY WKS-SLDO-S LINE 06 POSITION 26.
                      
           COMPUTE WKS-SLDO-N = WKS-DIAS-TRA * WKS-SLDO-D.
           
           DISPLAY "TU SUELDO NETO ES:" LINE 07 POSITION 01. 
           DISPLAY WKS-SLDO-N LINE 07 POSITION 26.
           
           
           STOP RUN.
           

           
           
           
        
           
           
           
           
           