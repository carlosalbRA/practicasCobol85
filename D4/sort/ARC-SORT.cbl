      *MI PROGRAMA DE ARCHIVOS ORDENAR ARCHIVOS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARC-SORT.
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
		   SELECT WORKCLI ASSIGN TO DISK.
		   SELECT ORDENADO ASSIGN TO DISK.
        
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLI. 
			03 FILLER PIC X(50).
		
	   FD ORDENADO.
        01 REG-CLI-S 
			03 FILLER PIC X(50).
			
	   SD WORKCLI.
        01 REG-CLI-W.
			03 ID-CLIENTE-W       PIC 9(04).
			03 NOMBRE-CLIENTE-W   PIC A(20).
			03 FILLER PIC X(26).
        
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
       INICIO.
           SORT WORKCLI ON ASCENDING KEY NOMBRE-CLIENTE-W
		   USING CLIENTES 
		   GIVING ORDENADO.
           STOP RUN.
	       
 
           
           
           