       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTE-SORT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VRV-TRT-ESP.
           03  W-IC-ITRA     PIC 9(002) VALUE ZEROS COMP.
           03  W-FLAG-ESP    PIC 9(001) VALUE ZEROS.
           03  W-CCT-STRING  PIC X(001) VALUE SPACE.
           03  W-STRING-ENTD PIC X(105) VALUE SPACES.
           03  W-STRING-TRT  PIC X(105) VALUE SPACES.
           03  W-2-CCT       PIC X(002) VALUE SPACES.
       PROCEDURE DIVISION.
      * redefine a string tratada para espacos
           MOVE SPACES TO W-STRING-TRT
      * considera que o primeiro caracter nao eh espaco
           MOVE ZERO TO W-FLAG-ESP
           PERFORM VARYING W-IC-ITRA FROM 1 BY 1
             UNTIL W-IC-ITRA GREATER THAN LENGTH OF W-STRING-ENTD
      * variavel que sera usada para agrupa 2 caracteres:
      * um espaco (# que representa o espaco) e um nao espaco
               MOVE SPACES TO W-2-CCT
      * extrai o caracter
               MOVE W-STRING-ENTD(W-IC-ITRA:1) TO W-CCT-STRING
      * se for espaco, diz que eh espaco (para a proxima iteracao)
               IF W-CCT-STRING EQUAL SPACE
                   MOVE 1 TO W-FLAG-ESP
               END-IF
      * se nao for espaco e o caracter anterior espaco e nao for o
      * primeiro caracter da iteracao -> concatena
               IF W-CCT-STRING NOT EQUAL SPACE AND W-FLAG-ESP EQUAL 1
                 AND W-IC-ITRA NOT EQUAL 1
      * junta '#' (representa o espaco) com o caracter (nao eh espaco)
                   STRING '#' DELIMITED BY SIZE
                     W-CCT-STRING DELIMITED BY SIZE
                     INTO W-2-CCT
      * junta a dupla de caracteres com a string final'
                   STRING W-STRING-TRT DELIMITED BY SPACE
                     W-2-CCT DELIMITED BY SIZE
                     INTO W-STRING-TRT
                     MOVE 0 TO W-FLAG-ESP
      * continua para a proxima iteracao
                   CONTINUE
               END-IF
      * se nao for espaco, a concatenacao ser normal com o caracter
               IF W-CCT-STRING NOT EQUAL SPACE
                   STRING W-STRING-TRT DELIMITED BY SPACE
                     W-CCT-STRING DELIMITED BY SIZE
                     INTO W-STRING-TRT
                   MOVE 0 TO W-FLAG-ESP
               END-IF
      * no fim, o espaco eh ignorado e nao concatenado
           END-PERFORM
      * troca o '#' pelo espaco
           INSPECT W-STRING-TRT REPLACING ALL '#' BY SPACE
           DISPLAY 'FIM: ' W-STRING-TRT
           .
