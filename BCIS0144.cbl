      *----------------------------------------------------------------*
      * PROGRAMA..: BCIS0144
      * ANALISTA..: MATEUS BARBOSA DA SILVA.
      * AUTOR.....: MATEUS BARBOSA DA SILVA.
      * DATA......: 10/09/2025.
      * OBJETIVO..: Trata CPF e CNPJ (alfa) para o ambiente online
      *----------------------------------------------------------------*
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      *----------------------------------------------------------------*
      * Vrs Data     E/A/D   Respon.  Descrição da versão
      * --- -------- ------- -------- ---------------------------------*
      * 001 10092025 Implan. F7021226 Implantacao
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. BCIS0144.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      ******************************************************************
      ************** Tabela lista de programas chamados ****************
      ******************************************************************
       01 TAB-PROGRAMAS-CHAMADOS.
          03 MCIS0000                       PIC X(8) VALUE 'MCIS0000'.
          03 CICS5000                       PIC X(8) VALUE 'CICS5000'.
          03 DFES192D                       PIC X(8) VALUE 'DFES192D'.
          03 BCISLOG0                       PIC X(8) VALUE 'BCISLOG0'.
-INC ALMKCBL1
       77  CTE-INICIO                  PIC  X(025) VALUE
                                            '*** W.S.S. COMECA AQUI***'.
       77  CTE-PROG                    PIC  X(016) VALUE
                                            '*** BCIS0144 ***'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
       77  CTE-FIM                     PIC  X(018) VALUE
                                            '*** FIM NORMAL ***'.
       LOCAL-STORAGE SECTION.
      *
      * Variaveis locais que realizam o tratamento
       01  W-VRV-ENTD-SAID-PGM.
           03  W-CD-FUC            PIC 9(009) VALUE ZEROS.
           03  W-NR-IDFC-PF-PJ     PIC 9(014) VALUE ZEROS.
           03  W-CPF-CNPJ-FMTD     PIC X(018) VALUE SPACES.
           03  W-CPF-CNPJ-N-FMTD   PIC X(014) VALUE SPACES.
           03  W-CD-MCI-CLI        PIC 9(009) VALUE ZEROS.
           03  W-TIP-PSS           PIC 9(001) VALUE ZERO.
           03  W-CPF-N-FMTD        PIC X(011) VALUE SPACES.
           03  W-CPF-FMTD          PIC X(014) VALUE SPACES.
      * Variaveis usadas para extrair os caracteres do CPF/CNPJ
       01  W-VRV-EXTC-CNPJ.
           03  W-CCT-CPF-CNPJ      PIC X(001) VALUE SPACE.
           03  W-CPF-CNPJ-EXTC     PIC X(014) VALUE SPACES.
           03  W-IC-ITRA           PIC 9(001) VALUE ZEROS COMP.
           03  W-FLAG-LETRA        PIC 9(001) VALUE ZERO.
           03  W-FLAG-NR           PIC 9(001) VALUE ZERO.
           03  W-FLAG-EH-CNPJ      PIC 9(001) VALUE ZERO.
      * variavel com as letras e numeros
       01  W-CJT-CCT-VLDO.
           03  W-ALFABETO PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           03  W-NUMEROS  PIC X(10) VALUE '0123456789'.
      * Retorna "O" se ambiente online, e "B", se ambiente BATCH
       01  CICS5000-AREA.
           03  CICS5000-CD-RTN-PGM    PIC  9(02) VALUE 0.
           03  CICS5000-CD-AMB-EXEA   PIC  X(01) VALUE ' '.
               88  EXECUCAO-ONLINE    VALUE 'O'.
               88  EXECUCAO-BATCH     VALUE 'B'.
       01  W-PGM-CASD-ERRO            PIC  X(008) VALUE SPACES.
      *
      * BOOKs usados no programa
      *
      * Responsavel pela conversao do CNPJ (online)
       01  DFES192D-DADOS.
-INC DFEK192D
      * Busca dados do cliente no MCI pelo CPF/CNPJ
       01  MCIS0000-DADOS.
-INC MCIK0000
      * Grava logs de execucao no HLP
       01  BCISLOG0-DADOS.
-INC BCIKLOG0
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
-INC BCIK0144
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING DFHCOMMAREA.
-INC ALMKCBL2
      *----------------------------------------------------------------*
       000000-ROTINA-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE DFES192D-DADOS
                      MCIS0000-DADOS
                      BCISLOG0-DADOS
      *
           IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE 1          TO K0144-CD-RTN
               MOVE 'S0144 - Area com tamanho invalido (book).' TO
                 K0144-TX-MSG-RTN
               PERFORM 999000-GRAVA-LOG-HLP
               GOBACK
           END-IF
      *
           PERFORM 010000-VALIDA-ENTD
           PERFORM 020000-PROCESSA-FUC
           .
      *
       000000-TERMINA.
           GOBACK
           .
      *----------------------------------------------------------------*
       010000-VALIDA-ENTD  SECTION.
      *----------------------------------------------------------------*
      *
           EVALUATE K0144-CD-FUC-DFE
      * 1 - Busca atraves do identificador do numero PJ
               WHEN 1
                   IF K0144-NR-IDFC-PF-PJ EQUAL ZEROS
                       STRING 'S0144 - Numero identificador de pessoa'
                         ' vazio.' DELIMITED BY SIZE
                         INTO K0144-TX-MSG-RTN
                   END-IF
      * 2 - Busca atraves do CNPJ como texto
               WHEN 2
                   IF K0144-CPF-CNPJ-ALFA-N-FMTD EQUAL SPACES AND
                      K0144-CPF-CNPJ-ALFA-FMTD EQUAL SPACES
                       MOVE 'S0144 - Texto de CPF/CNPJ vazio.'
                         TO K0144-TX-MSG-RTN
                   END-IF
      * Nem 1 ou 2 - entrada invalida
               WHEN OTHER
                   MOVE 'S0144 - Codigo funcao invalido.'
                     TO K0144-TX-MSG-RTN
           END-EVALUATE
      *
           IF K0144-TX-MSG-RTN NOT EQUAL SPACES
               MOVE 2          TO K0144-CD-RTN
               PERFORM 999000-GRAVA-LOG-HLP
               GOBACK
           END-IF
      *
           CALL CICS5000 USING CICS5000-AREA
      *
           IF CICS5000-CD-RTN-PGM NOT EQUAL ZEROS
               MOVE 3          TO K0144-CD-RTN
               MOVE 'S0144 - Erro ao obter tipo de processamento.' TO
                 K0144-TX-MSG-RTN
               MOVE CICS5000 TO K0144-PGM-CASD-ERRO
               PERFORM 999000-GRAVA-LOG-HLP
               GOBACK
           END-IF
           .
      *
       010000-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       020000-PROCESSA-FUC SECTION.
      *----------------------------------------------------------------*
      *
           MOVE K0144-CD-FUC-DFE           TO W-CD-FUC
           MOVE K0144-NR-IDFC-PF-PJ        TO W-NR-IDFC-PF-PJ
           MOVE FUNCTION TRIM(K0144-CPF-CNPJ-ALFA-N-FMTD) TO
             W-CPF-CNPJ-N-FMTD
           MOVE FUNCTION TRIM(K0144-CPF-CNPJ-ALFA-FMTD)
             TO W-CPF-CNPJ-FMTD
      *
           EVALUATE K0144-CD-FUC-DFE
               WHEN 1
                   PERFORM 021000-BUSCA-CLI-MCI
                   IF W-TIP-PSS EQUAL 2
                       PERFORM 022000-BUSCA-CNPJ-DFE
                   ELSE
                       MOVE W-NR-IDFC-PF-PJ TO W-CPF-CNPJ-N-FMTD
                       PERFORM 023000-TRATA-CPF
                   END-IF
               WHEN 2
                   PERFORM 022100-EXTRAI-CCT-CNPJ
                   IF W-FLAG-EH-CNPJ EQUAL 1
                       PERFORM 022000-BUSCA-CNPJ-DFE
                   ELSE
                       MOVE W-CPF-CNPJ-N-FMTD TO W-NR-IDFC-PF-PJ
                       PERFORM 021000-BUSCA-CLI-MCI
                       IF W-TIP-PSS EQUAL 2
                           MOVE ZEROS TO W-NR-IDFC-PF-PJ
                           PERFORM 022000-BUSCA-CNPJ-DFE
                       ELSE
                           PERFORM 023000-TRATA-CPF
                       END-IF
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           MOVE W-NR-IDFC-PF-PJ   TO K0144-NR-IDFC-PF-PJ
           MOVE W-CPF-CNPJ-N-FMTD TO K0144-CPF-CNPJ-ALFA-N-FMTD
           MOVE W-CPF-CNPJ-FMTD   TO K0144-CPF-CNPJ-ALFA-FMTD
           .
      *
       020000-SAI.
           EXIT.
      *----------------------------------------------------------------*
       021000-BUSCA-CLI-MCI SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE MCIS0000-DADOS
      *
           MOVE 'BCI'           TO MCIS0000-SYS-CHAMADOR
           MOVE 'BCIS0144'      TO MCIS0000-PRG-CHAMADOR
                                   MCIS0000-USUARIO
           MOVE W-NR-IDFC-PF-PJ TO MCIS0000-CPF-CGC-E
      *
           MOVE LENGTH OF MCIS0000-DADOS TO EIBCALEN
      *
           CALL MCIS0000 USING DFHEIBLK MCIS0000-DADOS
      *
           IF MCIS0000-RET-CODE NOT EQUAL ZEROS
               MOVE MCIS0000-RET-CODE     TO K0144-CD-RTN
               MOVE MCIS0000-MSG-RET-CODE TO K0144-TX-MSG-RTN
               MOVE MCIS0000 TO W-PGM-CASD-ERRO
               PERFORM 999000-GRAVA-LOG-HLP
               GOBACK
           END-IF
      *
           MOVE MCIS0000-CD-CLI-S (1) TO W-CD-MCI-CLI
           MOVE MCIS0000-TIP-PSS-S(1) TO W-TIP-PSS K0144-TIP-PSS
           .
      *
       021000-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       022000-BUSCA-CNPJ-DFE  SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE DFES192D-DADOS
      *
           MOVE W-CD-FUC          TO DFES192D-CD-FUNCAO
           MOVE W-NR-IDFC-PF-PJ   TO DFES192D-NR-IDFC-PJ-E
           MOVE W-CPF-CNPJ-N-FMTD TO DFES192D-CD-CNPJ-SRF-E
      *
           MOVE LENGTH OF DFES192D-DADOS TO EIBCALEN
           CALL DFES192D USING DFHEIBLK DFES192D-DADOS
      *
           IF DFES192D-CD-RETORNO NOT EQUAL ZEROS
               MOVE DFES192D-CD-RETORNO     TO K0144-CD-RTN
               MOVE DFES192D-TX-RETORNO     TO K0144-TX-MSG-RTN
               MOVE DFES192D TO W-PGM-CASD-ERRO
               PERFORM 999000-GRAVA-LOG-HLP
               GOBACK
           END-IF
      *
           MOVE DFES192D-NR-IDFC-PJ-S  TO W-NR-IDFC-PF-PJ
           MOVE DFES192D-CD-CNPJ-SRF-S TO W-CPF-CNPJ-N-FMTD
      *
           PERFORM 022200-TRATA-CNPJ-ALFA
           .
      *
       010000-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       022200-TRATA-CNPJ-ALFA  SECTION.
      *----------------------------------------------------------------*
      *
      * Entrada: W-CPF-CNPJ-N-FMTD
      * Saida..: W-CPF-CNPJ-FMTD
      *
           PERFORM 022210-ADICIONA-ZEROS-CNPJ
      *
           STRING W-CPF-CNPJ-N-FMTD(1:2)
                  '.'
                  W-CPF-CNPJ-N-FMTD(3:3)
                  '.'
                  W-CPF-CNPJ-N-FMTD(6:3)
                  '/'
                  W-CPF-CNPJ-N-FMTD(9:4)
                  '-'
                  W-CPF-CNPJ-N-FMTD(13:2) DELIMITED BY SIZE
                  INTO W-CPF-CNPJ-FMTD
           .
      *
       022200-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       022210-ADICIONA-ZEROS-CNPJ  SECTION.
      *----------------------------------------------------------------*
      *
      * Entrada e saida: W-CPF-CNPJ-N-FMTD
      *
           MOVE FUNCTION REVERSE(W-CPF-CNPJ-N-FMTD) TO W-CPF-CNPJ-N-FMTD
           MOVE FUNCTION TRIM(W-CPF-CNPJ-N-FMTD) TO W-CPF-CNPJ-N-FMTD
           INSPECT W-CPF-CNPJ-N-FMTD CONVERTING SPACES TO ZEROS
           MOVE FUNCTION REVERSE(W-CPF-CNPJ-N-FMTD) TO W-CPF-CNPJ-N-FMTD
           .
      *
       022210-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       023000-TRATA-CPF SECTION.
      *----------------------------------------------------------------*
      *
      * Entrada: W-CPF-N-FMTD
      * Saida..: W-CPF-CNPJ-N-FMTD, W-CPF-CNPJ-FMTD
      *
           MOVE FUNCTION REVERSE(W-CPF-CNPJ-N-FMTD) TO W-CPF-N-FMTD
           MOVE FUNCTION REVERSE(W-CPF-N-FMTD)      TO W-CPF-N-FMTD
      *
           PERFORM 023100-ADICIONA-ZEROS-CPF
      *
           STRING W-CPF-N-FMTD(1:3)
                  '.'
                  W-CPF-N-FMTD(4:3)
                  '.'
                  W-CPF-N-FMTD(7:3)
                  '-'
                  W-CPF-N-FMTD(10:2) DELIMITED BY SIZE
                  INTO W-CPF-FMTD
      *
           MOVE W-CPF-N-FMTD TO W-CPF-CNPJ-N-FMTD
           MOVE W-CPF-FMTD   TO W-CPF-CNPJ-FMTD
           .
      *
       023000-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       023100-ADICIONA-ZEROS-CPF  SECTION.
      *----------------------------------------------------------------*
      *
      * Entrada e saida: W-CPF-N-FMTD
      *
           MOVE FUNCTION REVERSE(W-CPF-N-FMTD) TO W-CPF-N-FMTD
           MOVE FUNCTION TRIM(W-CPF-N-FMTD) TO W-CPF-N-FMTD
           INSPECT W-CPF-N-FMTD CONVERTING SPACES TO ZEROS
           MOVE FUNCTION REVERSE(W-CPF-N-FMTD) TO W-CPF-N-FMTD
           .
      *
       023100-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       022100-EXTRAI-CCT-CNPJ  SECTION.
      *----------------------------------------------------------------*
      *
           IF W-CPF-CNPJ-N-FMTD NOT EQUAL SPACES
               PERFORM 022110-VERIFICA-LETRAS
           ELSE
      * variavel que vai receber as letras e numeros
               MOVE SPACES TO W-CPF-CNPJ-EXTC
      * iteracao sobre cada caracter
               PERFORM VARYING W-IC-ITRA FROM 1 BY 1
                   UNTIL W-IC-ITRA > LENGTH OF W-CPF-CNPJ-FMTD
      * flag que serve para indicar se eh uma letra ou numero
                   MOVE ZERO TO W-FLAG-LETRA
                   MOVE ZERO TO W-FLAG-NR
      * extrai o caracter
                   MOVE W-CPF-CNPJ-FMTD(W-IC-ITRA:1) TO W-CCT-CPF-CNPJ
      * procura o caracter no conjunto de letras e numeros
                   INSPECT W-ALFABETO TALLYING W-FLAG-LETRA
                       FOR ALL W-CCT-CPF-CNPJ
                   INSPECT W-NUMEROS TALLYING W-FLAG-NR
                       FOR ALL W-CCT-CPF-CNPJ
      * se o caracter do CNPJ for numero ou letra
                   IF W-FLAG-LETRA EQUAL 1 OR W-FLAG-NR EQUAL 1
                       STRING W-CPF-CNPJ-EXTC DELIMITED BY SPACE
                              W-CCT-CPF-CNPJ  DELIMITED BY SIZE
                           INTO W-CPF-CNPJ-EXTC
                   END-IF
      * se encontrar alguma letra, eh CNPJ
                   IF W-FLAG-LETRA EQUAL 1
                       MOVE 1 TO W-FLAG-EH-CNPJ
                   END-IF
               END-PERFORM
      * move o cnpj extraido para uma variavel que desloca o conteudo
      * para a direita
               MOVE W-CPF-CNPJ-EXTC TO W-CPF-CNPJ-N-FMTD
           END-IF
      * coloca zeros a esquerda se houver espacos, nao precisa
      * necessariamente ser um CNPJ, mas facilita para transferir o
      * conteudo para uma variavel numerica
           PERFORM 022210-ADICIONA-ZEROS-CNPJ
           .
      *
       022100-SAI.
           EXIT
           .
      *----------------------------------------------------------------*
       022110-VERIFICA-LETRAS SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM VARYING W-IC-ITRA FROM 1 BY 1
               UNTIL W-IC-ITRA GREATER THAN LENGTH OF W-CPF-CNPJ-N-FMTD
               MOVE ZERO TO W-FLAG-LETRA
      * extrai o caracter
               MOVE W-CPF-CNPJ-N-FMTD(W-IC-ITRA:1) TO W-CCT-CPF-CNPJ
      * procura o caracter no alfabeto
               INSPECT W-ALFABETO TALLYING W-FLAG-LETRA
                   FOR ALL W-CCT-CPF-CNPJ
      * se for uma letra, sai do loop
                IF W-FLAG-LETRA EQUAL 1
                    MOVE 1 TO W-FLAG-EH-CNPJ
                    EXIT PERFORM
                END-IF
           END-PERFORM
           .
      *
       022110-SAI.
           EXIT
           .
      *---------------------------------------*
       999000-GRAVA-LOG-HLP            SECTION.
      *---------------------------------------*
      *
      * Armazena valores das variaveis do programa no momento do erro
      *
           INITIALIZE BCISLOG0-DADOS
      *
           MOVE 'BCI'                   TO KLOG0-CD-SIS-OGM
      *
           MOVE 'BCIS0144'              TO KLOG0-CD-PGM-CHMR
      *
           IF W-PGM-CASD-ERRO EQUAL SPACES
               MOVE 'BCIS0144'          TO W-PGM-CASD-ERRO
           ELSE
               MOVE W-PGM-CASD-ERRO     TO KLOG0-CD-PGM-CASD
           END-IF
      *
           MOVE W-PGM-CASD-ERRO         TO K0144-PGM-CASD-ERRO
                                           KLOG0-CD-PGM-CASD
      *
           MOVE K0144-TX-MSG-RTN        TO KLOG0-TX-MSG-ERR
      *
           MOVE 'K0144-CD-FUC-DFE'           TO KLOG0-NM-VRV    (01)
           MOVE K0144-CD-FUC-DFE             TO KLOG0-CTU-NRC-01(01)
           MOVE 'K0144-NR-IDFC-PF-PJ'        TO KLOG0-NM-VRV    (02)
           MOVE K0144-NR-IDFC-PF-PJ          TO KLOG0-CTU-NRC-01(02)
           MOVE 'K0144-CPF-CNPJ-ALFA-N-FMTD' TO KLOG0-NM-VRV    (03)
           MOVE K0144-CPF-CNPJ-ALFA-N-FMTD   TO KLOG0-CTU-VRV   (03)
           MOVE 'K0144-CPF-CNPJ-ALFA-FMTD'   TO KLOG0-NM-VRV    (04)
           MOVE K0144-CPF-CNPJ-ALFA-FMTD     TO KLOG0-CTU-VRV   (04)
      *
           MOVE LENGTH OF BCISLOG0-DADOS TO EIBCALEN
      *
           CALL BCISLOG0 USING DFHEIBLK BCISLOG0-DADOS
           .
      *
       999000-SAI.
           EXIT.
