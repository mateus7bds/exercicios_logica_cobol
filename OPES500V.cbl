      *-----------------------------------------------------------------
      * PROGRAMA..: OPES500V.
      * ANALISTA..: Adriano
      * AUTOR.....: Adriano
      * DATA......: 08.11.06
      * COMPILACAO: 54 - PSOSE600 - Cobol 6.3 c/otimizacao p/producao
      * OBJETIVO..: VALIDAR REGISTRO DE ORDEM DE PAGAMENTO P/ O EXTERIOR
      * ---------------------------------------------------------------*
      * Vrs Data     Entrega Respon. Descrição da versão
      * --- -------- ------- ------- ----------------------------------*
      * 113 29122025         F7021226 Implementacao CNPJ alfa
      * 112 25092025 2023098 C1026078 Mover o valor da operação em curso
      *                               para o programa BCIS4010 para
      *                               atender Matriz de risco.
      *                               Alteração da IN de 117-1.1.17 para
      *                               IN 95.
      * 111 28052025 2011424 C1284847 Centraliza parametros de banqueiro
      *                               no sistema BCI.92.54
      * 110 27032025 1881868 F0501470 Evita erro de banqueiro
      * 109 11022025 1863094 C1284847 Implementa Regra de Envio para
      *                              instituicao destino
      * 108 25022025 1882724 Juliano Alterar banqueiro de cobertura EUR
      *                              conforme regra da historia 1879000.
      * 107 06012025 1806474 Juliano Quando DOLAR no autoatendimento
      *                              utilizar (CHASUS33). Se APJ utiliza
      *                              (BOFAUS3N).
      * 106 13092024 1649314 Juliano Para moeda EURO e interface 20
      *                              utilizar JP Morgan CHASDEFX.
      * 105 11072024 1552507 Juliano Direcionar para banqueiro ZBK quan-
      *                              do moeda for franco suíco (425) e
      *                              interface de autoatendimento.
      * 104 09042023 1402139 Juliano Para moeda EURO e interface 20
      *                              utilizar Standard (SCBLDEFX).
      * 103 07122023 1231187 Marco A Para moeda EURO e interface <> 3
      *                              troca SCBLDEFX por CHASDEFX.
      * 102 28062023 1019099 Herbert Ajustes para novo marco cambial
      * 101 19062023 997587 F3605968 Para moeda EURO e interface <> 3
      *                              utiliza SCBLDEFX como destinatario.
      * 100 02022023 796482 F6002608 Grava código da Interface no LOG
      *                              complementar da matriz.
      * 099 20012023 Manut. F6002608 Grava log complementar utilizando
      *                              processo assíncrono BCISLOG1.
      * 098 03012023 742981 F6002608 Grava log complementar da análise
      *                              da matriz de risco.
      * 097 16122022 759686 F3605968 Nova lei cambial
      * 096 21092022 652420 F6798146 Para moeda dolar e interface 15
      *                              utiliza CHASUS33 como destinatario.
      * 095 18072022 573869 F6002608 Incluir variável CD-IDFC-EXEA-ANL
      *                              Obs.: Só repassa inf. com Matriz
      *                              de Risco Ativa.
      * ---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    OPES500V.
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION   SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       77  CTE-INICIO             PIC  X(24) VALUE 'WSS COMECA AQUI'.
       77  CTE-PROG               PIC  X(16) VALUE '*** OPES500V ***'.
       77  CTE-VERS               PIC  X(06) VALUE 'VRS112'.
      *
      ******************************************************************
      ************** Tabela lista de programas chamados ****************
      ******************************************************************
       01 TAB-PROGRAMAS-CHAMADOS.
          03 BCIS002R                       PIC X(8) VALUE 'BCIS002R'.
          03 BCIS003R                       PIC X(8) VALUE 'BCIS003R'.
          03 BCIS003N                       PIC X(8) VALUE 'BCIS003N'.
          03 BCIS006R                       PIC X(8) VALUE 'BCIS006R'.
          03 BCIS017R                       PIC X(8) VALUE 'BCIS017R'.
          03 BCISO003                       PIC X(8) VALUE 'BCISO003'.
          03 BCISO030                       PIC X(8) VALUE 'BCISO030'.
          03 BCIS051N                       PIC X(8) VALUE 'BCIS051N'.
          03 BCIS094L                       PIC X(8) VALUE 'BCIS094L'.
          03 BCIS108L                       PIC X(8) VALUE 'BCIS108L'.
          03 BCIS1113                       PIC X(8) VALUE 'BCIS1113'.
          03 BCIS4000                       PIC X(8) VALUE 'BCIS4000'.
          03 BCIS4010                       PIC X(8) VALUE 'BCIS4010'.
          03 BCIS4015                       PIC X(8) VALUE 'BCIS4015'.
          03 BCISLOG0                       PIC X(8) VALUE 'BCISLOG0'.
          03 BCISLOG1                       PIC X(8) VALUE 'BCISLOG1'.
          03 BDDE142F                       PIC X(8) VALUE 'BDDE142F'.
          03 BDDE143H                       PIC X(8) VALUE 'BDDE143H'.
          03 BDDSH143                       PIC X(8) VALUE 'BDDSH143'.
          03 BDDE142H                       PIC X(8) VALUE 'BDDE142H'.
          03 BDDE142J                       PIC X(8) VALUE 'BDDE142J'.
          03 BDDE142K                       PIC X(8) VALUE 'BDDE142K'.
          03 BECSO103                       PIC X(8) VALUE 'BECSO103'.
          03 BECSO320                       PIC X(8) VALUE 'BECSO320'.
          03 CCAS006R                       PIC X(8) VALUE 'CCAS006R'.
          03 CCAS0080                       PIC X(8) VALUE 'CCAS0080'.
          03 CCAS052L                       PIC X(8) VALUE 'CCAS052L'.
          03 CPRSB001                       PIC X(8) VALUE 'CPRSB001'.
          03 DEBSB972                       PIC X(8) VALUE 'DEBSB972'.
          03 DMES502V                       PIC X(8) VALUE 'DMES502V'.
          03 EIFSB014                       PIC X(8) VALUE 'EIFSB014'.
          03 EIFSB018                       PIC X(8) VALUE 'EIFSB018'.
          03 MCIS1006                       PIC X(8) VALUE 'MCIS1006'.
          03 MCIS1449                       PIC X(8) VALUE 'MCIS1449'.
          03 OPRS5900                       PIC X(8) VALUE 'OPRS5900'.
          03 OPEP802S                       PIC X(8) VALUE 'OPEP802S'.
          03 OPES012R                       PIC X(8) VALUE 'OPES012R'.
          03 OPES035R                       PIC X(8) VALUE 'OPES035R'.
          03 OPES036L                       PIC X(8) VALUE 'OPES036L'.
          03 OPES042G                       PIC X(8) VALUE 'OPES042G'.
          03 OPES051U                       PIC X(8) VALUE 'OPES051U'.
          03 OPES052U                       PIC X(8) VALUE 'OPES052U'.
          03 OPES0820                       PIC X(8) VALUE 'OPES0820'.
          03 OPES0821                       PIC X(8) VALUE 'OPES0821'.
          03 OPES800V                       PIC X(8) VALUE 'OPES800V'.
          03 OPES824S                       PIC X(8) VALUE 'OPES824S'.
          03 ORES0855                       PIC X(8) VALUE 'ORES0855'.
          03 ORES0871                       PIC X(8) VALUE 'ORES0871'.
          03 SEGSB513                       PIC X(8) VALUE 'SEGSB513'.
          03 BCIS921T                       PIC X(8) VALUE 'BCIS921T'.
          03 BCIS218R                       PIC X(8) VALUE 'BCIS218R'.
          03 BDDS833R                       PIC X(8) VALUE 'BDDS833R'.
          03 OPES043R                       PIC X(8) VALUE 'OPES043R'.
          03 BDDSBLOQ                       PIC X(8) VALUE 'BDDSBLOQ'.
          03 SBVERSAO                       PIC X(8) VALUE 'SBVERSAO'.
          03 SBCURDAT                       PIC X(8) VALUE 'SBCURDAT'.
          03 SBDIGITO                       PIC X(8) VALUE 'SBDIGITO'.
          03 SBDATA                         PIC X(8) VALUE 'SBDATA'.
          03 SBCPU                          PIC X(8) VALUE 'SBCPU'.
          03 SBTIMEDT                       PIC X(8) VALUE 'SBTIMEDT'.
          03 BCIS204U                       PIC X(8) VALUE 'BCIS204U'.
          03 BDDS145C                       PIC X(8) VALUE 'BDDS145C'.
          03 DFES192C                       PIC X(8) VALUE 'DFES192C'.
          03 BCISDFE0                       PIC X(8) VALUE 'BCISDFE0'.
      *
-INC ALMKCBL1
      *
      * Book do programa de ambiente - CICS5000
      * Definido na work para executar uma unica vez na thread
-INC HLPKE01B
      *
      *
      *************************
       LOCAL-STORAGE   SECTION.
      *************************
      *
      ************************************************************
      * VARIAVEIS DE TRABALHO
      ************************************************************

       77  W-TX-CTL               PIC  X(25) VALUE SPACES.
       77  W-VL-CTL               PIC  X(50) VALUE SPACES.
       77  W-NM-TMDR              PIC  X(50) VALUE SPACES.
       77  W-TX-DEPE              PIC  X(25) VALUE SPACES.
       77  W-TX-TIP-DATA          PIC  X(15) VALUE SPACES.
       77  W-CD-PRF-DEPE          PIC  9(04) VALUE ZEROS.
       77  W-CD-CT-VLDO           PIC  9(01) VALUE 0.
           88  W-IN-CT-VLDO                  VALUE 1.
       77  W-CD-SQL               PIC +++9.
       77  W-CD-CPF-CGC           PIC  9(14) VALUE ZEROS.
       77  W-CD-MOE               PIC  9(03) VALUE ZEROS.
       77  W-CD-IFC-CAIXA         PIC  9(12) VALUE ZEROS.
       77  W-VL-VRC               PIC  9(10)V9(02) VALUE ZEROS.
       77  W-VL-MOEN-MIN          PIC  9(15)V9(02) VALUE ZEROS.
       77  W-VL-MOEN-MAX          PIC  9(15)V9(02) VALUE ZEROS.
       77  W-NM-TAB               PIC  X(20) VALUE SPACES.
       77  W-IC2                  PIC  9(03) VALUE 0.
       77  W-IC3                  PIC  9(03) VALUE 0.
       77  W-AUX-REG-OK           PIC  X(01) VALUE 'N'.
      *
       77  W-CD-RTN-MCI           PIC  9(05) VALUE 0.
       77  W-VL-DSPN-PRPT         PIC S9(15)V9(02) VALUE 0.
       77  W-CD-PRD-MCI           PIC  9(03) VALUE ZEROS.

       77  W-CD-PRD-OPRS5900      PIC  9(03) VALUE ZEROS.

       77  W-CD-ERRO              PIC  9(03) VALUE ZEROS.

       01  W-TX-ITC-ADC-BNF       PIC  X(175) VALUE SPACES.
       01  FILLER REDEFINES W-TX-ITC-ADC-BNF.
           03  W-TX-CMP-72 OCCURS 5 TIMES PIC X(35).

       01  W-TX-DET-PGTO-AUX.
           03  W-TX-DET-PGTO    OCCURS 100 TIMES PIC  X(01).

       01  W-TX-TRM-AUX.
           03  W-TX-TRM         OCCURS 100 TIMES PIC  X(01).

      *
      ******************************************************************
      * VARIAVEIS DE CONTROLE PARA REGRAS DE ENVIO DE PAGAMENTO
      * Cadastramento de instituicao de envio pelo menu SISBB (OPE.88)
      ******************************************************************
       77  IND-RGR-DST               PIC X(1)  VALUE 'N'.
           88  RGR-ENVO-PGTO-ATIVA   VALUE 'S'.
           88  RGR-ENVO-INATIVA      VALUE 'N'.
      *
      ******************************************************************

       01  W-CT-POUP              PIC  9(11).
       01  FILLER  REDEFINES  W-CT-POUP.
           03  W-CT-FILLER        PIC  9(02).
           03  W-VAR-CT-POUP      PIC  9(02).
           03  W-NR-CT-POUP       PIC  9(07).

       77  W-IN-NULL              PIC S9(04) COMP VALUE +0.
       77  W-NR-CMP               PIC  9(02) VALUE ZEROS.
       77  W-NR-NDX               PIC  9(05) VALUE ZEROS.
       77  W-NR-NDX-72            PIC  9(05) VALUE ZEROS.
       77  W-NR-NDX-PFRL          PIC  9(05) VALUE ZEROS.
       77  W-NR-NDX-OPR           PIC  9(05) VALUE ZEROS.
       77  W-CD-RTN               PIC +++9.
       77  W-IN-CT-MIGD           PIC  9(01) VALUE ZEROS.

       77  W-VL-TARF              PIC  9(15)V99 VALUE ZEROS.

       01  W-VL-OPR.
           03  W-VL-OPR-EDT       PIC  +++.+++.+++.++9,99.
       77  W-TX-OPR-EDT           PIC  X(01).
       77  W-VL-OPR-ALF1          PIC  X(22).
       77  W-VL-OPR-ALF2          PIC  X(22).
       77  GD-CD-TIP-DEPE         PIC  X(001).

       01  W-CD-CPSO-AUX          PIC  X(13) VALUE SPACES.

       01  W-CD-CPSO.
           03  FILLER             PIC  X(04) VALUE '//FW'.
           03  W-CD-FED-ABA       PIC  9(09) VALUE ZEROS.

       77  W-CD-PRF-DEPE-OPR-BNC  PIC  9(04) VALUE ZEROS.
       77  W-NR-CC-TMDR-BNC       PIC  9(11) VALUE ZEROS.

       77  W-CD-RTN-EIFSB014      PIC  9(03) VALUE 0.
       77  W-EIFSB014-AGENCIA-RET PIC  9(04) VALUE 0.
       77  W-EIFSB014-CONTA-RET   PIC  9(11) VALUE 0.
       77  W-CD-RTN-EIFSB018      PIC  9(03) VALUE 0.

       77  W-CD-PAIS-PSQ          PIC  9(03) VALUE ZEROS.
       77  W-NM-BCO-PSQ           PIC  X(55) VALUE SPACES.
       77  W-NM-PRCA-PSQ          PIC  X(30) VALUE SPACES.
       77  W-CD-SB-ARG            PIC  X(02) VALUE SPACES.

       01  W-CD-SWFT-PSQ          PIC  X(11) VALUE SPACES.
       01  W-CD-SWFT-R REDEFINES W-CD-SWFT-PSQ.
           03 W-CD-SWFT-A8        PIC  X(08).
           03 W-CD-SWFT-A3        PIC  X(03).

       77  W-COUNT                PIC  9(02) VALUE ZEROS.
       77  W-QT-REG               PIC  9(03) VALUE ZEROS.

       01  W-INST-FNCR.
           03  W-CD-INST   OCCURS 200 TIMES   PIC  9(012).
           03  W-NM-HDNG   OCCURS 200 TIMES   PIC  X(055).
           03  W-NM-PRCA   OCCURS 200 TIMES   PIC  X(030).
           03  W-CD-SWFT   OCCURS 200 TIMES   PIC  X(011).
      *
       77  W-CD-INST-IDFR                     PIC  9(012).
       77  W-VL-DSP-EXNO-MOEE                 PIC  9(15)V9(02).
      *
       77  W-IN-PAIS-NSC-CUBA                 PIC  X(01) VALUE SPACES.
       77  W-IND-MARC-CMBL                    PIC  X(01) VALUE 'N'.
      *
       77  W-IN-STR-ATI                       PIC  X(01) VALUE SPACES.
       77  L-IN-RGR-MT-RSCO                   PIC  X(01) VALUE 'N'.
       77  W-NR-NDX-STR                       PIC  9(03) VALUE ZEROS.
       77  W-IN-MSG-ERRO-STR                  PIC  X(01) VALUE 'N'.
       77  W-IN-MSG-CAM0021R1-STR             PIC  X(01) VALUE 'N'.
       77  W-RGR-ENT                          PIC  9(04) VALUE ZEROS.
       77  W-IN-TIP-NTZ                       PIC  9(05) VALUE ZEROS.
           88  NATUREZA-SEGURO                 VALUE 1.
      *
       01  W-MT-OBK.
           03  W-AA-EMS-OBK             OCCURS 5 TIMES PIC 9(04).
           03  W-NR-OBK                 OCCURS 5 TIMES PIC 9(09).
           03  W-CD-TIP-VCLC-OBK        OCCURS 5 TIMES PIC 9(04).
      *
       77  W-IN-NR-OBK-INFD                   PIC  X(01) VALUE 'N'.

       77  IC-OGM                             PIC S9(04) COMP VALUE 0.
       77  IC-DST                             PIC S9(04) COMP VALUE 0.
       77  TX-ENTD                            PIC  X(100) VALUE SPACES.
       77  TX-SAID                            PIC  X(100) VALUE SPACES.
       77  NM-BNFC-SEM-ESP-DUPL               PIC  X(100) VALUE SPACES.
       77  NM-TMDR-SEM-ESP-DUPL               PIC  X(100) VALUE SPACES.
      *
       01  GDA-NATUREZA                     PIC  9(012) VALUE 0.
       01  FILLER REDEFINES GDA-NATUREZA.
          03  GDA-NATU-FATO-COD             PIC  9(005).
          03  GDA-TIPO-CPDR-VDDR            PIC  9(002).
          03  GDA-TIPO-AVAL-GVFD            PIC  9(001).
          03  GDA-TIPO-PGDR-RCDR            PIC  9(002).
          03  GDA-GRPO-OPER-CBIO            PIC  9(002).
      *
       01  W-TX-NUM-N11.
           03  W-TX-NUM-EDT-N11             PIC  ++++++++++9.
      *
       01  W-TX-NUM-N9.
           03  W-TX-NUM-EDT-N9              PIC  ++++++++9.
      *
       01  W-VL-OPR-AUX.
           03  W-VL-AUX-EDT                 PIC  +++.+++.+++.+++.++9,99.
      *
       01  W-TX-VRV-EDT                     PIC  X(001) VALUE SPACES.
       01  W-TIP-ITCE-ALF1                  PIC  X(009) VALUE SPACES.
       01  W-NR-CTA-ALF1                    PIC  X(011) VALUE SPACES.
       01  W-VL-MOEE-ALF1                   PIC  X(022) VALUE SPACES.
       01  W-VL-MOEN-ALF1                   PIC  X(022) VALUE SPACES.

       01  WS-VL-TARF-MOEE                  PIC  9(015)V9(02) VALUE 0.
       01  WS-VL-TARF-MOEN                  PIC  9(015)V9(02) VALUE 0.
       01  WS-VL-TTL-MOEE                   PIC  9(015)V9(02) VALUE 0.
       01  WS-VL-TTL-MOEN                   PIC  9(015)V9(02) VALUE 0.

      *
      ************************************************************
      * VARIAVEIS DE MANIPULACAO DO NOME DO TOMADOR
      ************************************************************
      *
       01  W-NM-OGNL.
           03  W-NM-OGNL-C OCCURS 60 TIMES PIC  X(01).
       01  W-NM-AJSD.
           03  W-NM-AJSD-C OCCURS 60 TIMES PIC  X(01).
       77  W-IC                   PIC  9(05) VALUE 0.
       77  W-IC-AJSD              PIC  9(05) VALUE 0.
       77  W-IC-NOME              PIC  9(03) VALUE 0.

      ************************************************************
      * VARIAVEIS DE DATAS
      ************************************************************

       01  DT-AUX-ATU-A10                   PIC  X(10) VALUE SPACES.
       01  FILLER REDEFINES  DT-AUX-ATU-A10.
           03  DD-AUX-ATU-A10               PIC  9(02).
           03  FILLER                       PIC  X(01).
           03  MM-AUX-ATU-A10               PIC  9(02).
           03  FILLER                       PIC  X(01).
           03  AA-AUX-ATU-A10               PIC  9(04).

       01  DT-AUX-EST-N8                    PIC  9(08) VALUE ZEROS.
       01  FILLER REDEFINES  DT-AUX-EST-N8.
           03  DD-AUX-EST-N8                PIC  9(02).
           03  MM-AUX-EST-N8                PIC  9(02).
           03  AA-AUX-EST-N8                PIC  9(04).

       01  DT-AUX-EMS-AMD                   PIC  9(08) VALUE ZEROS.
       01  FILLER REDEFINES  DT-AUX-EMS-AMD.
           03  AA-AUX-EMS-AMD               PIC  9(04).
           03  MM-AUX-EMS-AMD               PIC  9(02).
           03  DD-AUX-EMS-AMD               PIC  9(02).

       01  DT-CTL-NSS-NR-SEG                PIC  9(08) VALUE ZEROS.
       01  FILLER REDEFINES  DT-CTL-NSS-NR-SEG.
           03  AA-CTL-NSS-NR-SEG            PIC  9(04).
           03  MM-CTL-NSS-NR-SEG            PIC  9(02).
           03  DD-CTL-NSS-NR-SEG            PIC  9(02).

       77  DT-AUX-EMS-N8                    PIC  9(08) VALUE ZEROS.
       77  DT-AUX-LQDC-N8                   PIC  9(08) VALUE ZEROS.
       77  W-DT-NOVO-LEI2-DMA               PIC  9(008) VALUE ZEROS.
       77  W-DT-NOVO-LEI2-AMD               PIC  9(008) VALUE ZEROS.

       01  W-DT-DMA.
           03  W-DD                         PIC  9(02) VALUE ZEROS.
           03  W-MM                         PIC  9(02) VALUE ZEROS.
           03  W-AA                         PIC  9(04) VALUE ZEROS.

       01  W-DT-AMD.
           03  W-AA                         PIC  9(04) VALUE ZEROS.
           03  W-MM                         PIC  9(02) VALUE ZEROS.
           03  W-DD                         PIC  9(02) VALUE ZEROS.

       77  W-DT-VLZC                        PIC  9(08) VALUE ZEROS.
       77  W-DT-EMS                         PIC  9(08) VALUE ZEROS.
       01  W-DT-LQDC.
           03  W-DD-LQDC                    PIC  9(02) VALUE ZEROS.
           03  W-MM-LQDC                    PIC  9(02) VALUE ZEROS.
           03  W-AA-LQDC                    PIC  9(04) VALUE ZEROS.
       77  W-DT-BLQ                         PIC  9(08) VALUE ZEROS.
       77  W-DT-DEB                         PIC  9(08) VALUE ZEROS.
       77  W-DT-MVT                         PIC  9(08) VALUE ZEROS.

       01  W-CD-ENQ-CMT                     PIC  9(009) VALUE ZEROS.
       01  FILLER REDEFINES W-CD-ENQ-CMT.
           03  W-CD-ENQ-PRCL-N7             PIC  9(007).
           03  FILLER                       PIC  9(002).

       01  W-NR-CC-TMDR-A                   PIC  X(011).
       01  FILLER REDEFINES W-NR-CC-TMDR-A.
           03  TAB-DGTO-NR-CC-TMDR-A OCCURS 11 TIMES.
               05  W-DGTO-NR-CC-TMDR-A      PIC  X(001).
       01  FILLER REDEFINES W-NR-CC-TMDR-A.
           03  W-NR-CC-TMDR-PRCL-A5         PIC  X(005).
           03  FILLER                       PIC  X(006).

       01  W-IX                             PIC  9(005).
       01  W-TAM-VRV-ALFA                   PIC  9(003).

       01  W-CD-MCI-DEB                     PIC  9(009) VALUE ZEROS.

       01  GD-RESP                          PIC S9(004) COMP.
       01  CD-PGM-CHMD                      PIC  X(08) VALUE SPACES.
       01  TX-ERRO-LOG                      PIC  X(78) VALUE SPACES.
      *
      * Variaveis do CONVERTE-CNPJ-ALFA
      *
       01  VRV-CNV-CNPJ.
           03  W-NR-IDFC-PJ-E              PIC  9(014) VALUE ZEROS.
           03  W-CD-CNPJ-ALFA-S            PIC  X(014) VALUE ZEROS.
      *
       01  W-CD-CGC-ALFA                   PIC  X(014) VALUE ZEROS.
      *
       01  TAB-ENTD-SAID-CNPJ.
           03  W-LS-NR-IDFC-PJ-E PIC 9(014) VALUE ZEROS OCCURS 20 TIMES.
           03  W-LS-CNPJ-ALFA-S  PIC X(014) VALUE ZEROS OCCURS 20 TIMES.
      *
      ************************************************************
      * BOOKS SUBROTINAS
      ************************************************************

      * SBCPU
      *----------------------------------------------------------------*
       01  W-NOME-CPU                       PIC  X(04).
       01  FILLER REDEFINES W-NOME-CPU.
           03  W-AMBIENTE                   PIC  X(03).
           03  FILLER                       PIC  X(01).

      * SBTIMEDT
      *----------------------------------------------------------------*
       01  FUNCAO-F07        PIC  X(0003) VALUE 'F07'.
       01  WK-AREA.
           03  WK-AREA-TAM   PIC  9(0009) COMP VALUE 1024.
           03  WK-AREA-DADOS PIC  X(1024) VALUE SPACES.
       01  FORMATO           PIC  X(0002) VALUE X'0000'.
       01  TAMANHO           PIC  X(0002) VALUE X'000E'.
       01  DATA-HORA         PIC  X(0014).
       01  DATA-HORA-R REDEFINES DATA-HORA.
           03 DATA-ATUAL-AMD PIC  9(0008).
           03 HORA-ATUAL     PIC  9(0006).


      * BCISO003
      *----------------------------------------------------------------*
       01  L-BCISO003.
           03  003-CD-PAIS                  PIC 9(004).
           03  003-DV-PAIS                  PIC X(001).
           03  003-NM-PAIS                  PIC X(025).
           03  003-MSG-RETORNO              PIC X(078).
           03  003-MSG-RETORNO-R REDEFINES 003-MSG-RETORNO.
               05  003-MSG                  PIC X(070).
               05  003-SQLCODE              PIC  +++9.
               05  FILLER                   PIC X(004).
           03  003-CD-ERRO                  PIC 9(002).

      * BCISO030
      *----------------------------------------------------------------*
       01  L-BCISO030.
           03  SO030-CD-ERRO                PIC  9(02).
           03  SO030-MSG-RET                PIC  X(78).
           03  FILLER REDEFINES SO030-MSG-RET.
               05  SO030-MSG-ERRO           PIC  X(70).
               05  SO030-SQLCODE            PIC  +++99.
               05  FILLER                   PIC  X(03).
           03  SO030-CD-PRF-AG              PIC  9(04).
           03  SO030-CD-TRAN                PIC  X(05).
           03  SO030-CD-TRAN-GRL            PIC  X(05).
           03  SO030-DT-MVT                 PIC  X(10).
           03  SO030-IN-VRF-HH              PIC  X(01).
           03  SO030-HH-VRF                 PIC  X(08).
           03  SO030-DT-VLZC                PIC  X(10).
           03  SO030-DT-UTIL-ANT            PIC  X(10).
      *
       77  CD-CPF-CGC-ALFA                  PIC  X(014) VALUE SPACES.
       77  W-QTD-CNPJ                       PIC  9(002) VALUE ZEROS.
      *
      * MCIS1006 - Recupera país de origem do cliente
       01  MCIS1006-DADOS.
-INC MCIB1006

      * MCIS1449 - Recupera nacionalidades do cliente
       01  MCIS1449-DADOS.
-INC MCIK1449

      * OPES012R - Recupera dados de pais (Western Union)
      *----------------------------------------------------------------*
       01  L-OPES012R.
-INC OPEK012R

      * OPES035R - Recupera naturezas autorizadas
      *----------------------------------------------------------------*
       01  L-OPES035R.
-INC OPEK035R

      * OPES036L - Recupera termos invalidos
      *----------------------------------------------------------------*
       01  L-OPES036L.
-INC OPEK036L

      * Book da gerenciador do processo OPE OBT
       01  OPES042G-DADOS.
-INC OPEK042G

      * Validar nome do estado WSUN de EUA e Mexico
       01  OPES051U-DADOS.
-INC OPEK051U

      * Validar nome de cidade WSUN do Mexico
       01  OPES052U-DADOS.
-INC OPEK052U

      * BCIS094L - Recupera Banqueiro Preferencial
      *----------------------------------------------------------------*
       01  L-BCIS094L.
-INC BCIK094L

      * BCIS4000 - Valida Regras - Geral
      *----------------------------------------------------------------*
       01  L-BCIS4000.
-INC BCIK4000

      * BCIS4010 - Valida Regras - Matriz de Risco
      *----------------------------------------------------------------*
       01  L-BCIS4010.
-INC BCIK4010

      * BCIS4015 - Valida Regras - Razao DEB
      *----------------------------------------------------------------*
       01  L-BCIS4015.
-INC BCIK4015

      * BCIS105L - Recupera País com Restrição na moeda
      *----------------------------------------------------------------*
       01  L-BCIS108L.
-INC BCIK108L

      * CPRSB001
      *----------------------------------------------------------------*
       01  L-CPRSB001.
-INC CPRKB001

      * EIFSB014
      *----------------------------------------------------------------*
-INC EIFKS014

      * EIFSB018
      *----------------------------------------------------------------*
-INC EIFKS018

      * Recupera contrato de câmbio ( CCAS006R )
      *----------------------------------------------------------------*
       01  L-CCAS006R.
-INC CCAK006R

      * Recupera mensagem STR
       01  L-CCAS052L.
-INC CCAK052L

      * Trata nova lei cambial
       01  L-CCAS0080.
-INC CCAK0080

       01  L-BDDSBLOQ.
-INC BDDKBLOQ

      * BECSO103
      *----------------------------------------------------------------*
       01  L-BECSO103.
           03  SO103-CD-RET                 PIC  9(03).
           03  SO103-MSG-RET                PIC  X(79).
           03  SO103-AG                     PIC  9(04).
           03  SO103-TIP-MC                 PIC  X(01).
           03  SO103-CD-MOE                 PIC  9(03).
           03  SO103-DT-BASE                PIC  X(08).
           03  SO103-HR-BASE                PIC  X(06).
           03  SO103-IND-CPR-VND            PIC  X(01).
           03  SO103-TIP-TAXA               PIC  X(05).
           03  SO103-DT-MOEE                PIC  X(08).
           03  SO103-DT-RSV                 PIC  X(08).
           03  SO103-TAXA-CMBL              PIC  9(06)V9999999.
           03  SO103-IPTF-CMBL              PIC  9(06)V9999999.

      * BECSO320
      *----------------------------------------------------------------*
       01  L-BECSO320.
-INC BECKO320

      * DMES502V
      *----------------------------------------------------------------*
       01  L-DMES502V.
-INC DMEK502V

      * OPRS5900
       01  L-OPRS5900.
-INC OPRK5900
      *----------------------------------------------------------------*
      * OPES0820
      *----------------------------------------------------------------*
       01  L-OPES0820.
-INC OPEK0820

      * OPES0821
      *----------------------------------------------------------------*
       01  L-OPES0821.
-INC OPEK0821

      * OPES800V
      *----------------------------------------------------------------*
       01  L-OPES800V.
-INC OPEK800V

      * OPES824S
      *----------------------------------------------------------------*
       01  L-OPES824S.
-INC OPEK824S

      * ORES0855
      *----------------------------------------------------------------*
       01  L-ORES0855.
-INC OREK0855

      * ORES0871
      *----------------------------------------------------------------*
       01  L-ORES0871.
-INC OREK0871
      *
      *
       01  L-OPES043R.
-INC OPEK043R

      * DEBSB972
      *----------------------------------------------------------------*
       01  L-DEBSB972.
           03  PARM-ENTRADA.
               05  PARM-AGEN                PIC S9(05) COMP-3  VALUE +0.
               05  PARM-CONTA               PIC S9(11) COMP-3  VALUE +0.
               05  PARM-AMBIENTE            PIC  X(01)  VALUE ' '.
           03  PARM-SAIDA.
               05  PARM-COD-RETORN          PIC  9(03).
               05  PARM-AGENCIA             PIC S9(05)  COMP-3.
               05  PARM-CONTA-RET           PIC S9(11)  COMP-3.
               05  PARM-SETEX               PIC S9(03)  COMP-3.
               05  PARM-RAZAO               PIC S9(09)  COMP.
               05  PARM-CESEC               PIC S9(04)  COMP.
               05  PARM-SUPER               PIC S9(04)  COMP.
               05  PARM-DT-ABERTURA         PIC  X(10).
               05  PARM-DT-ULT-ATIV         PIC  X(10).
               05  PARM-DT-INI-INAD         PIC  X(10).
               05  PARM-VENC-CH-ESP         PIC  X(10).
               05  PARM-DT-ULT-DBTO         PIC  X(10).
               05  PARM-DT-CONTRATO         PIC  X(10).
               05  PARM-NOM-TITULAR         PIC  X(25).
               05  PARM-NOM-SOLIDAR         PIC  X(25).
               05  PARM-CGC-CPF             PIC S9(15)  COMP-3.
               05  PARM-COD-BDC             PIC S9(09)  COMP.
               05  PARM-BDC-SOLID           PIC S9(09)  COMP.
               05  PARM-ISEN-CPMF           PIC S9(04)  COMP.
               05  PARM-PRIVAT              PIC S9(04)  COMP.
               05  PARM-OBSERVACAO          PIC  X(01).
               05  PARM-SITUACAO            PIC S9(01)  COMP-3.
               05  PARM-ISENTO-IOF          PIC S9(01)  COMP-3.
               05  PARM-PESSOA              PIC S9(01)  COMP-3.
               05  PARM-RESTRI-TALAO        PIC S9(01)  COMP-3.
               05  PARM-RESTRI-SALDO        PIC S9(01)  COMP-3.
               05  PARM-RESTRI-FALECIDO     PIC S9(01)  COMP-3.
               05  PARM-RESTRI-COR          PIC S9(01)  COMP-3.
               05  PARM-RESTRI-CCF          PIC S9(01)  COMP-3.
               05  PARM-TP-CH-ESP           PIC S9(01)  COMP-3.
               05  PARM-COD-DEPOSITO        PIC S9(01)  COMP-3.
               05  PARM-LIN-MSG             PIC  X(50).
               05  PARM-COD-STATUS          PIC S9(01)  COMP-3.
               05  PARM-RESG-AUTOM          PIC S9(17)  COMP-3.
               05  PARM-APLIC-AUTOM         PIC S9(03)  COMP-3.
               05  PARM-MT-CO-C-ESP         PIC S9(01)  COMP-3.
               05  FILLER                   PIC  X(35).

      * BDDE142H
      *----------------------------------------------------------------*
       01  L-BDDE142H.
-INC BDDK142A
-INC BDDKMSGS

      * BDDE142X
      *----------------------------------------------------------------*
       01  L-BDDE142X.
-INC BDDK142B
-INC BDDKMSGS

      * BDDE143H
      *----------------------------------------------------------------*
-INC BDDK143B
      *
      * BDDSH143
      *----------------------------------------------------------------*
       01  L-BDDSH143.
-INC BDDKH143

      * BDDE142F
      *----------------------------------------------------------------*
       01  L-BDDE142F.
-INC BDDK142C
-INC BDDKMSGS

      * SEGSB513
      *----------------------------------------------------------------*
       01  L-SEGSB513.
           03  513-CD-NSS-NR            PIC  9(17) VALUE ZEROS.
           03  513-VL-MOEE              PIC  9(13)V9(02) VALUE ZEROS.
           03  513-CD-RTN               PIC  9(02) VALUE ZEROS.
           03  513-SQL-ERRO             PIC  9(04) VALUE ZEROS.
           03  513-TX-MSG-RTN           PIC  X(78) VALUE SPACES.

      * OPEP802S
      *----------------------------------------------------------------*
       01  L-OPEP802S.
-INC OPEK802S
      *
       01  L-BCIS002R.
-INC BCIK002R
      *
       01  L-BCIS003R.
-INC BCIK003R
      *
       01  L-BCIS006R.
-INC BCIK006R
      *
       01  L-BCIS017R.
-INC BCIK017R
      *
       01  L-BCIS051N.
-INC BCIK051N
      *
      *01  L-BCIS921S.
      *-INC BCIK921S
      *
       01  L-BCIS921T.
-INC BCIK921T
      *
       01  L-BCIS218R.
-INC BCIK218R
      *
       01  L-BCIS1113.
-INC BCIK1113
      *
       01  L-BDDS833R.
-INC BDDK833A
      *
       01  L-BDDS145C.
-INC BDDK145C
      *
      * SBDATA
      *----------------------------------------------------------------*
       77  F01-FUNCAO                       PIC  X(03) VALUE 'F01'.
       77  F01-ARG01                        PIC  9(08) VALUE 0.
       77  F01-ARG02                        PIC  9(01) VALUE 2.

       77  FUNCAO                           PIC  X(03) VALUE 'F09'.
       77  ARG01                            PIC  9(08).
       77  ARG02                            PIC  9(08).
       01  ARG03                            PIC  X(05) VALUE SPACES.
       01  ARG03-RED REDEFINES ARG03.
           03  DIAS-UTEIS                   PIC S9(03).
           03  FILLER                       PIC  X(02).

      * SBDIGITO
      *----------------------------------------------------------------*
       01  PRM-DV                           PIC  X(31).
       01  FILLER REDEFINES PRM-DV.
           03  COD-FUNC                     PIC  9(03).
           03  VET-NMRO                     PIC  X(25).
           03  DIG-CALC2                    PIC  X(02).
           03  DIG-CALC-RED REDEFINES DIG-CALC2.
               05  FILLER                   PIC  X(01).
               05  DIG-CALC1                PIC  X(01).
           03  COD-ERRO                     PIC  9(01).
      *
       77  L-CD-FUC                         PIC  9(01) VALUE ZEROS.
       77  L-DT-ATU                         PIC  9(08) VALUE ZEROS.
       77  L-HR-ATU                         PIC  X(08) VALUE ZEROS.
      *
      ******************************************************************
      * BOOKS TABELAS
      ******************************************************************

      * Tabela DB2OPE.MOE_PAIS_WSUN
      *----------------------------------------------------------------*
-INC OPEKD028

      * Tabela DB2OPE.SGRA
      *----------------------------------------------------------------*
-INC OPEKD014

      * Tabela TARF_FXA_VL_ORD_PG
      *----------------------------------------------------------------*
-INC OPEKD016
      *
      * -- Grava log no HLP
       01  L-BCISLOG0.
-INC BCIKLOG0
      *
      * >> Grava log do complemento da análise da Matriz de Risco.
       01  L-BCISLOG1.
-INC BCIKLOG1
      *
      *
       01  L-BCIS204U.
-INC BCIK204U
      *=================================================================
      *    LISTA PARÂMETROS DA TABELA DB2BCI.LDR_SIS
      *=================================================================
      *
       01 L-BCIS003N.
-INC BCIK003N
      *=================================================================
      * Converte para CNPJ alfa (BCISDFE0)
      *=================================================================
       01  BCISDFE0-DADOS.
-INC BCIKDFE0
      *
      *=================================================================
      * Converte lista de CNPJ para alfa (DFES192C)
      *=================================================================
      *
       01  DFES192C-DADOS.
-INC DFEK192C
      *
      ************************************************************
      * FINAL DEFINIÇAO DE VARIAVEIS
      ************************************************************
       77  CTE-FINAL              PIC  X(24) VALUE 'WSS TERMINA AQUI'.
      ************************************************************

           EXEC SQL INCLUDE SQLCA END-EXEC.

      ************************************************************
      * FINAL DEFINIÇAO DE VARIAVEIS
      ************************************************************
       77  CTE-FINAL              PIC  X(24) VALUE 'WSS TERMINA AQUI'.

      *************************
       LINKAGE         SECTION.
      *************************
       01  DFHCOMMAREA.
-INC OPEK500V

      **************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.
      **************************************
      *
      * MOCK-COBOL  EIBCALEN
T-REXX*    MOVE 999 TO EIBCALEN

           IF  EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA
               MOVE 9999             TO S500V-CD-RTN
               STRING ' 500V-Area com tamanho invalido - verifique book/
      -            'local.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
               GOBACK
           END-IF
-INC ALMKCBL2

           CALL CICS5000 USING CICS5000-COMMAREA

           IF  CICS5000-RETCODE NOT EQUAL ZEROS
               MOVE 9999             TO S500V-CD-RTN
               STRING ' 500V-CICS5000 Erro definindo ambiente de '
                   'execucao.'
                   DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
               GOBACK
           END-IF

      *    IF  EXECUCAO-BATCH
      *        CALL SBVERSAO USING CTE-PROG CTE-VERS
      *    END-IF
           .

      *----------------------------------------------------------------*
       000-000-ROTINA-PRINCIPAL      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 000-000-ROTINA-PRINCIPAL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE S500V-MSG
               S500V-MT-CLI
               S500V-MT-INST-FNCR
               GD-CD-TIP-DEPE
               L-BCIS002R
               S500V-IN-OCOR-VLDC-RGR
               S500V-TS-VLDC-RGR
               S500V-CD-IDFC-EXEA-ANL
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           CALL SBCPU USING W-NOME-CPU.

           IF  RETURN-CODE NOT EQUAL 0
               PERFORM 999088-ERRO-088
           END-IF.

           PERFORM 050-000-PROCEDIMENTOS-INICIAIS

           IF  (S500V-CD-CNL       EQUAL 'I' OR
               S500V-CD-CNL       EQUAL 'B') AND
               S500V-CD-FASE-PRCT EQUAL 'D'
               PERFORM 105-000-VALORES-PADRAO
           END-IF.

           EVALUATE S500V-CD-FASE-PRCT
              WHEN ' '
                 PERFORM 100-000-VALIDA-DEPENDENCIA

                 IF  S500V-CD-IDFR-ORD-PGTO NOT EQUAL ZEROS
                     PERFORM 110-000-VALIDA-TRANSACAO
                 END-IF

                 IF  S500V-CD-OPC NOT EQUAL 61
                     PERFORM 105-000-VALORES-PADRAO
                 END-IF
              WHEN 'D'
              WHEN 'C'
                 IF  S500V-CD-SWFT-BNFC NOT EQUAL SPACES AND
                     (S500V-CD-TIP-ITCE-REG EQUAL 12 OR
                      S500V-CD-TIP-ITCE-REG EQUAL 15 OR
                      S500V-CD-TIP-ITCE-REG EQUAL 20 OR
                      S500V-CD-TIP-ITCE-REG EQUAL 55)
                     PERFORM 050-100-BANQUEIROS-SUSPENSOS
                 END-IF
                 IF (S500V-OPCAO EQUAL '11A' OR S500V-OPCAO EQUAL '12A')
                    IF S500V-CD-TIP-OPR EQUAL 'I'
                       EVALUATE S500V-CD-TIP-ORD-PGTO
                          WHEN 1
                             IF S500V-CD-CNL NOT EQUAL 'I' AND
                                S500V-CD-CNL NOT EQUAL 'B'

                                IF S500V-CD-CVN-ORD-PGTO = 1
                                   PERFORM 920000-VRF-CD-CPSO
                                END-IF

                                EVALUATE S500V-CD-TLA
                                   WHEN 1
                                      PERFORM 200-000-VALIDA-GERAL
                                   WHEN 2
                                      PERFORM 310-000-VALIDA-SWFT-02
                                      PERFORM 320000-VLDC-IBAN
                                      PERFORM 845-000-VALIDA-DSP-EXNO
                                      PERFORM 846-000-VLD-RMTE-DFRT-BNFC
                                   WHEN 3
                                      PERFORM 800-000-VALIDA-INST-FNCR
                                      PERFORM 845-000-VALIDA-DSP-EXNO
                                 END-EVALUATE
                             ELSE
      *                  >>     CANAL I = INTERNET
      *                  >>     CANAL B = P. BATCH
                                PERFORM 200-000-VALIDA-GERAL
                                PERFORM 310-000-VALIDA-SWFT-02
                                PERFORM 320000-VLDC-IBAN
                                PERFORM 800-000-VALIDA-INST-FNCR
                                PERFORM 845-000-VALIDA-DSP-EXNO
                                PERFORM 846-000-VLD-RMTE-DFRT-BNFC
                             END-IF
                          WHEN 2
                             IF S500V-CD-CNL NOT EQUAL 'I' AND
                                S500V-CD-CNL NOT EQUAL 'B'
                                EVALUATE S500V-CD-TLA
                                   WHEN 1
                                      PERFORM 200-000-VALIDA-GERAL
                                   WHEN 2
                                      PERFORM 410-000-VALIDA-SEGURO-02
                                END-EVALUATE
                             ELSE
      *                  >>     CANAL I = INTERNET
      *                  >>     CANAL B = P. BATCH
                                PERFORM 200-000-VALIDA-GERAL
                                PERFORM 410-000-VALIDA-SEGURO-02
                             END-IF
                          WHEN 3
                             IF S500V-CD-CNL NOT EQUAL 'I' AND
                                S500V-CD-CNL NOT EQUAL 'B'
                                EVALUATE S500V-CD-TLA
                                   WHEN 1
                                      PERFORM 200-000-VALIDA-GERAL
                                      PERFORM 500-000-VALIDA-WSUN-01
                                   WHEN 2
                                      PERFORM 510-000-VALIDA-WSUN-02
                                      PERFORM 846-000-VLD-RMTE-DFRT-BNFC
                                END-EVALUATE
                             ELSE
      *                  >>     CANAL I = INTERNET
      *                  >>     CANAL B = P. BATCH
                                PERFORM 200-000-VALIDA-GERAL
                                PERFORM 500-000-VALIDA-WSUN-01
                                PERFORM 510-000-VALIDA-WSUN-02
                                PERFORM 846-000-VLD-RMTE-DFRT-BNFC
                             END-IF
                       END-EVALUATE
                       PERFORM 900-000-VALIDA-FINAL
                    END-IF
                 END-IF
           END-EVALUATE.

           GOBACK.

      *----------------------------------------------------------------*
       050-000-PROCEDIMENTOS-INICIAIS SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 050-000-PROCEDIMENTOS-INICIAIS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      * -- Verifica novo marco cambial - Novembro 2023
           PERFORM 050-050-VERIF-NOVO-MARCO-CMB

           IF  S500V-AA-EMS-OB NOT NUMERIC
               MOVE ZEROS TO S500V-AA-EMS-OB
           END-IF

           IF  S500V-NR-OB NOT NUMERIC
               MOVE ZEROS TO S500V-NR-OB
           END-IF

           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
      *--      Inicializa variaveis da tabela interna de OBK's
               MOVE ZEROS    TO W-NR-OBK (W-IC)
               MOVE ZEROS    TO W-AA-EMS-OBK (W-IC)
               MOVE ZEROS    TO W-CD-TIP-VCLC-OBK (W-IC)
      *
               IF S500V-AA-EMS-OBK (W-IC)  IS NOT NUMERIC
                  MOVE ZEROS TO S500V-AA-EMS-OBK (W-IC)
               END-IF
               IF S500V-NR-OBK (W-IC)      IS NOT NUMERIC
                  MOVE ZEROS TO S500V-NR-OBK (W-IC)
               END-IF
               IF S500V-CD-TIP-VCLC-OBK (W-IC) IS NOT NUMERIC
                  MOVE ZEROS TO S500V-CD-TIP-VCLC-OBK (W-IC)
               END-IF
           END-PERFORM
           .
       050-000-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   050-000-SAI.'
                      .
       050-000-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       050-050-VERIF-NOVO-MARCO-CMB   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 050-050-VERIF-NOVO-MARCO-CMB.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE 'N'           TO W-IND-MARC-CMBL

      * -- Chama SBTIMEDT para recuperar data no formato AAAAMMDD
           CALL SBTIMEDT USING FUNCAO-F07 WK-AREA FORMATO
                               TAMANHO DATA-HORA

           IF RETURN-CODE NOT EQUAL 0
              PERFORM 999233-ERRO-233
           END-IF

           INITIALIZE L-BCIS003R

           MOVE 'CCA'          TO K003R-SG-SIS
           MOVE 'DT-NOVO-LEI2' TO K003R-CD-IDFC-ARQ

           MOVE LENGTH OF L-BCIS003R TO EIBCALEN
           CALL BCIS003R USING DFHEIBLK L-BCIS003R

      * MOCK-POINT MCKV1 K003R-CD-RTN-PGM

           IF K003R-CD-RTN-PGM NOT EQUAL ZEROS
              PERFORM 999234-ERRO-234
           END-IF

           IF  K003R-NR-CTL EQUAL ZEROS
               MOVE 99999999 TO W-DT-NOVO-LEI2-AMD
           ELSE
               MOVE K003R-NR-CTL TO W-DT-NOVO-LEI2-DMA
               STRING W-DT-NOVO-LEI2-DMA(05:04)
                      W-DT-NOVO-LEI2-DMA(03:02)
                      W-DT-NOVO-LEI2-DMA(01:02)
                 DELIMITED BY SIZE INTO W-DT-NOVO-LEI2-AMD
           END-IF

           IF DATA-ATUAL-AMD >= W-DT-NOVO-LEI2-AMD
              MOVE 'S'           TO W-IND-MARC-CMBL
           END-IF
           .
       050-050-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   050-050-FIM.'
                      .
       050-050-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       050-100-BANQUEIROS-SUSPENSOS   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 050-100-BANQUEIROS-SUSPENSOS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE L-BCIS003R

           MOVE 'BCI'          TO K003R-SG-SIS
           MOVE 'IN-IMPT-BANQ' TO K003R-CD-IDFC-ARQ

           MOVE LENGTH OF L-BCIS003R TO EIBCALEN
           CALL BCIS003R USING DFHEIBLK L-BCIS003R

           IF  K003R-NR-CTL EQUAL 1
      *----> Impedimento de Banqueiro Ativo
               INITIALIZE BCIS003N-RTN-PGM BCIS003N-CHAVE

               MOVE 'OPE'               TO BCIS003N-SG-SIS
               MOVE '+'                 TO BCIS003N-CD-IDFC-ARQ

               IF  EXECUCAO-ONLINE
                   EXEC CICS LINK PROGRAM (  BCIS003N  )
                                  COMMAREA( L-BCIS003N )
                                  LENGTH  ( LENGTH OF L-BCIS003N )
                   END-EXEC
               ELSE
                   MOVE LENGTH OF L-BCIS003N TO EIBCALEN
                   CALL BCIS003N USING DFHEIBLK L-BCIS003N
               END-IF

               IF  BCIS003N-CD-RTN-PGM EQUAL ZEROS
                   SEARCH BCIS003N-TAB-OCR
                     AT END
                           CONTINUE
                     WHEN S500V-CD-SWFT-BNFC(1:8) EQUAL
                     BCIS003N-CD-IDFC-ARQ-SAID(BCIN003N-IX-TAB-OCR)(2:8)
                           PERFORM 999231-ERRO-231
                   END-SEARCH
               END-IF
           END-IF
           .
       050-100-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   050-100-FIM.'
                      .
       050-100-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       100-000-VALIDA-DEPENDENCIA    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 100-000-VALIDA-DEPENDENCIA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-OPES0820
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 'emitente#'           TO W-TX-DEPE.
           MOVE S500V-CD-FASE-PRCT    TO K0820-CD-TIP-VLDC.
           MOVE S500V-CD-PRF-DEPE-EMT TO K0820-CD-PRF-DEPE-PSQ.
           MOVE S500V-CD-PRF-DEPE-USU TO K0820-CD-PRF-DEPE-USU.

           PERFORM 850-000-TRATA-DEPE.

           MOVE K0820-NM-DEPE          TO S500V-NM-DEPE-EMT.

           IF  S500V-CD-PRF-DEPE-EXEC EQUAL ZEROS AND
               S500V-CD-PRF-DEPE-TRNC EQUAL ZEROS
               MOVE K0820-CD-PRF-DEPE-EXEC TO S500V-CD-PRF-DEPE-EXEC
               MOVE K0820-CD-PRF-DEPE-TRNC TO S500V-CD-PRF-DEPE-TRNC
           END-IF.

           IF  S500V-CD-OPC          NOT EQUAL 61 AND
      *--  Transações registradas na plataforma 3.0 DE 147421
      *--  não irão validar o tipo de dependência.
               S500V-CD-TIP-ITCE-REG NOT EQUAL 55
               MOVE K0820-CD-TIP-DEPE      TO GD-CD-TIP-DEPE
      *--  Transaçoes com ordens SWIFT e SEGURO só podem ser  realizadas
      *    por dependências do grupo CAMBIO, e WSUN por qualquer agência
      *--  D.E. 2007/78054 Orgão DG pode emitir ordem SWIFT e WSUN
               IF  S500V-CD-TIP-ORD-PGTO = 1
                   IF  NOT(K0820-CD-TIP-DEPE = 'C' OR = 'T' OR = 'D')
                       GO TO 999071-ERRO-071
                   END-IF
               END-IF
               IF  S500V-CD-TIP-ORD-PGTO = 2
                   IF  NOT(K0820-CD-TIP-DEPE = 'C' OR = 'T')
                       GO TO 999071-ERRO-071
                   END-IF
               END-IF
               IF  S500V-CD-TIP-ORD-PGTO = 3
                   IF  NOT(K0820-CD-TIP-DEPE = 'C' OR = 'T'
                       OR = 'A' OR = 'D')
                       GO TO 999071-ERRO-071
                   END-IF
               END-IF

      *--      Verifica se é dependência no exterior
               IF  K0820-CD-TIP-DEPE EQUAL 'E'
                   GO TO 999067-ERRO-067
               END-IF

      *--      Verifica se a dependência possui autonomia contábil
               IF  K0820-CD-TIP-DEPE NOT EQUAL 'D'
                   AND K0820-IN-AUT-CTB NOT EQUAL 'S'
                   GO TO 999066-ERRO-066
               END-IF

           END-IF.
      *--      Busca código CAMBIO da dependência executora
           INITIALIZE L-OPES0820

           MOVE 'EXECUTORA#'           TO W-TX-DEPE
           MOVE 'R'                    TO K0820-CD-TIP-VLDC
           MOVE S500V-CD-PRF-DEPE-EXEC TO K0820-CD-PRF-DEPE-PSQ

           PERFORM 850-000-TRATA-DEPE

           MOVE K0820-NM-DEPE          TO S500V-NM-DEPE-EXEC
           MOVE K0820-CD-CMB           TO S500V-CD-CMB-DEPE-EXEC
           .
       100-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   100-099-SAIDA.'
                      .
       100-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       105-000-VALORES-PADRAO        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 105-000-VALORES-PADRAO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE 'L'               TO S500V-CD-TIP-MC.
           MOVE 702               TO S500V-CD-PRD.
           MOVE 3                 TO S500V-CD-MDLD-PRD.

           IF  S500V-CD-TIP-ORD-PGTO = 3
               IF  W-AMBIENTE EQUAL 'HOM'
                   MOVE SPACES        TO S500V-CD-IDFR-SIS
               ELSE
                   IF  W-AMBIENTE EQUAL 'DES'
                       MOVE 'BDBTEST' TO S500V-CD-IDFR-SIS
                   ELSE
                       MOVE 'BDBPROD' TO S500V-CD-IDFR-SIS
                   END-IF
               END-IF

               MOVE 'USD'        TO S500V-CD-MOE-OGM-WSUN
               MOVE 001000059801 TO S500V-CD-INST-BNFC
           END-IF.

           INITIALIZE L-BCISO030
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE  S500V-CD-PRF-DEPE-EMT TO SO030-CD-PRF-AG.
           MOVE 'CB32'                 TO SO030-CD-TRAN-GRL.
           MOVE  S500V-DT-MVT          TO SO030-DT-MVT.
           MOVE 'N'                    TO SO030-IN-VRF-HH.
           STRING 'OP' S500V-OPCAO DELIMITED BY SIZE INTO SO030-CD-TRAN.

           MOVE LENGTH OF L-BCISO030 TO EIBCALEN
           CALL BCISO030 USING DFHEIBLK L-BCISO030.

           IF  SO030-CD-ERRO NOT EQUAL 0
               GO TO 999045-ERRO-045
           END-IF.

           MOVE SO030-DT-VLZC TO S500V-DT-VLZC.

       105-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   105-099-SAIDA.'
                      .
       105-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       110-000-VALIDA-TRANSACAO      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 110-000-VALIDA-TRANSACAO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  S500V-IN-CMB-SPFD EQUAL 'S'
      *--      Carrega dados do contrato
               PERFORM 720-000-RECUPERA-CTR-CCA
           END-IF.
           EVALUATE S500V-OPCAO
               WHEN '11A'
      *--         Inclusão do registro da ordem
                   IF  S500V-CD-TIP-OPR NOT EQUAL 'I'
                       IF  (S500V-CD-EST-OPR NOT EQUAL 'R') OR
                           (S500V-IN-CNFC    NOT EQUAL 'N'  AND
                            S500V-IN-CNFC    NOT EQUAL 'L')
                           GO TO 999003-ERRO-003
                       END-IF
                   END-IF

               WHEN '11B'
      *--         ==> Reversão do registro da ordem
      *--         ==> Tipo de operação I = inclusão
                   IF  S500V-CD-TIP-OPR EQUAL 'I'
      *--             ==> Verifica STR ativo
                       PERFORM 120-000-VERIFICA-STR-ATIVO
                       IF  W-IN-STR-ATI EQUAL 'S'
      *--                ==> T - Efetivada e confirmada
                           IF  (S500V-CD-EST-OPR EQUAL 'T')
                               AND (S500V-IN-CNFC    EQUAL 'S')
      *--                     ==> Operação com câmbio simplificado
      *--                     ==> não será verificada mensagem STR.
                               IF  S500V-IN-CMB-SPFD EQUAL 'S'
                                   CONTINUE
                               ELSE
      *--                         ==> Operação permitida somente se
      *--                         ==> houver mensagem com erro no STR -
      *--                         ==> operação não efetivada no BACEN.
                                   PERFORM 130-000-VERIFICA-MSG-ERRO-STR
                                   IF  W-IN-MSG-ERRO-STR EQUAL 'N'
                                       GO TO 999151-ERRO-151
                                   END-IF
                               END-IF
                               IF  S500V-CD-TIP-ORD-PGTO EQUAL 3
                                 AND S500V-DT-EMS NOT EQUAL S500V-DT-MVT
                                   GO TO 999001-ERRO-001
                               END-IF
                           ELSE
      *--                    ==> R - Registrada
                               IF  (S500V-CD-EST-OPR NOT EQUAL 'R') OR
                                   (S500V-IN-CNFC    NOT EQUAL 'S')
                                   GO TO 999003-ERRO-003
                               END-IF
                           END-IF
                       ELSE
      *--                ==> R - Registrada
                           IF  (S500V-CD-EST-OPR NOT EQUAL 'R') OR
                               (S500V-IN-CNFC    NOT EQUAL 'S')
                               GO TO 999003-ERRO-003
                           END-IF
                           IF  S500V-CD-TIP-ORD-PGTO EQUAL 3
                               AND S500V-DT-EMS NOT EQUAL S500V-DT-MVT
                               GO TO 999001-ERRO-001
                           END-IF
                       END-IF
                   ELSE
      *--             ==> E - Excluída
                       IF  (S500V-CD-EST-OPR NOT EQUAL 'E') OR
                           (S500V-IN-CNFC    NOT EQUAL 'N')
                           GO TO 999003-ERRO-003
                       END-IF
                   END-IF

               WHEN '11C'
      *--         ==> Anulação do registro da ordem
      *--         ==> Tipo de operação I = inclusão
                   IF  S500V-CD-TIP-OPR EQUAL 'I'
      *--             ==> Verifica STR ativo
                       PERFORM 120-000-VERIFICA-STR-ATIVO
                       IF  W-IN-STR-ATI EQUAL 'S'
      *--                ==> T - Efetivada e confirmada
                           IF  (S500V-CD-EST-OPR EQUAL 'T')
                               AND (S500V-IN-CNFC    EQUAL 'S')
      *--                     ==> Operação com câmbio simplificado
      *--                     ==> não será verificada mensagem STR.
                               IF  S500V-IN-CMB-SPFD EQUAL 'S'
                                   CONTINUE
                               ELSE
      *--                         ==> Operação permitida somente se
      *--                         ==> houver mensagem CAM0021R1
      *--                         ==> respondida - operação efetivada
      *--                         ==> no BACEN.
                                   PERFORM 140-000-VERIFICA-MSG-STR
                                   IF  W-IN-MSG-CAM0021R1-STR EQUAL 'N'
                                       GO TO 999151-ERRO-151
                                   END-IF
                               END-IF
                               IF  S500V-CD-TIP-ORD-PGTO EQUAL 3
                                 AND S500V-DT-EMS NOT EQUAL S500V-DT-MVT
                                   GO TO 999001-ERRO-001
                               END-IF
                               IF  S500V-IN-CMB-SPFD     EQUAL 'S' AND
                                   K006R-NR-SEQL-CTR-PPL NOT EQUAL ZEROS
      *--                        ==> Contrato ja globalizado -
      *--                        ==> Anulaão nao permitida.
      *--                        ==> Utilizar Opção 13a.
                                   PERFORM 999122-ERRO-122
                               END-IF
                           ELSE
                               GO TO 999003-ERRO-003
                           END-IF
                       ELSE
      *--                T - Efetivada ou S - Estornada
                           IF  ((S500V-CD-EST-OPR NOT EQUAL 'T') AND
                               (S500V-CD-EST-OPR NOT EQUAL 'S')) OR
                               (S500V-IN-CNFC    NOT EQUAL 'S')
                               GO TO 999003-ERRO-003
                           END-IF
                           IF  S500V-CD-TIP-ORD-PGTO EQUAL 3
                               AND S500V-DT-EMS NOT EQUAL S500V-DT-MVT
                               GO TO 999001-ERRO-001
                           END-IF
                           IF  S500V-IN-CMB-SPFD     EQUAL 'S' AND
                               K006R-NR-SEQL-CTR-PPL NOT EQUAL ZEROS
      *--                    Contrato ja globalizado -
      *--                    Anulação nao permitida. Utilizar Opção 13a.
                               PERFORM 999122-ERRO-122
                           END-IF
                       END-IF
                   ELSE
      *--             A - Anulada
                       IF  (S500V-CD-EST-OPR NOT EQUAL 'A') OR
                           (S500V-IN-CNFC    NOT EQUAL 'N')
                           GO TO 999003-ERRO-003
                       END-IF
                   END-IF

               WHEN '12A'
      *--         Alteração do registro da ordem
                   IF  S500V-CD-TIP-OPR EQUAL 'I'
      *--             R - Registrada
                       IF  S500V-CD-EST-OPR EQUAL 'R'
                           IF  S500V-IN-GLBZ EQUAL 'S'
                               GO TO 999004-ERRO-004
                           END-IF
                       ELSE
                           GO TO 999003-ERRO-003
                       END-IF
                   ELSE
      *--             D - Alterada
                       IF  (S500V-CD-EST-OPR NOT EQUAL 'D') OR
                           (S500V-IN-CNFC    NOT EQUAL 'N')
                           GO TO 999003-ERRO-003
                       END-IF
                   END-IF
               WHEN '13A'
      *--         Cancelamento do registro da ordem
                   IF  S500V-CD-TIP-OPR EQUAL 'I'
                       IF  S500V-IN-CMB-SPFD EQUAL 'S'
      *--                 ==> Câmbio Simplif - T Efetivada/ L Liquidada
                           IF  (S500V-CD-EST-OPR NOT EQUAL 'L') AND
                               (S500V-CD-EST-OPR NOT EQUAL 'T') OR
                               (S500V-IN-CNFC    NOT EQUAL 'S')
                               GO TO 999003-ERRO-003
                           END-IF
                       ELSE
                           IF  S500V-CD-EST-OPR   EQUAL 'L'
      *--                     ==> Ordem liquidada - Cancelamento nao
      *--                     ==> permitido.  *-- Utilizar Opção 33c.
                               PERFORM 999152-ERRO-152
                           END-IF
      *--                 T - Efetivada e confirmada
                           IF  S500V-CD-EST-OPR EQUAL 'T'
                               AND S500V-IN-CNFC    EQUAL 'S'
      *--                     ==> Verifica STR ativo
                               PERFORM 120-000-VERIFICA-STR-ATIVO
                               IF  W-IN-STR-ATI EQUAL 'S'
      *--                         ==> Operação com câmbio simplificado
      *--                         ==> não será verificada mensagem STR.
                                   IF  S500V-IN-CMB-SPFD EQUAL 'S'
                                       CONTINUE
                                   ELSE
      *--                             ==> Operação permitida somente se
      *--                             ==> houver mensagem CAM0021R1
      *--                             ==> respondida - operaão
      *--                             ==> efetivada no BACEN.
                                       PERFORM 140-000-VERIFICA-MSG-STR
                                       IF  W-IN-MSG-CAM0021R1-STR = 'N'
                                           GO TO 999151-ERRO-151
                                       END-IF
                                   END-IF
                               END-IF
                           ELSE
                               GO TO 999003-ERRO-003
                           END-IF
                       END-IF
                       IF  S500V-IN-CMB-SPFD     EQUAL 'S' AND
                           K006R-NR-SEQL-CTR-PPL EQUAL ZEROS AND
                           K006R-NR-BC-CTR       EQUAL ZEROS
      *--                 ==> Contrato ainda nao globalizado -
      *--                 ==> Cancelamento nao permitido.
      *--                 ==> Utilizar Opção 11c.
                           PERFORM 999123-ERRO-123
                       END-IF
                   ELSE
      *--             C - Cancelada
                       IF  (S500V-CD-EST-OPR NOT EQUAL 'C') OR
                           (S500V-IN-CNFC    NOT EQUAL 'N')
                           GO TO 999003-ERRO-003
                       END-IF
                   END-IF

               WHEN '61 '
      *--         Consulta
                   CONTINUE
               WHEN OTHER
                   GO TO 999005-ERRO-005
           END-EVALUATE.

       110-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   110-099-SAIDA.'
                      .
       110-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *
      *----------------------------------------------------------------*
       120-000-VERIFICA-STR-ATIVO    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 120-000-VERIFICA-STR-ATIVO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      *--  --> W-IN-STR-ATI - Indicador STR Ativo
           MOVE 'N' TO W-IN-STR-ATI

           INITIALIZE L-BCIS003R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 'CCA'    TO K003R-SG-SIS.
           MOVE 'STR-BC' TO K003R-CD-IDFC-ARQ.

           MOVE LENGTH OF L-BCIS003R TO EIBCALEN
           CALL BCIS003R USING DFHEIBLK L-BCIS003R.

           IF  K003R-CD-RTN-PGM NOT EQUAL ZEROS
               PERFORM 999149-ERRO-149
           END-IF.

           IF  K003R-NR-CTL EQUAL 1
      *--      --> STR - Ativo
               MOVE 'S' TO W-IN-STR-ATI
           ELSE
      *--      --> STR - Inativo
               MOVE 'N' TO W-IN-STR-ATI
           END-IF.
      *
       120-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   120-099-SAIDA.'
                      .
       120-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *
      *----------------------------------------------------------------*
       130-000-VERIFICA-MSG-ERRO-STR SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 130-000-VERIFICA-MSG-ERRO-STR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
           INITIALIZE L-CCAS052L
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 1                       TO K052L-CD-FUC.
           MOVE S500V-NR-SEQL-CTR-VND   TO K052L-NR-SEQL-CTR-ENT.

           MOVE LENGTH OF L-CCAS052L TO EIBCALEN
           CALL CCAS052L USING DFHEIBLK L-CCAS052L.

           IF  K052L-CD-RTN EQUAL ZEROS
               MOVE 'N' TO W-IN-MSG-ERRO-STR
               PERFORM VARYING W-NR-NDX-STR FROM 01 BY 01
                   UNTIL W-NR-NDX-STR GREATER K052L-QT-REG-SAI
                   OR K052L-CD-IDFR-MSG-CMB-SAI(W-NR-NDX-STR)
                   EQUAL ZEROS
                   IF  K052L-CD-EST-MSG-ENV-SAI(W-NR-NDX-STR) EQUAL 4
                       MOVE 'S'   TO W-IN-MSG-ERRO-STR
                   END-IF
               END-PERFORM
           ELSE
               IF  K052L-CD-SQLCODE-RTN EQUAL +100
      *--         ==> Operaçoes em estoque FEPI retornarão SQL-100
      *--             não deverá parar o processamento
                   MOVE 'S'   TO W-IN-MSG-ERRO-STR
               ELSE
                   PERFORM 999150-ERRO-150
               END-IF
           END-IF.
      *
       130-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   130-099-SAIDA.'
                      .
       130-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *----------------------------------------------------------------*
       140-000-VERIFICA-MSG-STR      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 140-000-VERIFICA-MSG-STR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
           INITIALIZE L-CCAS052L
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 1                       TO K052L-CD-FUC.
           MOVE S500V-NR-SEQL-CTR-VND   TO K052L-NR-SEQL-CTR-ENT.

           MOVE LENGTH OF L-CCAS052L TO EIBCALEN
           CALL CCAS052L USING DFHEIBLK L-CCAS052L.

           IF  K052L-CD-RTN EQUAL ZEROS
               MOVE 'N' TO W-IN-MSG-CAM0021R1-STR
               PERFORM VARYING W-NR-NDX-STR FROM 01 BY 01
                   UNTIL W-NR-NDX-STR GREATER K052L-QT-REG-SAI
                   OR K052L-CD-IDFR-MSG-CMB-SAI(W-NR-NDX-STR)
                   EQUAL ZEROS
                   IF  K052L-CD-EST-MSG-ENV-SAI(W-NR-NDX-STR) EQUAL 3
                       MOVE 'S'   TO W-IN-MSG-CAM0021R1-STR
                   END-IF
               END-PERFORM
           ELSE
               IF  K052L-CD-SQLCODE-RTN EQUAL +100
      *--         ==> Operaçoes em estoque FEPI retornarão SQL-100
      *--             não deverá parar o processamento
                   MOVE 'S' TO W-IN-MSG-CAM0021R1-STR
               ELSE
                   PERFORM 999150-ERRO-150
               END-IF
           END-IF.
      *
       140-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   140-099-SAIDA.'
                      .
       140-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *----------------------------------------------------------------*
       150-000-VERIFICA-MT-ATIVO        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 150-000-VERIFICA-MT-ATIVO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      * >> L-IN-RGR-MT-RSCO - Indicador Matriz de Risco Ativa
           MOVE 'N'                     TO L-IN-RGR-MT-RSCO

           INITIALIZE L-BCIS003R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0

           MOVE 'OPE'                   TO K003R-SG-SIS
           MOVE 'RGR-MT-RSCO'           TO K003R-CD-IDFC-ARQ

           MOVE LENGTH OF L-BCIS003R TO EIBCALEN
           CALL BCIS003R USING DFHEIBLK L-BCIS003R

           IF  K003R-CD-RTN-PGM NOT EQUAL ZEROS
               PERFORM 999149-ERRO-149
           END-IF.

           IF  K003R-NR-CTL EQUAL 1
               MOVE 'S'                 TO L-IN-RGR-MT-RSCO
           ELSE
               MOVE 'N'                 TO L-IN-RGR-MT-RSCO
           END-IF
           .
       150-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   150-099-SAIDA.'
                      .
       150-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       200-000-VALIDA-GERAL          SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 200-000-VALIDA-GERAL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Valida Contrato prévio CCA
           PERFORM 201-000-VALIDA-CTR-PREVIO.

      *--  Valida tomador.
           PERFORM 210-000-VALIDA-TMDR.

      *--  Valida forma de intermediação e agencia/conta.
           PERFORM 220-000-VALIDA-FMA-INTM.

      *--  Valida datas (emissão, liquidação, trava e débito).
           IF  S500V-IN-ORPG-AGDD NOT EQUAL 'S'
               PERFORM 230-000-VALIDA-DATAS
           END-IF.

      *--  Valida mercado de câmbio
           IF  S500V-CD-TIP-MC NOT EQUAL 'L'
               GO TO 999002-ERRO-002
           END-IF.

      *--  Valida país do beneficiário (ordem Western Union)
           IF  S500V-CD-TIP-ORD-PGTO = 3
               PERFORM 700-000-VALIDA-PAIS-BNFC
           END-IF.

      *--  Valida código da moeda.
           PERFORM 240-000-VALIDA-CD-MOEDA.

      *--  Valida Natureza do fato operacional ,
      *           Tipo de Comprador/Vendedor   ,
      *           Tipo de Avaliacao            ,
      *           Tipo de Pagador/Recebedor    e
      *           Grupo de Operacoes de Cambio.
           PERFORM 840-000-VALIDA-NATUREZA.

      *--  Valida combinacao da natureza dentro do parametrizador do BCI
      *--  Nao valida execucao batch ou canal Webservice
           IF  S500V-CD-CNL NOT EQUAL 'B'
           AND S500V-CD-CNL NOT EQUAL 'W'
      *--  Nao valida codigo comprador/vendedor para novo marco cambial
      *--  O codigo comprador/vendedor sera sempre 67 nesse caso
              IF  W-IND-MARC-CMBL EQUAL 'S'
                  IF S500V-CD-CPRD-VNDR-MOE NOT EQUAL 67
                     PERFORM 999235-ERRO-235
                  END-IF
              ELSE
                  PERFORM 841-000-VALIDAR-CPRD-VNDR
              END-IF
              PERFORM 842-000-VERIFICA-NATUREZA
           END-IF

      *--  Valida forma de entrega da moeda.
           PERFORM 250-000-VALIDA-FMA-ETGA-MOE.

      *--  Valida valor em moeda estrangeira.
           IF  S500V-VL-MOEE  EQUAL 0
               GO TO 999136-ERRO-136
           END-IF.

      *--  Acao 83690 - Nao permite emissao de ordem com valor em moeda
      *--  estrangeira menor do que 1.00
           IF  S500V-VL-MOEE  < 1
               GO TO 999153-ERRO-153
           END-IF.

      *--  Busca taxa de câmbio, calcula/valida MOEN e valida IOF
           IF  S500V-IN-ORPG-AGDD NOT EQUAL 'S'
               IF  S500V-CD-FMA-INTM EQUAL 4

      *--      DE 90120 Ação 52379 Chama BEC para verificar
      *        se operação é pronta
                   PERFORM 880-000-CHAMA-BECSO103

      *--      Para intermediação (4-conta em moeda estrangeira) deve-se
      *        buscar a taxa da moeda para cálculo da moeda nacional
                   PERFORM 890-000-BUSCA-TAXA

      *--      Esta intermediação não aceita IOF.
                   IF  S500V-PC-IOF NOT EQUAL ZEROS
                       GO TO 999086-ERRO-086
                   END-IF
               ELSE
      *--      Demais intermediaçoes: taxa cambial é recuperada do BEC
                   IF  S500V-VL-TAXA-CMB EQUAL 0
                       IF  S500V-NR-BLT-BEC EQUAL ZEROS
                           PERFORM 880-000-CHAMA-BECSO103
                       ELSE
                           PERFORM 885-000-VALIDA-PROPOSTA
                       END-IF

      *--          Calcula valor em moeda nacional
                       COMPUTE S500V-VL-MOEN ROUNDED =
                           S500V-VL-MOEE * S500V-VL-TAXA-CMB
                       ON SIZE ERROR
                          PERFORM 999190-ERRO-190
                       END-COMPUTE
                   ELSE
      *--          Calcula valores limite em moeda nacional
                       IF  S500V-VL-MOEE <= W-VL-VRC
                           MOVE 0,01 TO W-VL-MOEN-MIN
                       ELSE
                           COMPUTE W-VL-MOEN-MIN ROUNDED =
                        ( S500V-VL-MOEE - W-VL-VRC ) * S500V-VL-TAXA-CMB
                           ON SIZE ERROR
                              PERFORM 999191-ERRO-191
                           END-COMPUTE
                       END-IF

                       COMPUTE W-VL-MOEN-MAX ROUNDED =
                        ( S500V-VL-MOEE + W-VL-VRC ) * S500V-VL-TAXA-CMB
                       ON SIZE ERROR
                          PERFORM 999192-ERRO-192
                       END-COMPUTE

                       IF  S500V-VL-MOEN < W-VL-MOEN-MIN
                           OR  S500V-VL-MOEN > W-VL-MOEN-MAX
                           GO TO 999075-ERRO-075
                       END-IF
                   END-IF

      *--      Calcula/valida IOF
                   IF  S500V-PC-IOF NOT EQUAL ZEROS
                       IF  S500V-CD-FMA-INTM EQUAL 3
      *                Esta intermediação (3-OBT) Não aceita IOF.
                           GO TO 999087-ERRO-087
                       END-IF
                       MOVE 'S'  TO S500V-IN-IOF
                   ELSE
                       MOVE 'N'  TO S500V-IN-IOF
                   END-IF
               END-IF
           ELSE
               IF  S500V-NR-BLT-BEC NOT EQUAL ZEROS
                   PERFORM 885-000-VALIDA-PROPOSTA
               END-IF
           END-IF.

           IF S500V-CD-FMA-INTM EQUAL 3
              IF  S500V-CD-CNL      NOT EQUAL 'B'
              AND S500V-CD-CVN-ORD-PGTO EQUAL 2
                  IF  S500V-AA-EMS-OB NOT EQUAL ZEROS
                  OR  S500V-NR-OB     NOT EQUAL ZEROS
                      PERFORM 999177-ERRO-177
                  END-IF
                  PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
                      IF S500V-AA-EMS-OBK (W-IC)  NOT EQUAL ZEROS
                      OR S500V-NR-OBK (W-IC)      NOT EQUAL ZEROS
                      OR S500V-CD-TIP-VCLC-OBK (W-IC) NOT EQUAL ZEROS
                         PERFORM 999177-ERRO-177
                      END-IF
                  END-PERFORM
              END-IF

              IF (S500V-CD-CNL          NOT EQUAL 'B'
              AND S500V-CD-CVN-ORD-PGTO NOT EQUAL 2)
              OR (S500V-CD-CNL         EQUAL 'B'
              AND S500V-SG-SIS-OGM NOT EQUAL ' '
              AND S500V-NR-RMS     NOT EQUAL 0)
                  PERFORM 228-000-VALIDA-PRC-OB
              ELSE
                  MOVE 99738085          TO S500V-NR-CC-TMDR
                  MOVE '3'               TO S500V-DV-CC-TMDR
              END-IF
           END-IF

      *--  Valida informacoes IR para o contrato
           IF  S500V-CD-CNL EQUAL 'I'
               PERFORM 251-000-VALIDA-DADOS-IR
           END-IF

      *--  Valida regras - Matriz de risco
      *--  Não valida para Web-Service (CCAS015B)
           IF  S500V-CD-TIP-ITCE-REG NOT EQUAL 23
               PERFORM 150-000-VERIFICA-MT-ATIVO
               PERFORM 970-000-VALIDA-REGRAS
           END-IF
           .
       200-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   200-099-SAIDA.'
                      .
       200-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       201-000-VALIDA-CTR-PREVIO     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 201-000-VALIDA-CTR-PREVIO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Emissão com contrato prévio CCA somente para ordens Swift e
      *--  Seguro.

           IF  S500V-NR-CTR-BC-EDTD  NOT EQUAL ZEROS
               IF  S500V-CD-TIP-ORD-PGTO NOT EQUAL 1 AND
                   S500V-CD-TIP-ORD-PGTO NOT EQUAL 2
                   GO TO 999145-ERRO-145
               END-IF
           END-IF.

      *--  Emissão com contrato prévio CCA não permite intermediação DME

           IF  S500V-NR-CTR-BC-EDTD  NOT EQUAL ZEROS
               AND S500V-CD-FMA-INTM         EQUAL 4
               GO TO 999146-ERRO-146
           END-IF.

       201-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   201-099-SAIDA.'
                      .
       201-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       210-000-VALIDA-TMDR           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 210-000-VALIDA-TMDR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-OPES0821
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-CD-CLI     TO K0821-CD-CLI.
           MOVE S500V-CD-CPF-CGC TO K0821-CD-CPF-CNPJ.

           MOVE LENGTH OF L-OPES0821 TO EIBCALEN
           CALL OPES0821 USING DFHEIBLK L-OPES0821.

           IF  K0821-CD-RTN NOT EQUAL ZEROS
               GO TO 999090-ERRO-090
           ELSE
               IF  K0821-QT-CLI > 1
      *--          Se existir mais de um cliente para o CNPJ/CPF,retorna
      *            erro 8888 para  permitir a escolha de qual cliente na
      *            interface de inclusão do registro da ordem.
                   PERFORM VARYING W-NR-NDX FROM 1 BY 1
                       UNTIL   W-NR-NDX GREATER K0821-QT-CLI
                       MOVE K0821-CD-CPF-CNPJ-LS(W-NR-NDX)
                           TO S500V-CD-CPF-CGC-LS(W-NR-NDX)
                       MOVE K0821-NM-CLI-LS(W-NR-NDX)
                           TO S500V-NM-CLI-LS(W-NR-NDX)
                       MOVE K0821-CD-TIP-CLI-LS(W-NR-NDX)
                           TO S500V-CD-TIP-CLI-LS(W-NR-NDX)
                       MOVE K0821-CD-CLI-LS(W-NR-NDX)
                           TO S500V-CD-CLI-LS(W-NR-NDX)
                   END-PERFORM
                   MOVE 8888 TO S500V-CD-RTN
                   GOBACK
               END-IF
           END-IF.

           MOVE K0821-CD-CLI           TO S500V-CD-CLI.
           MOVE K0821-CD-TIP-CLI       TO S500V-CD-TIP-CLI.
           MOVE K0821-NM-CLI           TO S500V-NM-TMDR.
           MOVE K0821-CD-PRF-DEPE-FCAD TO S500V-CD-PRF-DEPE-FCAD.
           MOVE K0821-TX-END-CLI       TO S500V-TX-END-TMDR.
           MOVE K0821-TX-CID-CLI       TO S500V-NM-CID-TMDR.
           MOVE K0821-TX-CEP           TO S500V-CD-CEP-TMDR.
           MOVE K0821-TX-UF            TO S500V-NM-EST-TMDR.
           MOVE K0821-TX-TEL           TO S500V-NR-TEL-TMDR.
           MOVE K0821-DT-NSC-CLI       TO S500V-DT-NSC-TMDR.

      *--  Recupera dados para ordens Western Union
           IF  S500V-CD-TIP-ORD-PGTO = 3
      *--      Tipo de documento, conforme Demanda 63760
      *        IF  K0821-CD-TIP-DOC-CLI >= 20
      *            AND K0821-CD-TIP-DOC-CLI <= 30
      *            IF  K0821-CD-TIP-DOC-CLI = 22
      *--              Passaport
      *                MOVE 'A'          TO S500V-TX-TIP-DOC-TMDR
      *            ELSE
      *--              National ID
      *                MOVE 'B'          TO S500V-TX-TIP-DOC-TMDR
      *            END-IF
      *            MOVE K0821-CD-DOC-CLI TO S500V-CD-DOC-TMDR
      *--      Acao 172257 - manda "E" no tipo de documento,
      *--      Acao 172257 - manda CPF no codigo do documento
                   MOVE 'E'              TO S500V-TX-TIP-DOC-TMDR
                   MOVE S500V-CD-CPF-CGC TO S500V-CD-DOC-TMDR
                   MOVE 'BR'             TO S500V-SG-PAIS-DOC-TMDR
      *        ELSE
      *            MOVE ' '              TO S500V-TX-TIP-DOC-TMDR
      *            MOVE SPACES           TO S500V-CD-DOC-TMDR
      *            MOVE '  '             TO S500V-SG-PAIS-DOC-TMDR
      *        END-IF

      *--      Move 'BR' fixo cfe correio 2006/05977769 de 13.06.2006.
               MOVE 'BR'                 TO S500V-SG-PAIS-TMDR

               MOVE SPACES       TO W-NM-OGNL
                   W-NM-AJSD
               MOVE K0821-NM-CLI TO W-NM-OGNL
               PERFORM 211-000-AJUSTA-NOME
           END-IF

      *--- Bloqueia envio por cliente cubano ou nacionalizado cubano
      *--- para dolar americano e canadense - somente PF
           IF  S500V-CD-TIP-CLI EQUAL 1
                IF  S500V-CD-MOE     EQUAL 220
                OR  S500V-CD-MOE     EQUAL 165
                    PERFORM 212-000-VERIFICA-CUBANO
                END-IF
           END-IF
           .
       210-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   210-099-SAIDA.'
                      .
       210-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       211-000-AJUSTA-NOME           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 211-000-AJUSTA-NOME.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Separa primeiro e ultimo nome do tomador.
           MOVE 60 TO W-IC.
           MOVE 0  TO W-IC-AJSD.
           MOVE 0  TO W-IC-NOME.
           PERFORM UNTIL W-IC-AJSD EQUAL 1 OR W-IC EQUAL 0
               IF  (W-NM-OGNL-C(W-IC) NOT EQUAL SPACES)
                   ADD 1  TO W-IC-NOME
               END-IF
               IF  (W-IC-NOME NOT EQUAL ZEROS) AND
                   (W-NM-OGNL-C(W-IC) EQUAL SPACES)
                   ADD 1  TO W-IC-AJSD W-IC
                   MOVE W-NM-OGNL(W-IC:W-IC-NOME) TO W-NM-AJSD
               END-IF
               SUBTRACT 1 FROM W-IC
           END-PERFORM.

           MOVE W-NM-AJSD           TO S500V-ULT-NM-TMDR.
      *
           IF  W-IC NOT EQUAL 0
               MOVE W-NM-OGNL(1:W-IC)      TO S500V-PRMO-NM-TMDR
           ELSE
               MOVE W-NM-OGNL(1:W-IC-NOME) TO S500V-PRMO-NM-TMDR
           END-IF.

       211-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   211-099-SAIDA.'
                      .
       211-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       212-000-VERIFICA-CUBANO           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 212-000-VERIFICA-CUBANO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE 'N' TO W-IN-PAIS-NSC-CUBA

           INITIALIZE MCIS1006-CONTROLE
                      MCIS1006-ENTRADA

           MOVE 'OPE'                       TO MCIS1006-SYS-CHAMADOR

           MOVE 'OPES500V'                  TO MCIS1006-PRG-CHAMADOR
           MOVE 1                           TO MCIS1006-FUNCAO
           IF  S500V-CD-CNL EQUAL 'I'
               MOVE S500V-CD-CLI            TO MCIS1006-COD-MCI
           ELSE
               MOVE S500V-CD-USU            TO MCIS1006-USUARIO
               MOVE S500V-CD-PRF-DEPE-USU   TO MCIS1006-COD-DEPE
           END-IF
           MOVE S500V-CD-CLI                TO MCIS1006-COD-E

           MOVE LENGTH OF MCIS1006-DADOS TO EIBCALEN
           CALL MCIS1006 USING DFHEIBLK MCIS1006-DADOS

           IF  MCIS1006-RET-CODE NOT EQUAL 0 AND
               MCIS1006-SQLCODE  NOT EQUAL 100
               GO TO 999182-ERRO-182
           ELSE
               IF  MCIS1006-SQLCODE  NOT EQUAL 100
      * ------- Cliente cubano
                   IF  MCIS1006-COD-PAIS-ORIG-S (1) EQUAL 42
                          MOVE 'S' TO W-IN-PAIS-NSC-CUBA
                   END-IF
               ELSE
                  GO TO 999185-ERRO-185
               END-IF
           END-IF

           INITIALIZE MCIS1449-CONTROLE
                      MCIS1449-ENTRADA

           MOVE 'OPE'                        TO MCIS1449-SYS-CHAMADOR
           MOVE 'OPES500V'                   TO MCIS1449-PRG-CHAMADOR
           MOVE 1                            TO MCIS1449-FUNCAO
           IF  S500V-CD-CNL EQUAL 'I'
               MOVE S500V-CD-CLI             TO MCIS1449-COD-MCI
           ELSE
               MOVE S500V-CD-USU             TO MCIS1449-USUARIO
               MOVE S500V-CD-PRF-DEPE-USU    TO MCIS1449-COD-DEPE
           END-IF
           MOVE S500V-CD-CLI                 TO MCIS1449-CD-CLI

           MOVE LENGTH OF MCIS1449-DADOS TO EIBCALEN
           CALL MCIS1449 USING DFHEIBLK MCIS1449-DADOS

           IF  MCIS1449-RET-CODE NOT EQUAL 0 AND
               MCIS1449-SQLCODE  NOT EQUAL 100
               GO TO 999183-ERRO-183
           ELSE
               IF  MCIS1449-SQLCODE NOT EQUAL 100
                   PERFORM VARYING W-NR-NDX FROM 1 BY 1
                     UNTIL W-NR-NDX GREATER MCIS1449-QTD-RET
                       IF  MCIS1449-CD-PAIS-NCLD-CLI-S
                          (W-NR-NDX) EQUAL 42
                              MOVE 'S' TO W-IN-PAIS-NSC-CUBA
                       END-IF
                   END-PERFORM
               ELSE
                   GO TO 999186-ERRO-186
               END-IF
           END-IF
           .
       212-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   212-099-SAIDA.'
                      .
       212-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       220-000-VALIDA-FMA-INTM       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 220-000-VALIDA-FMA-INTM.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *--  +-----------------------------------------------------------+
      *    | Formas de Intermediacao:           SWIFT  SEGURO  W.UNION |
      *    | ----------------------------------------------------------|
      *    | 1 -> Conta corrente                 Sim     Sim     Sim   |
      *    | 2 -> Caixa                          Sim     Sim     Não   |
      *    | 3 -> Ordens bancárias do tesouro    Sim     Sim     Não   |
      *    | 4 -> Conta em moeda estrangeira     Sim     Não     Não   |
      *    | 5 -> Depósito vinculado             Sim     Sim     Não   |
      *    | 6 -> Conta poupança                 Sim     Sim     Sim   |
      *    +-----------------------------------------------------------+
      *
           IF  (S500V-CD-TIP-ORD-PGTO = 2 AND S500V-CD-FMA-INTM = 4)
               OR  (S500V-CD-TIP-ORD-PGTO = 3 AND
               NOT(S500V-CD-FMA-INTM = 1 OR = 6))
               GO TO 999006-ERRO-006
           END-IF.

      *--  Valida meio de intermediação para dependências DG.
           IF  S500V-CD-TIP-ORD-PGTO = 1 AND
               NOT(S500V-CD-FMA-INTM = 1 OR = 6)
               PERFORM 227-000-VALIDA-FMA-INTM-DG
           END-IF.
           DISPLAY 'BATATA 1'
           EVALUATE S500V-CD-FMA-INTM
               WHEN 1
                   DISPLAY 'BATATA 2'
                   MOVE 'Conta corrente'  TO S500V-TX-FMA-INTM

                   PERFORM 221-000-VALIDA-DIGITO-AGENCIA
                   PERFORM 222-000-VALIDA-DIGITO-CONTA
                   MOVE 6   TO W-CD-PRD-OPRS5900
                   PERFORM 223-000-VALIDA-AGENCIA-CONTA
                   PERFORM 229-000-VALIDA-RAZAO-DEB
               WHEN 2

                   MOVE 'Caixa'               TO S500V-TX-FMA-INTM
                   MOVE S500V-CD-PRF-DEPE-EMT TO S500V-CD-PRF-DEPE-OPR
                   MOVE 332531200             TO S500V-NR-CC-TMDR
                   MOVE '6'                   TO S500V-DV-CC-TMDR
               WHEN 3
                   MOVE 'OBT'                 TO S500V-TX-FMA-INTM

      *            IF  S500V-CD-CNL      NOT EQUAL 'B'
      *            AND S500V-CD-CVN-ORD-PGTO EQUAL 2
      *                IF  S500V-AA-EMS-OB NOT EQUAL ZEROS
      *                OR  S500V-NR-OB     NOT EQUAL ZEROS
      *                    PERFORM 999177-ERRO-177
      *                END-IF
      *            END-IF
      *
      *            IF (S500V-CD-CNL          NOT EQUAL 'B'
      *            AND S500V-CD-CVN-ORD-PGTO NOT EQUAL 2)
      *            OR (S500V-CD-CNL         EQUAL 'B'
      *            AND S500V-SG-SIS-OGM NOT EQUAL ' '
      *            AND S500V-NR-RMS     NOT EQUAL 0)
      *                PERFORM 228-000-VALIDA-PRC-OB
      *            ELSE
      *                MOVE 99738085          TO S500V-NR-CC-TMDR
      *                MOVE '3'               TO S500V-DV-CC-TMDR
      *            END-IF

                   INITIALIZE L-OPES0820
                       REPLACING ALPHANUMERIC BY ' '
                       NUMERIC BY  0
                   MOVE 'da conta#'           TO W-TX-DEPE
                   MOVE 'R'                   TO K0820-CD-TIP-VLDC
                   MOVE S500V-CD-PRF-DEPE-OPR TO K0820-CD-PRF-DEPE-PSQ
                   PERFORM 850-000-TRATA-DEPE

                   IF  K0820-IN-AUT-CTB NOT EQUAL 'S'
                       GO TO 999066-ERRO-066
                   END-IF
               WHEN 4
                  MOVE 'Conta em moeda estrangeira' TO S500V-TX-FMA-INTM
                   IF  S500V-NR-BLT-BEC NOT EQUAL ZEROS
                       GO TO 999084-ERRO-084
                   END-IF

                   PERFORM 221-000-VALIDA-DIGITO-AGENCIA
                   PERFORM 222-000-VALIDA-DIGITO-CONTA
                   PERFORM 224-000-VERIFICA-CT-MOEE
               WHEN 5
                   MOVE 'Depósito vinculado' TO S500V-TX-FMA-INTM
                   PERFORM 221-000-VALIDA-DIGITO-AGENCIA
                   PERFORM 222-000-VALIDA-DIGITO-CONTA
                   PERFORM 225-000-VALIDA-AG-CTA-VNCL
               WHEN 6
                   MOVE 'Conta poupanca'     TO S500V-TX-FMA-INTM

                   PERFORM 221-000-VALIDA-DIGITO-AGENCIA
                   PERFORM 222-000-VALIDA-DIGITO-CONTA
                   PERFORM 222-100-VALIDA-VRC-NR-CT-POUP

                   MOVE S500V-CD-PRF-DEPE-OPR  TO W-CD-PRF-DEPE-OPR-BNC
                   MOVE S500V-NR-CC-TMDR       TO W-NR-CC-TMDR-BNC

                   MOVE 3   TO W-CD-PRD-OPRS5900
                   PERFORM 226-000-VALIDA-AG-CONTA-POUP
               WHEN OTHER
                   GO TO 999006-ERRO-006
           END-EVALUATE.

       220-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   220-099-SAIDA.'
                      .
       220-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       221-000-VALIDA-DIGITO-AGENCIA SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 221-000-VALIDA-DIGITO-AGENCIA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Calcula dígito da agência.
           MOVE S500V-CD-PRF-DEPE-OPR TO W-CD-PRF-DEPE.

           MOVE 011           TO COD-FUNC.
           MOVE W-CD-PRF-DEPE TO VET-NMRO.

           CALL SBDIGITO USING COD-FUNC VET-NMRO DIG-CALC2 COD-ERRO.

           IF  RETURN-CODE NOT EQUAL ZEROS
               GO TO 999016-ERRO-016
           END-IF.

           IF  DIG-CALC1 NOT EQUAL S500V-DV-PRF-DEPE-OPR
               GO TO 999017-ERRO-017
           END-IF.

       221-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   221-099-SAIDA.'
                      .
       221-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       222-000-VALIDA-DIGITO-CONTA   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 222-000-VALIDA-DIGITO-CONTA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Calcula dígito da conta corrente.

           MOVE 011              TO COD-FUNC.
           MOVE S500V-NR-CC-TMDR TO VET-NMRO.

           CALL SBDIGITO USING COD-FUNC VET-NMRO DIG-CALC2 COD-ERRO.

           IF  RETURN-CODE NOT EQUAL ZEROS
               GO TO 999014-ERRO-014
           END-IF.

           IF  DIG-CALC1 NOT EQUAL S500V-DV-CC-TMDR
               GO TO 999015-ERRO-015
           END-IF.

       222-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   222-099-SAIDA.'
                      .
       222-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       222-100-VALIDA-VRC-NR-CT-POUP     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 222-100-VALIDA-VRC-NR-CT-POUP.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           MOVE S500V-NR-CC-TMDR TO W-CT-POUP

           IF  W-VAR-CT-POUP EQUAL ZEROS
               GO TO 999187-ERRO-187
           END-IF
           .
       222100-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   222100-SAI.'
                      .
       222100-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       223-000-VALIDA-AGENCIA-CONTA  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 223-000-VALIDA-AGENCIA-CONTA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  K0821-QT-CT LESS 6
               PERFORM VARYING W-NR-NDX FROM 1 BY 1
                   UNTIL W-NR-NDX GREATER 6
                   IF  (K0821-CD-PRF-DEPE-CT(W-NR-NDX)
                       EQUAL S500V-CD-PRF-DEPE-OPR) AND
                       (K0821-CD-CT(W-NR-NDX)
                       EQUAL S500V-NR-CC-TMDR) AND
                       (K0821-CD-PRD(W-NR-NDX) EQUAL W-CD-PRD-MCI)
                       MOVE 1 TO W-CD-CT-VLDO
                   END-IF
               END-PERFORM
           END-IF.

           IF  NOT W-IN-CT-VLDO

               PERFORM 871-000-CHAMA-OPRS5900

      *
      *** CD-INST-ORGC
      *** 001 Banco do Brasil
      *** 151 Banco Nossa Caixa

               IF  S500V-CD-INST-ORGC = 1
                   IF  W5900-COD-RETORNO EQUAL ZEROS
                       PERFORM VARYING W-NR-NDX-OPR  FROM 1 BY 1
                           UNTIL W-NR-NDX-OPR  EQUAL 20
                           OR W-IN-CT-VLDO
                           IF  W5900-CD-PSS-CTR-OPR-S03(W-NR-NDX-OPR)
                               EQUAL S500V-CD-CLI
                               MOVE 1 TO W-CD-CT-VLDO
                           END-IF
                       END-PERFORM

      * MOCK-POINT DEFINIR-CT-VLDO

                       IF  NOT W-IN-CT-VLDO
                           IF  W5900-CD-TIP-CPF-CGC-S03(1)   EQUAL 2
      *--                     Aceitar contas de Pessoa Jurídica de
      *                       mesmo radical CNPJ do informado na
      *                       ordem. ( DEMANDA N° 2001/11321 )
      *
                               INITIALIZE VRV-CNV-CNPJ W-CD-CGC-ALFA
                               MOVE S500V-CD-CPF-CGC TO W-NR-IDFC-PJ-E
                               PERFORM 223-001-CONVERTE-CNPJ-ALFA
                               MOVE W-CD-CNPJ-ALFA-S TO W-CD-CGC-ALFA
      *
                               DISPLAY 'W-CD-CGC-ALFA: ' W-CD-CGC-ALFA
                               DISPLAY '223-002-CNV-LS-CNPJ-ALFA'
      *
      * Entrada: W5900-NR-CPF-CGC-S03(W-IC)
                               PERFORM 223-002-CNV-LS-CNPJ-ALFA
      * Saida..: W-LS-CNPJ-ALFA-S
      *
                               PERFORM VARYING W-NR-NDX FROM 1 BY 1
                                   UNTIL W-NR-NDX EQUAL 20
                                   OR W-IN-CT-VLDO
      *                             MOVE W5900-NR-CPF-CGC-S03(W-NR-NDX)
      *                                 TO W-CD-CPF-CGC
      *                             IF  W-CD-CPF-CGC(1:8)
      *                                 EQUAL S500V-CD-CPF-CGC(1:8)
      *                           AND W5900-CD-TIP-CPF-CGC-S03(W-NR-NDX)
      *                                 EQUAL 2
      *                                 MOVE 1 TO W-CD-CT-VLDO
      *                             END-IF
      * se for CNPJ (tipo 2)
                                   IF W5900-CD-TIP-CPF-CGC-S03(W-NR-NDX)
                                       EQUAL 2
      * comparacao entre os 8 primeiro caracteres de ambos os CNPJs
                                       DISPLAY
                                         'W-LS-CNPJ-ALFA-S(W-NR-NDX):'
                                         W-LS-CNPJ-ALFA-S(W-NR-NDX)
                                       DISPLAY 'W-CD-CGC-ALFA: '
                                       W-CD-CGC-ALFA
                                       IF W-CD-CGC-ALFA(1:8) EQUAL
                                         W-LS-CNPJ-ALFA-S(W-NR-NDX)(1:8)
                                           MOVE 1 TO W-CD-CT-VLDO
                                       END-IF
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   IF  S500V-CD-INST-ORGC  = 151
                       PERFORM 873-000-CALCULA-DIGITO-AGENCIA
                       PERFORM 874-000-CALCULA-DIGITO-CONTA
                   ELSE
                       MOVE  0     TO  S500V-CD-INST-ORGC
                       GO TO 999126-ERRO-126
                   END-IF
               END-IF
           END-IF.

           IF  NOT W-IN-CT-VLDO
           AND W-AMBIENTE            NOT EQUAL 'DES'
               GO TO 999092-ERRO-092
           END-IF.

       223-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   223-099-SAIDA.'
                      .
       223-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       223-001-CONVERTE-CNPJ-ALFA SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 223-001-CONVERTE-CNPJ-ALFA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      * Entrada: W-NR-IDFC-PJ-E
      * Saida..: W-CD-CNPJ-ALFA-S
      *
           INITIALIZE BCISDFE0-DADOS
      *
           MOVE W-NR-IDFC-PJ-E TO KDFE0-NR-IDFC-PJ-E
      *
           DISPLAY 'KDFE0-NR-IDFC-PJ-E: ' KDFE0-NR-IDFC-PJ-E
      *
           MOVE LENGTH OF BCISDFE0-DADOS TO EIBCALEN
      *
           CALL BCISDFE0 USING DFHEIBLK BCISDFE0-DADOS
      *
           DISPLAY 'KDFE0-CD-RTN: ' KDFE0-CD-RTN
      *
           IF KDFE0-CD-RTN NOT EQUAL ZEROS
               DISPLAY 'KDFE0-TX-MSG-RTN: ' KDFE0-TX-MSG-RTN
               GOBACK
           END-IF
      *
           MOVE KDFE0-CD-CNPJ-SRF-S TO W-CD-CNPJ-ALFA-S
      *
      * Adiciona zeros a esquerda do CNPJ, se não houver 14 digitos do
      * CNPJ
      *
           MOVE FUNCTION REVERSE(W-CD-CNPJ-ALFA-S) TO W-CD-CNPJ-ALFA-S
           MOVE FUNCTION TRIM(W-CD-CNPJ-ALFA-S)    TO W-CD-CNPJ-ALFA-S
           INSPECT W-CD-CNPJ-ALFA-S CONVERTING SPACES TO ZEROS
           MOVE FUNCTION REVERSE(W-CD-CNPJ-ALFA-S) TO W-CD-CNPJ-ALFA-S
           .
      *
       223-001-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 223-001-SAI.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT
           .
      *----------------------------------------------------------------*
       223-002-CNV-LS-CNPJ-ALFA SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 223-002-CNV-LS-CNPJ-ALFA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      * Entrada: W5900-NR-CPF-CGC-S03(W-IC)
      * Saida..: W-CD-CNPJ-ALFA-S
      *
           INITIALIZE TAB-ENTD-SAID-CNPJ
      *
           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 20
               MOVE W5900-NR-CPF-CGC-S03(W-IC)
                   TO W-LS-NR-IDFC-PJ-E(W-IC)
               IF W5900-NR-CPF-CGC-S03(W-IC) EQUAL ZEROS
                   SUBTRACT 1 FROM W-IC GIVING W-QTD-CNPJ
                   EXIT PERFORM
               END-IF
               IF W-IC EQUAL 20
                   MOVE 20 TO W-QTD-CNPJ
               END-IF
           END-PERFORM
      *
           IF EXECUCAO-ONLINE
               PERFORM 223-003-CNV-LS-CNPJ-O
           ELSE
               PERFORM 223-004-CNV-LS-CNPJ-B
           END-IF
           .
      *
       223-002-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 223-002-SAI.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT
           .
      *----------------------------------------------------------------*
       223-003-CNV-LS-CNPJ-O         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 223-003-CNV-LS-CNPJ-O.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * Entrada: W-LS-NR-IDFC-PJ-E
      * Saida..: W-LS-CNPJ-ALFA-S
      *
           INITIALIZE DFES192C-DADOS
      *
           MOVE 1 TO DFES192C-CD-FUNCAO
           MOVE W-QTD-CNPJ TO DFES192C-QT-PSQ-E
      *
           PERFORM VARYING W-IC FROM 1 BY 1
               UNTIL W-IC GREATER THAN W-QTD-CNPJ
               MOVE W-LS-NR-IDFC-PJ-E(W-IC) TO
                   DFES192C-NR-IDFC-PJ-E(W-IC)
           END-PERFORM
      *
           MOVE LENGTH OF DFES192C-DADOS TO EIBCALEN
      *
           CALL DFES192C USING DFHEIBLK DFES192C-DADOS
      *
           DISPLAY 'DFES192C-CD-RETORNO: ' DFES192C-CD-RETORNO
      *
           DISPLAY 'DFES192C-TX-RETORNO: ' DFES192C-TX-RETORNO
      *
           IF DFES192C-CD-RETORNO NOT EQUAL ZEROS
               DISPLAY 'DFES192C-TX-RETORNO: ' DFES192C-TX-RETORNO
               GOBACK
           END-IF
      *
      * Adiciona zeros a esquerda do CNPJ, se não houver 14 digitos do
      * CNPJ
      *
           DISPLAY 'W-QTD-CNPJ: ' W-QTD-CNPJ
           DISPLAY 'DFES192C-CD-CNPJ-SRF-S(1): '
               DFES192C-CD-CNPJ-SRF-S(1)
           DISPLAY 'DFES192C-CD-CNPJ-SRF-S(3):'
               DFES192C-CD-CNPJ-SRF-S(3)

           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > W-QTD-CNPJ
               MOVE DFES192C-CD-CNPJ-SRF-S(W-IC) TO W-CD-CNPJ-ALFA-S
               MOVE FUNCTION REVERSE(W-CD-CNPJ-ALFA-S)
                   TO W-CD-CNPJ-ALFA-S
               MOVE FUNCTION TRIM(W-CD-CNPJ-ALFA-S) TO
                   W-CD-CNPJ-ALFA-S
               INSPECT W-CD-CNPJ-ALFA-S CONVERTING SPACE TO ZERO
               MOVE FUNCTION REVERSE(W-CD-CNPJ-ALFA-S)
                   TO W-CD-CNPJ-ALFA-S
               MOVE W-CD-CNPJ-ALFA-S TO W-LS-CNPJ-ALFA-S(W-IC)
               DISPLAY 'W-LS-CNPJ-ALFA-S(W-IC): ' W-LS-CNPJ-ALFA-S(W-IC)
           END-PERFORM
           .
      *
       223-003-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 223-003-SAI.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT
           .
      *----------------------------------------------------------------*
       223-004-CNV-LS-CNPJ-B      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 223-004-CNV-LS-CNPJ-B.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * Entrada: W-LS-NR-IDFC-PJ-E
      * Saída..: W-LS-CNPJ-ALFA-S
      *
           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > W-QTD-CNPJ
               MOVE W-LS-NR-IDFC-PJ-E(W-IC) TO W-NR-IDFC-PJ-E
               PERFORM 223-001-CONVERTE-CNPJ-ALFA
               MOVE W-CD-CNPJ-ALFA-S TO W-LS-CNPJ-ALFA-S(W-IC)
           END-PERFORM
           .
      *
       223-004-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 223-004-SAI.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT
           .
      *
      *----------------------------------------------------------------*
       224-000-VERIFICA-CT-MOEE      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 224-000-VERIFICA-CT-MOEE.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE K502V-PRM-ENT K502V-DADO-RTN
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 2                     TO K502V-CD-FNC.
           MOVE 5                     TO K502V-CD-TIP-ITCE.
           MOVE S500V-CD-PRF-DEPE-OPR TO K502V-CD-PRF-DEPE.
           MOVE S500V-NR-CC-TMDR      TO K502V-NR-CT-CLI.
           MOVE S500V-CD-CLI          TO K502V-CD-CLI.
           MOVE S500V-CD-MOE          TO K502V-CD-MOE.
           MOVE 2                     TO K502V-CD-TIP-MVTC.
           MOVE S500V-NM-TMDR         TO K502V-NM-CLI-RSP.

           MOVE LENGTH OF L-DMES502V TO EIBCALEN
           CALL DMES502V USING DFHEIBLK L-DMES502V.

           IF  K502V-CD-RTN NOT EQUAL ZEROS
               GO TO 999009-ERRO-009
           END-IF.

       224-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   224-099-SAIDA.'
                      .
       224-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       225-000-VALIDA-AG-CTA-VNCL    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 225-000-VALIDA-AG-CTA-VNCL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-DEBSB972
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0

           MOVE S500V-CD-PRF-DEPE-OPR TO PARM-AGEN
           MOVE S500V-NR-CC-TMDR      TO PARM-CONTA
                                         W-NR-CC-TMDR-A

      * -- Seta execucao BATCH/ONLINE para o DEBSB972
           IF  EXECUCAO-ONLINE
               MOVE 'O'               TO PARM-AMBIENTE
           ELSE
               MOVE 'B'               TO PARM-AMBIENTE
           END-IF

           MOVE LENGTH OF L-DEBSB972 TO EIBCALEN
           CALL DEBSB972 USING DFHEIBLK L-DEBSB972

           IF  PARM-COD-RETORN NOT EQUAL ZEROS
               GO TO 999089-ERRO-089
           END-IF

      *--  MCI do titular da conta no sistema DEB deve ser igual ao
      *--  CD-MCI da OPE.
           MOVE PARM-COD-BDC  TO W-CD-MCI-DEB

           IF  W-CD-MCI-DEB  NOT EQUAL S500V-CD-CLI
               GO TO 999163-ERRO-163
           END-IF

      * -- Move nr da conta para a esquerda
           PERFORM VARYING W-IX FROM 1 BY 1
               UNTIL W-IX > 11 OR W-DGTO-NR-CC-TMDR-A (W-IX) NOT = 0
           END-PERFORM
           COMPUTE W-TAM-VRV-ALFA = LENGTH OF W-NR-CC-TMDR-A -
               ( W-IX - 1 )
           ON SIZE ERROR
              PERFORM 999193-ERRO-193
           END-COMPUTE
           MOVE W-NR-CC-TMDR-A (W-IX:W-TAM-VRV-ALFA) TO
               W-NR-CC-TMDR-A

      * -- Numero da conta informada deve ser 31027XXXXXX
           IF  W-NR-CC-TMDR-PRCL-A5 NOT = '31027'
               GO TO 999164-ERRO-164
           END-IF

      *--  Conta informada deve pertencer à razão  310273503
           MOVE PARM-RAZAO    TO W-CD-ENQ-CMT

      *    IF  W-CD-ENQ-PRCL-N7 NOT EQUAL 3102735
           IF  W-CD-ENQ-CMT     NOT EQUAL 310273503
               GO TO 999094-ERRO-094
           END-IF
           .
       225-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   225-099-SAIDA.'
                      .
       225-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       226-000-VALIDA-AG-CONTA-POUP  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 226-000-VALIDA-AG-CONTA-POUP.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Verifica se a conta pertence ao tomador da ordem
           MOVE 003 TO W-CD-PRD-MCI.
           PERFORM 223-000-VALIDA-AGENCIA-CONTA.

      *--  Valida conta poupança BB

           IF  S500V-CD-INST-ORGC = 1
               INITIALIZE CPRSRMSG
                   REPLACING ALPHANUMERIC BY ' '
                   NUMERIC BY  0

               MOVE S500V-CD-PRF-DEPE-OPR  TO AGENCIA-CPR
               MOVE S500V-NR-CC-TMDR       TO CONTA-CPR

               MOVE LENGTH OF L-CPRSB001 TO EIBCALEN
               CALL CPRSB001 USING DFHEIBLK L-CPRSB001

               IF  RET-IND-CONTA NOT EQUAL 'N'
                   OR  RET-IND-BLOQ  NOT EQUAL '0'
                   GO TO 999091-ERRO-091
               END-IF

      *--     AG/CONTA inválida
               IF  RET-AGENCIA-CPR = 0000
                   GO TO 999008-ERRO-008
               END-IF

      *--     Parâmetros não localizados/inválidos
               IF  CPR-CONTROLE-ERRO NOT EQUAL ZEROS
                   GO TO 999093-ERRO-093
               END-IF
           ELSE

      *--     Valida conta poupança BB correspondente BNC

               IF  S500V-CD-INST-ORGC = 151
                   INITIALIZE EIFSB014-DADOS
                       REPLACING ALPHANUMERIC BY ' '
                       NUMERIC BY  0

                   MOVE 1                      TO EIFSB014-FUNCAO
                   MOVE S500V-CD-INST-ORGC     TO EIFSB014-INSTITUICAO
                   MOVE W-CD-PRF-DEPE-OPR-BNC  TO EIFSB014-AGENCIA
                   MOVE W-NR-CC-TMDR-BNC       TO EIFSB014-CONTA
                   MOVE 'O'                    TO EIFSB014-AMBIENTE

                   MOVE LENGTH OF EIFSB014-DADOS TO EIBCALEN
                   CALL EIFSB014 USING DFHEIBLK EIFSB014-DADOS

                   IF  EIFSB014-RETORNO NOT EQUAL 0
                       MOVE EIFSB014-RETORNO TO W-CD-RTN-EIFSB014
                       GO TO 999134-ERRO-134
                   END-IF

                   IF  EIFSB014-AGENCIA-RETORNO NOT EQUAL
                       S500V-CD-PRF-DEPE-OPR
                       AND EIFSB014-CONTA-RETORNO   NOT EQUAL
                       S500V-NR-CC-TMDR
                       MOVE EIFSB014-AGENCIA-RETORNO TO
                           W-EIFSB014-AGENCIA-RET
                       MOVE EIFSB014-CONTA-RETORNO   TO
                           W-EIFSB014-CONTA-RET
                       GO TO 999137-ERRO-137
                   END-IF
               ELSE
                   GO TO 999135-ERRO-135
               END-IF
           END-IF.

      *--  Verifica se a conta pertence ao tomador da ordem
           MOVE 003 TO W-CD-PRD-MCI.
           PERFORM 223-000-VALIDA-AGENCIA-CONTA.

       226-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   226-099-SAIDA.'
                      .
       226-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       227-000-VALIDA-FMA-INTM-DG    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 227-000-VALIDA-FMA-INTM-DG.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *--  D.E. 2007/78054 Orgão DG permite c.c. e poupança
      *
           INITIALIZE L-OPES0820
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.
      *
           MOVE 'emitente#'           TO W-TX-DEPE.
           MOVE ' '                   TO K0820-CD-TIP-VLDC.
           MOVE S500V-CD-PRF-DEPE-EMT TO K0820-CD-PRF-DEPE-PSQ.
           MOVE S500V-CD-PRF-DEPE-USU TO K0820-CD-PRF-DEPE-USU.

           PERFORM 850-000-TRATA-DEPE.
      *
           IF  K0820-CD-TIP-DEPE = 'D' AND
               S500V-CD-TIP-ORD-PGTO = 1 AND
               NOT(S500V-CD-FMA-INTM = 1 OR = 6)
               GO TO 999116-ERRO-116
           END-IF.
      *
       227-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   227-099-SAIDA.'
                      .
       227-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       228-000-VALIDA-PRC-OB         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 228-000-VALIDA-PRC-OB.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *--  D.E. 2012/116945 Ação 98387
      *--  Proposta BEC obrigatoria para ONLINE
      *--  e BATCH quando informada remessa
           IF  S500V-NR-BLT-BEC NOT NUMERIC
               GO TO 999167-ERRO-167
           ELSE
               IF  S500V-NR-BLT-BEC EQUAL ZERO
                   GO TO 999168-ERRO-168
               END-IF
           END-IF

      *--  Ano de emissao e numero da OB obrigatorio
      *     IF  S500V-AA-EMS-OB NOT NUMERIC
      *         GO TO 999169-ERRO-169
      *     ELSE
      *        IF  S500V-AA-EMS-OB EQUAL ZERO
      *            GO TO 999170-ERRO-170
      *        END-IF
      *    END-IF
      *    IF  S500V-NR-OB     NOT NUMERIC
      *        GO TO 999171-ERRO-171
      *    ELSE
      *        IF  S500V-NR-OB EQUAL ZERO
      *            GO TO 999172-ERRO-172
      *        END-IF
      *    END-IF

      *    MOVE ZEROS            TO W-NR-OBK
           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
               OR ( S500V-AA-EMS-OBK      (W-IC) EQUAL ZEROS
                AND S500V-NR-OBK          (W-IC) EQUAL ZEROS
                AND S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL ZEROS
                AND W-IC > 1 )
      *        Movimenta valores para tabela interna de OBK's
               MOVE S500V-AA-EMS-OBK      (W-IC) TO W-AA-EMS-OBK (W-IC)
               MOVE S500V-NR-OBK          (W-IC) TO W-NR-OBK (W-IC)
               MOVE S500V-CD-TIP-VCLC-OBK (W-IC)
                                             TO W-CD-TIP-VCLC-OBK (W-IC)
      *
               IF  S500V-AA-EMS-OBK      (W-IC) EQUAL ZEROS
                   PERFORM 999170-ERRO-170
               END-IF
               IF  S500V-NR-OBK          (W-IC) EQUAL ZEROS
                   PERFORM 999172-ERRO-172
               END-IF
               IF  S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL ZEROS
                   PERFORM 999216-ERRO-216
               END-IF
      *        IF  S500V-NR-OBK          (W-IC) EQUAL W-NR-OBK
      *            PERFORM 999217-ERRO-217
      *        ELSE
      *            MOVE S500V-NR-OBK     (W-IC) TO W-NR-OBK
      *        END-IF
           END-PERFORM

      *--  Verifica se OBK da ordem OPE está preenchida
           MOVE 'N'              TO W-IN-NR-OBK-INFD
           IF S500V-IN-OBK-TARF EQUAL 'S'
              PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
               OR ( S500V-AA-EMS-OBK      (W-IC) EQUAL ZEROS
                AND S500V-NR-OBK          (W-IC) EQUAL ZEROS
                AND S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL ZEROS)
               OR ( W-IN-NR-OBK-INFD EQUAL 'S' )
                 IF   S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL 1
                  AND S500V-AA-EMS-OBK (W-IC)  NOT EQUAL ZEROS
                  AND S500V-NR-OBK     (W-IC)  NOT EQUAL ZEROS
                     MOVE 'S'               TO W-IN-NR-OBK-INFD
                 END-IF
              END-PERFORM
           END-IF

           IF   S500V-IN-OBK-TARF EQUAL 'S'
            AND W-IN-NR-OBK-INFD   EQUAL 'N'
               PERFORM 999226-ERRO-226
           END-IF

      *--  Verifica se OBK de tarifa está preenchida
           MOVE 'N'              TO W-IN-NR-OBK-INFD
           IF S500V-IN-OBK-TARF EQUAL 'S'
              PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
               OR ( S500V-AA-EMS-OBK      (W-IC) EQUAL ZEROS
                AND S500V-NR-OBK          (W-IC) EQUAL ZEROS
                AND S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL ZEROS)
               OR ( W-IN-NR-OBK-INFD EQUAL 'S' )
                 IF   S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL 2
                  AND S500V-AA-EMS-OBK (W-IC)  NOT EQUAL ZEROS
                  AND S500V-NR-OBK     (W-IC)  NOT EQUAL ZEROS
                     MOVE 'S'               TO W-IN-NR-OBK-INFD
                 END-IF
              END-PERFORM
           END-IF

           IF   S500V-IN-OBK-TARF EQUAL 'S'
            AND W-IN-NR-OBK-INFD   EQUAL 'N'
               PERFORM 999225-ERRO-225
           END-IF

      *--  Compara as OBK's para verificar se existe duplicidade
           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
               OR (S500V-AA-EMS-OBK (W-IC) EQUAL ZEROS)
               PERFORM VARYING W-IC3 FROM 1 BY 1 UNTIL W-IC3 > 5
                   OR (W-AA-EMS-OBK (W-IC3) EQUAL ZEROS)
                   IF W-IC NOT EQUAL W-IC3
                      IF S500V-AA-EMS-OBK (W-IC) EQUAL
                                                    W-AA-EMS-OBK (W-IC3)
                         AND S500V-NR-OBK (W-IC) EQUAL W-NR-OBK (W-IC3)
                          PERFORM 999217-ERRO-217
                      END-IF
                      IF S500V-CD-TIP-VCLC-OBK (W-IC) EQUAL
                                               W-CD-TIP-VCLC-OBK (W-IC3)
                         PERFORM 999227-ERRO-227
                      END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

      *--  Verifica se OB eh bloqueavel
           PERFORM VARYING W-IC FROM 1 BY 1 UNTIL W-IC > 5
                OR S500V-AA-EMS-OBK (W-IC)  EQUAL ZEROS
               PERFORM 228-100-VALIDA-OB
           END-PERFORM
           .
       228-100-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   228-100-SAI.'
                      .
       228-100-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       228-100-VALIDA-OB             SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 228-100-VALIDA-OB.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE OPES042G-DADOS

           MOVE 06                           TO  S042G-CD-FUC
           MOVE S500V-AA-EMS-OBK (W-IC)      TO  S042G-AA-EMS-OB
           MOVE S500V-NR-OBK (W-IC)          TO  S042G-NR-OB
           MOVE S500V-CD-TIP-VCLC-OBK (W-IC) TO  S042G-CD-TIP-VCLC-OB
           MOVE S500V-CD-USU                 TO  S042G-CD-CHV-USU
           MOVE S500V-CD-CPF-CGC             TO  S042G-CPF-CNPJ-EMT
           MOVE S500V-NM-BNFC                TO  S042G-NM-BNFC
           MOVE S500V-CD-TIP-CLI             TO  S042G-CD-TIP-CLI

           IF  EXECUCAO-ONLINE
               EXEC CICS
                   LINK PROGRAM ( OPES042G )
                   COMMAREA     ( OPES042G-DADOS )
                   LENGTH       ( LENGTH OF OPES042G-DADOS )
               END-EXEC
           ELSE
               MOVE LENGTH OF OPES042G-DADOS TO EIBCALEN
               CALL OPES042G USING DFHEIBLK OPES042G-DADOS
           END-IF

           IF  S042G-CD-RTN-PGM NOT ZERO
      *        MOVE S042G-CD-RTN-PGM  TO  W-CD-RTN
               MOVE S042G-CD-RTN-PGM  TO  S500V-CD-RTN
               MOVE 53        TO S500V-NR-CMP-ERRO
               STRING 'S500V ' S042G-MSG-ERRO-PGM
                   DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
               GOBACK
      *        PERFORM 999173-ERRO-173
           END-IF
           .
       228-100-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   228-100-SAI.'
                      .
       228-100-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       229-000-VALIDA-RAZAO-DEB      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 229-000-VALIDA-RAZAO-DEB.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      * Realiza chamada ao BCIS4015 que por sua vez aciona o DEB para
      * verificar se para o razao da conta e permitido o envio de ordem
      *
           INITIALIZE L-BCIS4015
      *
           MOVE 1                     TO BCIK4015-CD-FUNC-E
           MOVE 'OPE'                 TO BCIK4015-SG-SIS-E
           MOVE 'OPES500V'            TO BCIK4015-NM-PGM-CHMR-E
           MOVE S500V-CD-USU          TO BCIK4015-CD-USU-E
           MOVE S500V-CD-CLI          TO BCIK4015-CD-CLI-E
           MOVE S500V-CD-PRF-DEPE-OPR TO BCIK4015-CD-PRF-DEPE-CLI-E
           MOVE S500V-NR-CC-TMDR      TO BCIK4015-NR-CT-CLI-E
      *
           MOVE LENGTH OF L-BCIS4015 TO EIBCALEN
           CALL BCIS4015 USING DFHEIBLK L-BCIS4015
      *
           IF  BCIK4015-CD-RTN NOT EQUAL ZEROS
               GO TO 999207-ERRO-207
           END-IF
           .
      *
       229-000-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   229-000-SAI.'
                      .
       229-000-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *----------------------------------------------------------------*
       230-000-VALIDA-DATAS          SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 230-000-VALIDA-DATAS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           UNSTRING S500V-DT-VLZC DELIMITED BY '.' INTO W-DD OF W-DT-AMD
               W-MM OF W-DT-AMD
               W-AA OF W-DT-AMD
           MOVE W-DT-AMD               TO  W-DT-VLZC.

           UNSTRING S500V-DT-MVT  DELIMITED BY '.' INTO W-DD OF W-DT-AMD
               W-MM OF W-DT-AMD
               W-AA OF W-DT-AMD
           MOVE W-DT-AMD               TO  W-DT-MVT.

      *--  Valida data da emissão
           UNSTRING S500V-DT-EMS DELIMITED BY '.' INTO W-DD OF W-DT-DMA
               W-MM OF W-DT-DMA
               W-AA OF W-DT-DMA.

           IF  W-DD OF W-DT-DMA EQUAL 31 AND W-MM OF W-DT-DMA EQUAL 12
               GO TO 999019-ERRO-019
           END-IF.

           MOVE W-DT-DMA           TO F01-ARG01
               DT-AUX-EMS-N8.
           MOVE 'de emissão#'      TO W-TX-TIP-DATA.
           PERFORM 830-000-VALIDA-DATA.
           IF  F01-ARG01 NOT EQUAL W-DT-DMA
               GO TO 999020-ERRO-020
           END-IF.

           MOVE CORRESPONDING W-DT-DMA TO W-DT-AMD.
           MOVE W-DT-AMD               TO W-DT-EMS.

           IF  S500V-NR-BLT-BEC EQUAL ZEROS
               IF  W-DT-EMS < W-DT-MVT
                   GO TO 999082-ERRO-082
               END-IF
           END-IF.

           IF  W-DT-EMS < W-DT-VLZC
               GO TO 999021-ERRO-021
           END-IF.

           IF  W-DT-EMS > W-DT-MVT
               AND S500V-CD-TIP-ORD-PGTO NOT EQUAL 4
               GO TO 999079-ERRO-079
           END-IF.

      *--  Valida data da liquidação.
           UNSTRING S500V-DT-LQDC DELIMITED BY '.' INTO W-DD OF W-DT-DMA
               W-MM OF W-DT-DMA
               W-AA OF W-DT-DMA.
           MOVE W-DT-DMA              TO F01-ARG01
               DT-AUX-LQDC-N8.
           MOVE 'da liquidação#'      TO W-TX-TIP-DATA.
           PERFORM 830-000-VALIDA-DATA.
           IF  F01-ARG01 NOT EQUAL W-DT-DMA
               GO TO 999022-ERRO-022
           END-IF.

           MOVE CORRESPONDING W-DT-DMA TO W-DT-AMD.
           MOVE W-DT-AMD               TO W-DT-LQDC.

           IF  W-DT-LQDC < W-DT-EMS
               GO TO 999023-ERRO-023
           END-IF.

      *--  DE 90120 Ação 52379
      *    Retira a validação abaixo, pois essa validação nao considera
      *    feriado na praça da moeda. As validaç es são feitas pelo BEC
      *

      *--  Validação das datas de emissão e liquidação. Se a diferença
      *    entre elas for maior do que 2 dias uteis, a proposta do BEC
      *    é obrigatoriamente necessária.

      *--  O ARG03 com valor '777  ' fará com que a maior data (data de
      *    liquidação) não entre no cálculo para saber se a operação é
      *    imediata (mesmo dia) , pronta (maximo de 2 dias) ou futura
      *    (mais do que 2 dias), ou seja, o cálculo será realizado da
      *    data da emissão até D-1 da data da liquidação.
      *
      *    MOVE  DT-AUX-EMS-N8         TO  ARG01.
      *    MOVE  DT-AUX-LQDC-N8        TO  ARG02.
      *    MOVE  '777  '               TO  ARG03.
      *
      *    CALL SBDATA USING FUNCAO ARG01 ARG02 ARG03.
      *
      *    IF  F01-ARG01 EQUAL 99999999
      *        GO TO 999076-ERRO-076
      *    END-IF.
      *
      *    IF  F01-ARG01 EQUAL 88888888
      *        GO TO 999077-ERRO-077
      *    END-IF.
      *
      *    IF  S500V-NR-BLT-BEC EQUAL ZEROS
      *        IF  DIAS-UTEIS GREATER 2
      *            GO TO 999078-ERRO-078
      *        END-IF
      *    END-IF.

           IF  S500V-CD-TIP-ORD-PGTO = 3
               AND W-DT-LQDC NOT = W-DT-EMS
               GO TO 999080-ERRO-080
           END-IF.

      *--  Valida data da trava.
           UNSTRING S500V-DT-BLQ DELIMITED BY '.' INTO W-DD OF W-DT-DMA
               W-MM OF W-DT-DMA
               W-AA OF W-DT-DMA.
           MOVE W-DT-DMA              TO F01-ARG01.
           MOVE 'da trava#'           TO W-TX-TIP-DATA.
           PERFORM 830-000-VALIDA-DATA.
           IF  F01-ARG01 NOT EQUAL W-DT-DMA
               GO TO 999024-ERRO-024
           END-IF.

           MOVE CORRESPONDING W-DT-DMA TO W-DT-AMD.
           MOVE W-DT-AMD               TO W-DT-BLQ.

           IF  W-DT-BLQ < W-DT-LQDC
               GO TO 999025-ERRO-025
           END-IF.

           IF  W-DT-BLQ < W-DT-MVT
               GO TO 999026-ERRO-026
           END-IF.

      *--  DE 90120 Ação 52379
      *    Para intermediação 4, verifica se as datas da trava e
      *    liquidação sao iguais. Intermediação não pode ter BEC
           IF  S500V-CD-FMA-INTM EQUAL 4
               IF  W-DT-BLQ NOT EQUAL W-DT-LQDC
                   GO TO 999119-ERRO-119
               END-IF
               IF  S500V-NR-BLT-BEC NOT EQUAL ZEROS
                   GO TO 999120-ERRO-120
               END-IF
           END-IF.

      *--  Valida data do débito.
           UNSTRING S500V-DT-DEB DELIMITED BY '.' INTO W-DD OF W-DT-DMA
               W-MM OF W-DT-DMA
               W-AA OF W-DT-DMA.
           MOVE W-DT-DMA              TO F01-ARG01.
           MOVE 'do débito#'          TO W-TX-TIP-DATA.
           PERFORM 830-000-VALIDA-DATA.
           IF  F01-ARG01 NOT EQUAL W-DT-DMA
               GO TO 999027-ERRO-027
           END-IF.

           MOVE CORRESPONDING W-DT-DMA TO W-DT-AMD.
           MOVE W-DT-AMD               TO W-DT-DEB.

           IF  W-DT-DEB < W-DT-EMS
               GO TO 999028-ERRO-028
           END-IF.

           IF  W-DT-DEB > W-DT-LQDC
               GO TO 999029-ERRO-029
           END-IF.

       230-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   230-099-SAIDA.'
                      .
       230-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       240-000-VALIDA-CD-MOEDA       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 240-000-VALIDA-CD-MOEDA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Ordens na moeda REAL não devem possuir taxa cambial
           IF  S500V-CD-MOE EQUAL 790
               IF  (S500V-VL-TAXA-CMB NOT EQUAL ZEROS) AND
                   (S500V-VL-TAXA-CMB NOT EQUAL 1)
                   GO TO 999030-ERRO-030
               END-IF
           END-IF.

      *--  Busca o nome e verifica se a moeda está cadastrada.
           MOVE 1            TO K002R-CD-TIP-PSQ.
           MOVE S500V-CD-MOE TO K002R-CD-MOE.

           MOVE LENGTH OF L-BCIS002R TO EIBCALEN
           CALL BCIS002R USING DFHEIBLK L-BCIS002R.

           IF  K002R-CD-RTN EQUAL ZEROS
               IF  K002R-IN-ATVO EQUAL 'N'
                   PERFORM 999033-ERRO-033
               ELSE
                   MOVE K002R-NM-SNGL      TO S500V-NM-MOE
                   MOVE K002R-VL-VRC       TO W-VL-VRC
                   MOVE K002R-CD-INTT      TO W-CD-IFC-CAIXA
               END-IF
           ELSE
               MOVE K002R-CD-RTN TO S500V-CD-RTN
               MOVE K002R-CD-SQL TO S500V-CD-SQL
               IF  K002R-CD-SQL EQUAL 100
                   PERFORM 999031-ERRO-031
               ELSE
                   PERFORM 999032-ERRO-032
               END-IF
           END-IF.

      *--  Para forma de intermediação (4-conta em moeda estrangeira),
      *    o código da moeda informado tem que ser igual ao da conta.
           IF  (S500V-CD-FMA-INTM EQUAL 4) AND
               (K502V-CD-MOE  NOT EQUAL S500V-CD-MOE)
               GO TO 999034-ERRO-034
           END-IF.


       240-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   240-099-SAIDA.'
                      .
       240-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       250-000-VALIDA-FMA-ETGA-MOE   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 250-000-VALIDA-FMA-ETGA-MOE.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

      *--  Busca o texto da forma de entrega e verifica se está ativa.
      *
           INITIALIZE L-BCIS017R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.
      *
           MOVE S500V-CD-FMA-ETGA-MOE TO K017R-COD.
      *
           MOVE LENGTH OF L-BCIS017R TO EIBCALEN
           CALL BCIS017R USING DFHEIBLK L-BCIS017R.
      *
           IF  K017R-CD-RTN NOT EQUAL ZEROS
               IF  K017R-CD-SQL EQUAL 100
                   PERFORM 999035-ERRO-035
               ELSE
                   PERFORM 999036-ERRO-036
               END-IF
           END-IF.
      *
           MOVE K017R-TXT TO S500V-TX-FMA-ETGA-MOE.

           IF  K017R-IND-ATVO  NOT EQUAL 'S'
               GO TO 999037-ERRO-037
           END-IF.
      *
      * Projeto 5035 - alterada Forma de entrega de 40 para 20.
      *
           IF  S500V-CD-TIP-ORD-PGTO = 2
               IF  S500V-CD-FMA-ETGA-MOE NOT EQUAL 20
                   PERFORM 999095-ERRO-095
               END-IF
           END-IF.

       250-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   250-099-SAIDA.'
                      .
       250-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       251-000-VALIDA-DADOS-IR       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 251-000-VALIDA-DADOS-IR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           IF  S500V-IN-RLHT-IR NOT EQUAL ' ' AND 'N' AND 'S'
               PERFORM 999124-ERRO-124
           END-IF.

           IF  S500V-IN-RLHT-IR EQUAL 'S'
      *--  Valida data de recolhimento do IR
               UNSTRING S500V-DT-RLHT-IR DELIMITED BY '.' INTO
                   W-DD OF W-DT-DMA
                   W-MM OF W-DT-DMA
                   W-AA OF W-DT-DMA

               MOVE W-DT-DMA           TO F01-ARG01
                   DT-AUX-EMS-N8
               MOVE 'de rec. IR#'      TO W-TX-TIP-DATA
               PERFORM 830-000-VALIDA-DATA
               IF  F01-ARG01 NOT EQUAL W-DT-DMA
                   GO TO 999020-ERRO-020
               END-IF
           END-IF.

       251-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   251-099-SAIDA.'
                      .
       251-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       310-000-VALIDA-SWFT-02        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 310-000-VALIDA-SWFT-02.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

      *--  Valida código do país.
           PERFORM 700-000-VALIDA-PAIS-BNFC.

      *--  Valida banqueiro do favorecido.
           PERFORM 710-000-VALIDA-BANQ-BNFC.

      *--  Valida país com restricao na moeda.
           PERFORM 310-040-VALIDA-PAIS-COM-RST.

           IF  S500V-CD-TIP-CLI EQUAL 1
           AND S500V-CD-MOE     EQUAL 978
               PERFORM 212-000-VERIFICA-CUBANO
           END-IF
      *
      *--  Nao preenche informacoes de banqueiro destinatario quando
      *--  for interface SISBB, moeda dolar ou euro e pais destinatario
      *--  nao e Cuba ou tomador nao e cubano
           IF  S500V-CD-TIP-ITCE-REG  EQUAL 3
            AND (S500V-CD-MOE          EQUAL 220
            OR  S500V-CD-MOE          EQUAL 978)
            AND (S500V-CD-PAIS-BNFC   NOT EQUAL 199
             AND W-IN-PAIS-NSC-CUBA   NOT EQUAL 'S')
               CONTINUE
           ELSE
               IF  S500V-CD-INST-BNFC NOT EQUAL ZEROS
                   OR  S500V-CD-MOE EQUAL 978
                   PERFORM 860-000-CHAMA-BDDSH143
               END-IF
           END-IF.

           IF  S500V-CD-CNL              EQUAL 'I' AND
               S500V-CD-TIP-ITCE-REG NOT EQUAL 55
               PERFORM 310-020-TRATA-NTZ-INTERNET
           END-IF.

      *--  Valida indicador de SWIFT automático
           IF  S500V-IN-SWFT-AUTC = 'S' AND S500V-CD-FMA-ETGA-MOE = 30
               GO TO 999048-ERRO-048
           END-IF.

      *    Verifica regra 198 - Remetente beneficiarios iguais
           MOVE 198      TO W-RGR-ENT.
      *
           PERFORM 919-000-CHAMA-BCIS051N.
      *
           IF  BCIS051N-RTN-PSQ EQUAL 1
      *        No SISBB e INTERNET o campo NM-BNFC e mascarado com 35
      *        caracteres
      *
               IF  S500V-CD-CNL EQUAL 'I' OR
                   S500V-CD-CNL EQUAL ' '
                   IF  S500V-NM-BNFC(1:35) NOT EQUAL S500V-NM-TMDR(1:35)
                       MOVE SPACES             TO W-NM-TMDR
                       MOVE S500V-NM-TMDR      TO W-NM-TMDR
                       INSPECT W-NM-TMDR REPLACING ALL
                        'º' BY 'R' '°' BY 'R'
                        'ª' BY 'A' 'ã' BY 'A' 'Ã' BY 'A'
                        'ä' BY 'A' 'á' BY 'A' 'à' BY 'A' 'â' BY 'A'
                        'Ä' BY 'A' 'Á' BY 'A' 'À' BY 'A' 'Â' BY 'A'
                        'ë' BY 'E' 'é' BY 'E' 'è' BY 'E' 'ê' BY 'E'
                        'Ë' BY 'E' 'É' BY 'E' 'È' BY 'E' 'Ê' BY 'E'
                        'ï' BY 'I' 'í' BY 'I' 'ì' BY 'I' 'î' BY 'I'
                        'Ï' BY 'I' 'Í' BY 'I' 'Ì' BY 'I' 'Î' BY 'I'
                        'ö' BY 'O' 'ó' BY 'O' 'ò' BY 'O' 'õ' BY 'O'
                        'Ö' BY 'O' 'Ó' BY 'O' 'Ò' BY 'O' 'Õ' BY 'O'
                        'Ö' BY 'O' 'ö' BY 'O' 'ô' BY 'O' 'Ô' BY 'O'
                        'ü' BY 'U' 'ú' BY 'U' 'ù' BY 'U' 'û' BY 'U'
                        'Ü' BY 'U' 'Ú' BY 'U' 'Ù' BY 'U' 'Û' BY 'U'
                        'ç' BY 'C' 'Ç' BY 'C' 'Ý' BY 'Y' '{' BY ' '
                        'Ñ' BY 'N' 'ñ' BY 'N' '&' BY 'E'
                        '"' BY ' ' '!' BY ' ' '@' BY ' ' '#' BY ' '
                        '$' BY ' ' '¨' BY ' ' '*' BY ' ' '(' BY ' '
                        ')' BY ' ' '_' BY ' ' '%' BY ' ' "'" BY ' '
                        '=' BY ' ' '<' BY ' ' '>' BY ' ' ';' BY ' '
                        '}' BY ' '

                        IF  S500V-NM-BNFC(1:35) NOT EQUAL
                            W-NM-TMDR(1:35)
                            PERFORM 999165-ERRO-165
                        END-IF
                   END-IF
               ELSE
                   IF  S500V-NM-BNFC NOT EQUAL S500V-NM-TMDR
                       PERFORM 999165-ERRO-165
                   END-IF
               END-IF
               IF  S500V-CD-CT-BNFC EQUAL SPACES
                   PERFORM 999166-ERRO-166
               END-IF
           END-IF.
      *

       310-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   310-099-SAIDA.'
                      .
       310-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       310-020-TRATA-NTZ-INTERNET    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 310-020-TRATA-NTZ-INTERNET.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------
      ** Verifica a obrigatoriedade da informação nos detalhes do paga-
      ** mento.
      **
           INITIALIZE L-OPES035R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.
      *
           MOVE 01                     TO S035R-CD-FUC.
           MOVE 01                     TO S035R-CD-TIP-ORD-PGTO-ENT.
           MOVE S500V-CD-TIP-ITCE-REG  TO S035R-CD-TIP-ITCE-ENT.
           MOVE S500V-CD-GR-OPR-CMB    TO S035R-CD-GR-OPR-CMB-ENT.
           MOVE S500V-CD-CPRD-VNDR-MOE TO S035R-CD-CPRD-VNDR-MOE-ENT.
           MOVE S500V-CD-PGDR-RCBD-BC  TO S035R-CD-PGDR-RCBD-BC-ENT.
           MOVE S500V-CD-TIP-AVL-GOV   TO S035R-CD-TIP-AVL-GOV-ENT.
           MOVE S500V-CD-NTZ-FATO-OPRL TO S035R-CD-NTZ-FATO-OPRL-ENT.
      *
           MOVE LENGTH OF L-OPES035R TO EIBCALEN
           CALL OPES035R USING DFHEIBLK L-OPES035R.
      *
           IF  S035R-CD-RTN NOT EQUAL ZEROS
               PERFORM 999131-ERRO-131
           END-IF.
      *
           IF  S035R-IN-DET-PGTO-SAI EQUAL 'S' AND
               S500V-TX-DET-PGTO     EQUAL SPACES
               PERFORM 999132-ERRO-132
           ELSE
               IF  S500V-TX-DET-PGTO NOT EQUAL SPACES
      **          Trata termo invalido.
                   PERFORM 310-030-TRATA-TRM-INVD
               END-IF
           END-IF.
      *
      **   Verifica preenchimento do numero de registro BC
           IF  S035R-IN-OBGD-REG-BC-SAI EQUAL 'S' AND
               S500V-CD-CTFD-BC         EQUAL SPACES
               PERFORM 999144-ERRO-144
           END-IF.
      *
       310-029-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   310-029-SAIDA.'
                      .
       310-029-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       310-030-TRATA-TRM-INVD        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 310-030-TRATA-TRM-INVD.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------
      **
           MOVE S500V-TX-DET-PGTO TO W-TX-DET-PGTO-AUX.
           MOVE ZEROS             TO W-IC-AJSD.

           PERFORM VARYING W-IC FROM 01 BY 01
               UNTIL W-IC GREATER 100

               IF  W-TX-DET-PGTO(W-IC) NOT EQUAL SPACES
                   ADD 1 TO W-IC-AJSD
                   MOVE W-TX-DET-PGTO(W-IC) TO W-TX-TRM(W-IC-AJSD)
               ELSE
                   IF  W-IC-AJSD GREATER 1

                       INITIALIZE L-OPES036L
                           REPLACING ALPHANUMERIC BY ' '
                           NUMERIC BY  0

                       MOVE 2 TO K036L-CD-FUC
                       MOVE W-TX-TRM-AUX TO K036L-TX-TRM-INVD-ENT
                       INSPECT K036L-TX-TRM-INVD-ENT
                           REPLACING ALL ' ' BY '%'

                       MOVE LENGTH OF L-OPES036L TO EIBCALEN
                       CALL OPES036L USING DFHEIBLK L-OPES036L

                       IF  K036L-TX-TRM-INVD-SAI(1) NOT EQUAL SPACES
                           PERFORM 999133-ERRO-133
                       END-IF
                   END-IF

                   MOVE SPACES TO W-TX-TRM-AUX
                   MOVE ZEROS  TO W-IC-AJSD

               END-IF

           END-PERFORM.
      *
       310-039-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   310-039-SAIDA.'
                      .
       310-039-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       310-040-VALIDA-PAIS-COM-RST   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 310-040-VALIDA-PAIS-COM-RST.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Quando tipo da ordem 1 (Swift) ou 2 (Seguro)
      *--  Busca e valida restricao de país na moeda da ordem
      *--  Caso encontrada a restrição, a ordem será invalidada

           INITIALIZE L-BCIS108L.
           MOVE 2                  TO K108L-CD-FUC.
           MOVE S500V-CD-PAIS-BNFC TO K108L-CD-PAIS-COM-RST-ENT.
           MOVE S500V-CD-MOE       TO K108L-CD-MOE-COM-RST-ENT.
           MOVE LENGTH OF L-BCIS108L TO EIBCALEN.
           CALL BCIS108L USING DFHEIBLK L-BCIS108L.

      *--  Se retorno = 3 (Nenhum registro encontrado) - Continua
           IF  K108L-CD-RTN OF L-BCIS108L EQUAL 3
               CONTINUE
           ELSE
               PERFORM 999154-ERRO-154
           END-IF.

       310-049-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   310-049-SAIDA.'
                      .
       310-049-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       320000-VLDC-IBAN              SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 320000-VLDC-IBAN.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *--  VRS054
      *--  Valida tamanho da informacao enviada no campo conta do
      *--  beneficiario no exterior - IBAN
      *--  Tamanho maximo da informacao - 34 chars
           IF  S500V-CD-CT-BNFC(35:1) NOT EQUAL SPACES
               PERFORM 999176-ERRO-176
           END-IF
           .
       320000-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   320000-SAI.'
                      .
       320000-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       410-000-VALIDA-SEGURO-02      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 410-000-VALIDA-SEGURO-02.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           MOVE S500V-DT-EMS(1:2)              TO DD-AUX-EMS-AMD.
           MOVE S500V-DT-EMS(4:2)              TO MM-AUX-EMS-AMD.
           MOVE S500V-DT-EMS(7:4)              TO AA-AUX-EMS-AMD.

           PERFORM 414-000-VERIFICA-CTL-NSS-NR.

      * Moeda somente 220, conforme DE85090.
      *
      *--  Valida código da moeda.
           IF  S500V-CD-MOE NOT EQUAL 220
               PERFORM 999118-ERRO-118
           END-IF.

      *--  Valida código do seguro.
           PERFORM 411-000-VALIDA-SEGURO.

      *--  Valida numero da apólice.
           PERFORM 412-000-VALIDA-NR-APLC.

      *--  Valida banqueiro do favorecido.
           PERFORM 710-000-VALIDA-BANQ-BNFC.

      *--  Valida país com restricao na moeda.
           PERFORM 310-040-VALIDA-PAIS-COM-RST.

           IF  S500V-CD-INST-BNFC NOT EQUAL ZEROS
               PERFORM 860-000-CHAMA-BDDSH143
           END-IF.

       410-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   410-099-SAIDA.'
                      .
       410-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       411-000-VALIDA-SEGURO         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 411-000-VALIDA-SEGURO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           MOVE S500V-CD-SGRO TO NR-SEQL-SGRA.

           EXEC SQL
               SELECT  NM_SGRA
                 INTO :NM-SGRA
                 FROM  DB2OPE.SGRA
                WHERE  NR_SEQL_SGRA = :NR-SEQL-SGRA
           END-EXEC.

           IF  SQLCODE NOT EQUAL ZEROS
               IF  SQLCODE EQUAL +100
                   GO TO 999069-ERRO-069
               ELSE
                   GO TO 999070-ERRO-070
               END-IF
           END-IF.

           EVALUATE S500V-CD-SGRO
               WHEN 09
                 MOVE 'RIO DE JANEIRO-RJ (BRASIL)' TO S500V-NM-PRCA-BNFC
                 MOVE 'BRASBRRJRJO'                TO S500V-CD-SWFT-BNFC
                       S500V-CD-SWFT-SGRO
               WHEN 34
                 MOVE 'SAO PAULO III-SP (BRASIL)'  TO S500V-NM-PRCA-BNFC
                 MOVE 'BRASBRRJOCO'                TO S500V-CD-SWFT-BNFC
                       S500V-CD-SWFT-SGRO
               WHEN OTHER
                   GO TO 999069-ERRO-069
           END-EVALUATE.

           MOVE S500V-CD-SGRO          TO S500V-NR-SEQL-SGRA.
           MOVE NM-SGRA                TO S500V-TX-SGRO.
           MOVE 105                    TO S500V-CD-PAIS-BNFC.
           MOVE 'BRASIL'               TO S500V-NM-PAIS-BNFC.
           MOVE 'BANCO DO BRASIL S.A.' TO S500V-NM-BCO-BNFC.

       411-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   411-099-SAIDA.'
                      .
       411-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       412-000-VALIDA-NR-APLC        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 412-000-VALIDA-NR-APLC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

      *--  Se o código do seguro for 34 (ALIANÇA DO BRASIL)
      *    e a forma de entrega da moeda for 20
      *    o "NOSSO NUMERO" deverá ser validado no sistema SEG.
      *    Projeto 5035 - alterada a forma entrega para 20.

           IF  S500V-CD-SGRO         EQUAL 34 AND
               S500V-CD-FMA-ETGA-MOE EQUAL 20
               IF  ( DT-AUX-EMS-AMD < DT-CTL-NSS-NR-SEG
                 AND S500V-CD-NSS-NR EQUAL ZEROS )
               OR  ( DT-AUX-EMS-AMD >= DT-CTL-NSS-NR-SEG
                 AND S500V-CD-NSS-NR-N17 EQUAL ZEROS )
                   GO TO 999073-ERRO-073
               ELSE
                   PERFORM 413-000-VALIDA-NSS-NR
               END-IF
           END-IF.

       412-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   412-099-SAIDA.'
                      .
       412-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       413-000-VALIDA-NSS-NR         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 413-000-VALIDA-NSS-NR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           INITIALIZE L-SEGSB513
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           IF DT-AUX-EMS-AMD < DT-CTL-NSS-NR-SEG
              MOVE S500V-CD-NSS-NR TO 513-CD-NSS-NR
           ELSE
              MOVE S500V-CD-NSS-NR-N17 TO 513-CD-NSS-NR
           END-IF
           MOVE S500V-VL-MOEE   TO 513-VL-MOEE.

           MOVE LENGTH OF L-SEGSB513 TO EIBCALEN
           CALL SEGSB513 USING DFHEIBLK L-SEGSB513.

           IF  513-CD-RTN NOT EQUAL 1
               GO TO 999074-ERRO-074
           END-IF.

       413-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   413-099-SAIDA.'
                      .
       413-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       414-000-VERIFICA-CTL-NSS-NR    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 414-000-VERIFICA-CTL-NSS-NR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BCIS003R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 'OPE'                 TO K003R-SG-SIS.
           MOVE 'CTL-NSS-NR'          TO K003R-CD-IDFC-ARQ.

           MOVE LENGTH OF L-BCIS003R TO EIBCALEN
           CALL BCIS003R USING DFHEIBLK L-BCIS003R.

           IF  K003R-CD-RTN-PGM NOT EQUAL ZEROS
               PERFORM 999188-ERRO-188
           END-IF.

           MOVE K003R-DT-CTL(1:2)     TO DD-CTL-NSS-NR-SEG.
           MOVE K003R-DT-CTL(4:2)     TO MM-CTL-NSS-NR-SEG.
           MOVE K003R-DT-CTL(7:4)     TO AA-CTL-NSS-NR-SEG.

       414-099-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   414-099-FIM.'
                      .
       414-099-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       500-000-VALIDA-WSUN-01        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 500-000-VALIDA-WSUN-01.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

      *--  Valida código da moeda.
           IF  S500V-CD-MOE NOT EQUAL 220
               PERFORM 999117-ERRO-117
           END-IF.

       500-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   500-099-SAIDA.'
                      .
       500-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       510-000-VALIDA-WSUN-02        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 510-000-VALIDA-WSUN-02.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           PERFORM 511-000-VALIDA-DADOS-BNFC.

           PERFORM 512-000-VALIDA-MOE-PAIS-BNFC.

           IF  (S500V-TX-DCR-FSCO-BNFC NOT EQUAL SPACES) AND
               (S500V-VL-MOEE LESS 500)
               PERFORM 999097-ERRO-097
           END-IF.

           PERFORM 513-000-CALCULA-TARIFAS-WSUN.

           COMPUTE S500V-VL-TTL           = S500V-VL-MOEE
               + S500V-VL-TTL-TARF-MOEE
           ON SIZE ERROR
              PERFORM 999194-ERRO-194
           END-COMPUTE
           COMPUTE S500V-VL-TTL-MOEN      = S500V-VL-MOEN
               + S500V-VL-TTL-TARF-MOEN
           ON SIZE ERROR
              PERFORM 999195-ERRO-195
           END-COMPUTE
           .
       510-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   510-099-SAIDA.'
                      .
       510-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      **----------------------------------------------------------------
       511-000-VALIDA-DADOS-BNFC     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 511-000-VALIDA-DADOS-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      **----------------------------------------------------------------

           PERFORM 511-500-BUSCA-TX-SRVC-ETGA.

           IF  S500V-NM-BNFC EQUAL SPACES
                PERFORM 999180-ERRO-180
           END-IF

           IF  S500V-ULT-NM-BNFC EQUAL SPACES
                PERFORM 999181-ERRO-181
           END-IF

      * -- para EUA e Mexico validar nome do estado na tabela UF-WSUN
           IF  ( S500V-CD-PAIS-BNFC EQUAL 249 OR
               S500V-CD-PAIS-BNFC EQUAL 493 )
               IF S500V-NM-EST-BNFC EQUAL ZEROS
                   PERFORM 999222-ERRO-222
               ELSE
                   PERFORM 511-100-VALIDA-NM-EST-WSUN
               END-IF
           END-IF
      * -- para Mexico validar nome da cidade na tabela CID-WSUN
           IF  S500V-CD-PAIS-BNFC EQUAL 493
               IF S500V-NM-CID-BNFC NOT EQUAL SPACES
                   PERFORM 511-200-VALIDA-CID-WSUN
               END-IF
           END-IF

           IF  (S500V-TX-END-BNFC      EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES) AND
               ((S500V-CD-SRVC-ETGA EQUAL '1') OR
               (S500V-CD-SRVC-ETGA EQUAL '3'))
               PERFORM 999098-ERRO-098
           END-IF.

           IF  (S500V-NM-CID-BNFC      EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES)
               PERFORM 999099-ERRO-099
           END-IF.

           IF  (S500V-NM-EST-BNFC      EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES)
               PERFORM 999100-ERRO-100
           END-IF.

           IF  (S500V-CD-CEP-BNFC      EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES) AND
               ((S500V-CD-SRVC-ETGA EQUAL '1') OR
               (S500V-CD-SRVC-ETGA EQUAL '3'))
               PERFORM 999101-ERRO-101
           END-IF.

           IF  (S500V-CD-TEL-BNFC      EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA EQUAL '2')
               PERFORM 999102-ERRO-102
           END-IF.

           IF  (S500V-NM-LCL-ETGA-BNFC EQUAL SPACES) AND
               (S500V-CD-SRVC-ETGA NOT EQUAL SPACES) AND
               ((S500V-CD-SRVC-ETGA EQUAL '1') OR
               (S500V-CD-SRVC-ETGA EQUAL '3'))
               PERFORM 999103-ERRO-103
           END-IF.

           IF  (S500V-CD-SRVC-ETGA         EQUAL SPACES)
               IF  (S500V-TX-DCR-FSCO-BNFC     EQUAL SPACES) AND
                   (S500V-CD-TEL-BNFC          EQUAL SPACES) AND
                   (S500V-NM-LCL-ETGA-BNFC     EQUAL SPACES)
                   CONTINUE
               ELSE
                   PERFORM 999104-ERRO-104
               END-IF
           END-IF.

           IF  ( S500V-CD-PAIS-BNFC EQUAL 249 OR
               S500V-CD-PAIS-BNFC EQUAL 493 )
               AND S500V-NM-EST-BNFC EQUAL SPACES
               AND S500V-CD-CEP-BNFC EQUAL SPACES
               PERFORM 999106-ERRO-106
           END-IF
           .
       511-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   511-099-SAIDA.'
                      .
       511-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       511-100-VALIDA-NM-EST-WSUN      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 511-100-VALIDA-NM-EST-WSUN.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE OPES051U-DADOS

           MOVE 5                   TO OPES051U-ACAO
           MOVE S500V-CD-PAIS-BNFC  TO OPES051U-CD-PAIS
           MOVE S500V-NM-EST-BNFC   TO OPES051U-NM-UF-WSUN-PSQ

           MOVE LENGTH OF OPES051U-DADOS TO EIBCALEN
           CALL OPES051U USING DFHEIBLK OPES051U-DADOS

           IF OPES051U-CD-RTN-PGM NOT EQUAL ZEROS
               IF OPES051U-CD-RTN-PGM EQUAL +100
      * --         nome não encontrado
                   PERFORM 999218-ERRO-218
               ELSE
      * --         erro ao executar OPES051U
                   PERFORM 999219-ERRO-219
               END-IF
           END-IF
           .
       511-100-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   511-100-SAIDA.'
                      .
       511-100-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       511-200-VALIDA-CID-WSUN         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 511-200-VALIDA-CID-WSUN.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE OPES052U-DADOS

           MOVE 5                   TO OPES052U-ACAO
           MOVE S500V-CD-PAIS-BNFC  TO OPES052U-CD-PAIS
           MOVE OPES051U-CD-UF-WSUN TO OPES052U-CD-UF-WSUN
           MOVE S500V-NM-CID-BNFC   TO OPES052U-NM-CID-WSUN-PSQ

           MOVE LENGTH OF OPES052U-DADOS TO EIBCALEN
           CALL OPES052U USING DFHEIBLK OPES052U-DADOS

           IF OPES052U-CD-RTN-PGM NOT EQUAL ZEROS
               IF OPES052U-CD-RTN-PGM EQUAL +100
      * --         nome não encontrado
                   PERFORM 999220-ERRO-220
               ELSE
      * --         erro ao executar OPES051U
                   PERFORM 999221-ERRO-221
               END-IF
           END-IF
           .
       511-200-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   511-200-SAIDA.'
                      .
       511-200-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       511-500-BUSCA-TX-SRVC-ETGA    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 511-500-BUSCA-TX-SRVC-ETGA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  S500V-CD-SRVC-ETGA NOT EQUAL SPACES
               EVALUATE S500V-CD-SRVC-ETGA
                   WHEN 0
                       CONTINUE
                   WHEN 1
                  MOVE 'Entrega pessoal          ' TO S500V-TX-SRVC-ETGA
                   WHEN 2
                  MOVE 'Notificação por telefone ' TO S500V-TX-SRVC-ETGA
                   WHEN 3
                  MOVE 'Notificação por telegrama' TO S500V-TX-SRVC-ETGA
                   WHEN OTHER
                       PERFORM 999105-ERRO-105
               END-EVALUATE
           ELSE
               MOVE SPACES  TO S500V-TX-SRVC-ETGA
           END-IF.

       511-599-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   511-599-SAIDA.'
                      .
       511-599-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       512-000-VALIDA-MOE-PAIS-BNFC  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 512-000-VALIDA-MOE-PAIS-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Verifica se a moeda esta cadastrada
           MOVE S500V-CD-PAIS-BNFC TO CD-PAIS OF MOE-PAIS-WSUN.
           MOVE S500V-CD-MOE-PGTO  TO CD-MOE  OF MOE-PAIS-WSUN.

           EXEC SQL
                SELECT CD_MOE
                  INTO :MOE-PAIS-WSUN.CD-MOE
                  FROM DB2OPE.MOE_PAIS_WSUN
                 WHERE CD_PAIS = :MOE-PAIS-WSUN.CD-PAIS
                   AND CD_MOE  = :MOE-PAIS-WSUN.CD-MOE
           END-EXEC.

           IF  SQLCODE NOT EQUAL ZEROS
               IF  SQLCODE EQUAL +100
                   GO TO 999050-ERRO-050
               ELSE
                   GO TO 999065-ERRO-065
               END-IF
           END-IF.

      *--  Busca o código da moeda.
           INITIALIZE L-BCIS002R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 1                  TO K002R-CD-TIP-PSQ.
           MOVE S500V-CD-MOE-PGTO  TO K002R-CD-MOE.

           MOVE LENGTH OF L-BCIS002R TO EIBCALEN
           CALL BCIS002R USING DFHEIBLK L-BCIS002R.

           IF  K002R-CD-RTN NOT EQUAL ZEROS
               IF  K002R-CD-SQL EQUAL 100
                   PERFORM 999111-ERRO-111
               ELSE
                   PERFORM 999112-ERRO-112
               END-IF
           END-IF.

           IF  K002R-IN-ATVO NOT EQUAL 'S'
               PERFORM 999113-ERRO-113
           END-IF.

           MOVE K002R-CD-SWFT   TO S500V-CD-MOE-DST-WSUN.

      *--  Busca a sigla do país.
           INITIALIZE L-BCIS006R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 1                  TO K006R-CD-FUC.
           MOVE S500V-CD-PAIS-BNFC TO K006R-COD.

           MOVE LENGTH OF L-BCIS006R TO EIBCALEN
           CALL BCIS006R USING DFHEIBLK L-BCIS006R.

           IF  K006R-CD-RTN OF L-BCIS006R NOT EQUAL ZEROS
               IF  K006R-SQLCODE-RTN EQUAL 100
                   PERFORM 999147-ERRO-147
               ELSE
                   PERFORM 999148-ERRO-148
               END-IF
           END-IF.

           MOVE K006R-COD-SWFT TO S500V-SG-PAIS-BNFC.

           INITIALIZE L-OPES012R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 01                 TO S012R-CD-FUC.
           MOVE S500V-CD-PAIS-BNFC TO S012R-CD-PAIS-ENT.

           MOVE LENGTH OF L-OPES012R TO EIBCALEN
           CALL OPES012R USING DFHEIBLK L-OPES012R.

           IF  S012R-CD-RTN NOT EQUAL ZEROS
               PERFORM 999114-ERRO-114
           END-IF.

           IF  S012R-IN-OBGD-QST-VRF-SAI EQUAL 'S'
               IF  S500V-TX-QST-VRF     EQUAL SPACES OR
                   S500V-TX-RPST        EQUAL SPACES
                   PERFORM 999142-ERRO-142
               END-IF
           ELSE
               IF  S500V-TX-QST-VRF NOT EQUAL SPACES OR
                   S500V-TX-RPST    NOT EQUAL SPACES
                   PERFORM 999143-ERRO-143
               END-IF
           END-IF.

       512-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   512-099-SAIDA.'
                      .
       512-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       513-000-CALCULA-TARIFAS-WSUN  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 513-000-CALCULA-TARIFAS-WSUN.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE L-OPEP802S TARF-FXA-VL-ORD-PG.

           IF  S500V-CD-IDFR-SIS NOT EQUAL SPACES

               IF  S500V-NR-SEQL-TRML EQUAL SPACES
                   PERFORM 960-000-BUSCA-SEQL-TRML
               END-IF

      *--  Busca tarifas junto a Western Union
               MOVE ZEROS                    TO P802S-CD-RTN
               MOVE S500V-TS-INC-TRAN        TO P802S-TS-INC-TRAN
               MOVE S500V-CD-IDFR-SIS        TO P802S-CD-IDFR-SIS
               MOVE S500V-CD-USU             TO P802S-CD-USU
               MOVE S500V-CD-PRF-DEPE-EMT    TO P802S-CD-PRF-DEPE-EMT
               MOVE S500V-NR-SEQL-TRML       TO P802S-NR-SEQL-TRML
               MOVE S500V-CD-MOE-DST-WSUN    TO P802S-CD-MOE-DST-WSUN
               MOVE S500V-CD-MOE-OGM-WSUN    TO P802S-CD-MOE-OGM-WSUN
               MOVE S500V-TX-MSG-TAB         TO P802S-TX-MSG-TAB
               MOVE S500V-CD-SRVC-ETGA       TO P802S-CD-SRVC-ETGA
               MOVE S500V-VL-MOEE            TO P802S-VL-MOEE

               MOVE S500V-CD-PRF-DEPE-OPR    TO P802S-CD-PRF-DEPE-OPR
               MOVE S500V-NR-CC-TMDR         TO P802S-NR-CC-TMDR
               MOVE S500V-DV-CC-TMDR         TO P802S-DV-CC-TMDR
               MOVE S500V-CD-TIP-ITCE-REG    TO P802S-CD-TIP-ITCE
               MOVE S500V-SG-PAIS-BNFC       TO P802S-SG-PAIS-BNFC

               EXEC CICS LINK
                         PROGRAM  ( OPEP802S )
                         COMMAREA ( L-OPEP802S )
                         LENGTH   ( LENGTH OF L-OPEP802S )
               END-EXEC

               MOVE P802S-TS-INC-TRAN TO S500V-TS-INC-TRAN

               IF  P802S-CD-RTN NOT EQUAL 001
                   PERFORM 999110-ERRO-110
               END-IF
           END-IF.

      *--  Busca tarifas na base de dados do OPE
           MOVE S500V-VL-MOEE TO VL-INC-ORD-PGTO
               VL-FIM-ORD-PGTO.

           EXEC SQL
                SELECT VL_TARF_ORD_PGTO
                     , TS_TARF
                  INTO :VL-TARF-ORD-PGTO
                     , :TS-TARF
                  FROM DB2OPE.TARF_FXA_VL_ORD_PG
                 WHERE TS_TARF         > '2000-01-01-00.00.00.000000'
                   AND VL_INC_ORD_PGTO <= :VL-INC-ORD-PGTO
                   AND VL_FIM_ORD_PGTO >= :VL-FIM-ORD-PGTO
                   AND CD_EST_TARF      = 1
           END-EXEC.

           IF  SQLCODE NOT EQUAL ZEROS
               GO TO 999108-ERRO-108
           END-IF.

           IF  W-AMBIENTE NOT EQUAL 'HOM'
           AND W-AMBIENTE NOT EQUAL 'DES'
      *--  Compara tarifa da Western Union com a existente na base OPE
               MOVE P802S-VL-TARF TO W-VL-TARF
               IF  W-VL-TARF NOT EQUAL VL-TARF-ORD-PGTO
                   PERFORM 999109-ERRO-109
               END-IF
           END-IF.

           MOVE TS-TARF TO S500V-TS-TARF.

      *--  Cálculo das tarifas em moeda estrangeira
           MOVE VL-TARF-ORD-PGTO  TO S500V-VL-TARF.
           MOVE P802S-VL-TARF-ADC TO S500V-VL-TARF-ADC.
           MOVE P802S-VL-IMPO-MSG TO S500V-VL-IMPO-MSG.
           COMPUTE S500V-VL-TTL-TARF-MOEE = S500V-VL-TARF
               + S500V-VL-IMPO-MSG
               + S500V-VL-TARF-ADC
           ON SIZE ERROR
              PERFORM 999196-ERRO-196
           END-COMPUTE

      *--  Cálculo da tarifas em moeda nacional
           COMPUTE S500V-VL-TARF-MOEN     ROUNDED = S500V-VL-TARF
                                                  * S500V-VL-TAXA-CMB
           ON SIZE ERROR
              PERFORM 999197-ERRO-197
           END-COMPUTE
           COMPUTE S500V-VL-TARF-ADC-MOEN ROUNDED = S500V-VL-TARF-ADC
                                                  * S500V-VL-TAXA-CMB
           ON SIZE ERROR
              PERFORM 999198-ERRO-198
           END-COMPUTE
           COMPUTE S500V-VL-IMPO-MSG-MOEN ROUNDED = S500V-VL-IMPO-MSG
                                                  * S500V-VL-TAXA-CMB
           ON SIZE ERROR
              PERFORM 999199-ERRO-199
           END-COMPUTE
           COMPUTE S500V-VL-TTL-TARF-MOEN = S500V-VL-TARF-MOEN
               + S500V-VL-TARF-ADC-MOEN
               + S500V-VL-IMPO-MSG-MOEN
           ON SIZE ERROR
              PERFORM 999200-ERRO-200
           END-COMPUTE
           .
       513-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   513-099-SAIDA.'
                      .
       513-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       700-000-VALIDA-PAIS-BNFC      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 700-000-VALIDA-PAIS-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  S500V-CD-PAIS-BNFC EQUAL ZEROS
               GO TO 999011-ERRO-011
           END-IF.

      * -- Verifica se a natureza eh de seguro
           PERFORM 701000-VRF-NTZ-SGRO

           IF (S500V-CD-TIP-ORD-PGTO = 1 AND NOT NATUREZA-SEGURO)
           OR  S500V-CD-TIP-ORD-PGTO = 3
               IF  S500V-CD-PAIS-BNFC EQUAL 105
                   GO TO 999012-ERRO-012
               END-IF
           END-IF

           MOVE S500V-CD-PAIS-BNFC  TO 003-CD-PAIS.

           MOVE LENGTH OF L-BCISO003 TO EIBCALEN
           CALL BCISO003 USING DFHEIBLK L-BCISO003.

           IF  003-CD-ERRO NOT EQUAL ZEROS
               GO TO 999013-ERRO-013
           END-IF.

           MOVE 003-NM-PAIS      TO S500V-NM-PAIS-BNFC.

       700-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   700-099-SAIDA.'
                      .
       700-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.


      *----------------------------------------------------------------*
       701000-VRF-NTZ-SGRO           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 701000-VRF-NTZ-SGRO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * -- Regra 15(OPE) - faixa de fato operacional de seguro
           MOVE 15       TO W-RGR-ENT

           PERFORM 919-000-CHAMA-BCIS051N

           IF  BCIS051N-RTN-PSQ EQUAL 1
               SET NATUREZA-SEGURO TO TRUE
           END-IF
           .
       701000-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   701000-SAI.'
                      .
       701000-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-000-VALIDA-BANQ-BNFC      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-000-VALIDA-BANQ-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE S500V-MT-INST-FNCR
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           IF  S500V-CD-SWFT-BNFC NOT EQUAL S500V-CD-SWFT-BNFC-ANT
           OR  S500V-NM-BCO-BNFC  NOT EQUAL S500V-NM-BCO-BNFC-ANT
           OR  S500V-NM-PRCA-BNFC NOT EQUAL S500V-NM-PRCA-BNFC-ANT
               INITIALIZE S500V-VRV-INST-FNCR
                   REPLACING ALPHANUMERIC BY ' '
                   NUMERIC BY  0
               MOVE S500V-CD-SWFT-BNFC TO S500V-CD-SWFT-BNFC-ANT
               MOVE S500V-NM-BCO-BNFC  TO S500V-NM-BCO-BNFC-ANT
               MOVE S500V-NM-PRCA-BNFC TO S500V-NM-PRCA-BNFC-ANT
           END-IF.

           MOVE S500V-CD-SWFT-BNFC TO W-CD-SWFT-PSQ.
           MOVE S500V-NM-BCO-BNFC  TO W-NM-BCO-PSQ.
           MOVE S500V-NM-PRCA-BNFC TO W-NM-PRCA-PSQ.
           MOVE ZEROS              TO W-NR-NDX.

           IF  S500V-CD-SWFT-BNFC NOT EQUAL SPACES
               MOVE S500V-CD-SWFT-BNFC TO W-CD-SWFT-PSQ
      *
      * --     Verifica se o codigo swift do beneficiario possui espacos
      * --     em branco.
               MOVE ZEROS TO W-COUNT
               INSPECT W-CD-SWFT-A8 TALLYING W-COUNT FOR ALL SPACES
               IF W-COUNT NOT EQUAL ZEROS
                  PERFORM 999178-ERRO-178
               END-IF
               IF W-CD-SWFT-A3(1:1) NOT EQUAL SPACE
                  IF W-CD-SWFT-A3(2:1) NOT EQUAL SPACE
                     IF W-CD-SWFT-A3(3:1) NOT EQUAL SPACE
                        CONTINUE
                     ELSE
                        PERFORM 999179-ERRO-179
                     END-IF
                  ELSE
                     IF W-CD-SWFT-A3(3:1) NOT EQUAL SPACE
                        PERFORM 999178-ERRO-178
                     ELSE
                        PERFORM 999179-ERRO-179
                     END-IF
                  END-IF
               ELSE
                  IF W-CD-SWFT-A3(2:1) EQUAL SPACE
                     IF W-CD-SWFT-A3(3:1) EQUAL SPACE
                        CONTINUE
                     ELSE
                        PERFORM 999178-ERRO-178
                     END-IF
                  ELSE
                     PERFORM 999178-ERRO-178
                  END-IF
               END-IF
      *
      *        PERFORM 710-010-VRFR-RCBD-SWFT
               PERFORM 710-200-TRATA-CSR-SWFT
           ELSE
               IF  W-NM-BCO-PSQ NOT EQUAL SPACES
               OR  W-NM-PRCA-PSQ NOT EQUAL SPACES
                   PERFORM 710-400-TRATA-CSR-BCO
               END-IF
           END-IF.

      *--  O codigo de erro 7777 indica  que a lista de banqueiro está
      *    carregada e deverá ser mostrada na interface de registro da
      *    ordem.
           IF  W-NR-NDX GREATER 1
               IF  S500V-CD-CNL NOT EQUAL 'B'
                   IF  S500V-CD-CNL EQUAL 'I'
                       IF  S500V-CD-TIP-ITCE-REG EQUAL 12 OR
                           S500V-CD-TIP-ITCE-REG EQUAL 15 OR
                           S500V-CD-TIP-ITCE-REG EQUAL 20
      *-- Ordens emitidas em Euro (978) serao enviadas para DEUTDEFF
                           IF S500V-CD-MOE NOT EQUAL 978
                              MOVE W-CD-IFC-CAIXA TO S500V-CD-INST-DST
                           END-IF
                       ELSE
                           PERFORM 999138-ERRO-138
                       END-IF
                   ELSE
                       MOVE 7777 TO S500V-CD-RTN
                       GOBACK
                   END-IF
               END-IF
           ELSE
               IF  W-NR-NDX              NOT EQUAL ZEROS AND
                   S500V-CD-CVN-ORD-PGTO NOT EQUAL 1
                   IF  S500V-CD-TIP-ITCE-REG EQUAL 55 OR
                       S500V-CD-TIP-ITCE-REG EQUAL 20 OR
                       S500V-CD-TIP-ITCE-REG EQUAL 12 OR
                       S500V-CD-TIP-ITCE-REG EQUAL 15
      * > ALM: 541955
      * > Quando o SWIFT informado não constar no livro de banqueiro,
      * > deve-se utilizar o banqueiro caixa como destinatario.
      * > adicionado interface mobile(20) no if acima
      *-- Ordens emitidas em Euro (978) serao enviadas para DEUTDEFF
                       IF S500V-CD-MOE NOT EQUAL 978
                          MOVE W-CD-IFC-CAIXA TO S500V-CD-INST-DST
                       END-IF
                   ELSE

      * >>        Evitar alteração indevida de banqueiro em oprs sisBB
      *           conforme exemplo do Resolve 7687747 para o caso do
      *           banqueiro UBSWUS33 sendo substituído por UBSWUS33PAK:
      *      Interface 3 -> sisBB
                    IF (S500V-CD-TIP-ITCE-REG EQUAL 3
                    AND W-NR-NDX = 1
                    AND S500V-CD-SWFT-BNFC(1:8) EQUAL
                                            S500V-CD-SWFT(W-NR-NDX)(1:8)
                    AND S500V-CD-SWFT-BNFC(9:3) EQUAL
                                           S500V-CD-SWFT(W-NR-NDX)(9:3))

                       MOVE S500V-CD-INST(1) TO S500V-CD-INST-BNFC
                       MOVE S500V-CD-SWFT(1) TO S500V-CD-SWFT-BNFC
                       MOVE S500V-NM-BCO(1)  TO S500V-NM-BCO-BNFC
                       MOVE S500V-NM-PRCA(1) TO S500V-NM-PRCA-BNFC

      *             Caso contrário, não altera, para evitar erros
                     ELSE
                        CONTINUE
                    END-IF
      *
      *                 MOVE S500V-CD-INST(1) TO S500V-CD-INST-BNFC
      *                 MOVE S500V-CD-SWFT(1) TO S500V-CD-SWFT-BNFC
      *                 MOVE S500V-NM-BCO(1)  TO S500V-NM-BCO-BNFC
      *                 MOVE S500V-NM-PRCA(1) TO S500V-NM-PRCA-BNFC
      *
                   END-IF
               END-IF
           END-IF.

      *    Trata código de compensação - VRS055
           IF  S500V-CD-CVN-ORD-PGTO  EQUAL 1      AND
               S500V-CD-CPSO          EQUAL SPACES AND
               S500V-CD-INST-BNFC NOT EQUAL ZEROS
               PERFORM 711-000-TRATA-CD-CPSO
           ELSE
               IF  S500V-CD-PAIS-BNFC    EQUAL 249    AND
                   S500V-CD-TIP-ORD-PGTO EQUAL 1      AND
                   S500V-CD-CPSO     NOT EQUAL SPACES
      *        Validar preechimento das informações do campo CPSO
                   PERFORM 711-500-TRATA-CD-CPSO
               END-IF
           END-IF.

           IF  S500V-CD-CNL           EQUAL 'I' OR
               S500V-CD-CNL           EQUAL 'B'
      *        VRS045 - Verifica se data de emissao esta preenchida
      *                 pois e obrigatoria para o OPES043R
               IF  S500V-DT-EMS NOT EQUAL SPACES
                   IF  S500V-CD-CVN-ORD-PGTO NOT EQUAL ZEROS OR
                       S500V-CD-INST-BNFC    NOT EQUAL ZEROS
                       PERFORM 710-050-RECUPERA-DADOS-CVN
                   END-IF
               END-IF
      *
      *        BB Americas - Por padrao o banco destinatario e o
      *        BB New York, a moeda deve ser dolar americano e a
      *        despesa por conta do Tomador
      *        IF  S500V-CD-INST-BNFC EQUAL 125970127101
      *            IF  S500V-CD-MOE NOT EQUAL 220
      *                GO TO 999155-ERRO-155
      *            END-IF
      *            IF  S500V-CD-PGDR-DSP-EXNO NOT EQUAL 'F'
      *                GO TO 999156-ERRO-156
      *            END-IF
      *            IF  S500V-CD-PAIS-BNFC NOT EQUAL 249
      *                GO TO 999157-ERRO-157
      *            END-IF
      *        Para esse destinatario, os demais campos de instituicao
      *        financeiras devem ser desconsiderados. O destinatario
      *        sera sempre o BB New York
      *            INITIALIZE S500V-VRV-INST-FNCR
      *                       REPLACING NUMERIC BY  0
      *                            ALPHANUMERIC BY ' '
      *            MOVE 1000059801      TO S500V-CD-INST-DST
      *        END-IF
           END-IF.
      *
           MOVE S500V-CD-SWFT-BNFC TO W-CD-SWFT-PSQ.
      *    PERFORM 710-010-VRFR-RCBD-SWFT.

       710-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-099-SAIDA.'
                      .
       710-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      * ----------------------------------------------------------------
       710-050-RECUPERA-DADOS-CVN     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-050-RECUPERA-DADOS-CVN.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      * ----------------------------------------------------------------
      *
      *    Verifica se existe convenio para a ordem, uma vez que essa
      *    funcionalidade nao foi implementada nos canais
           INITIALIZE  L-OPES043R
               REPLACING ALPHANUMERIC BY SPACES
               NUMERIC BY ZEROS
      *
           IF  S500V-CD-CVN-ORD-PGTO NOT EQUAL ZEROS
               MOVE 2                   TO K043R-CD-FUC
           ELSE
               MOVE 3                   TO K043R-CD-FUC
           END-IF
           MOVE S500V-CD-CVN-ORD-PGTO   TO K043R-CD-CVN-ORD-PGTO-ENT
           MOVE S500V-CD-INST-BNFC      TO K043R-CD-IFC-BNFC-ENT
           MOVE S500V-CD-TIP-ORD-PGTO   TO K043R-CD-TIP-ORD-PGTO-ENT
           MOVE S500V-DT-EMS            TO K043R-DT-INC-VGC-RGR-ENT
           MOVE S500V-CD-TIP-ITCE-REG   TO K043R-CD-TIP-ITCE-ENT
      *
           MOVE LENGTH OF L-OPES043R TO EIBCALEN
           CALL OPES043R USING DFHEIBLK L-OPES043R.
      *
           IF  K043R-CD-RTN         NOT EQUAL ZEROS AND
               K043R-CD-SQLCODE-RTN NOT EQUAL 100
               GO TO 999161-ERRO-161
           ELSE
               IF  K043R-CD-RTN EQUAL ZEROS
                   IF  K043R-CD-PAIS-SAI          NOT EQUAL ZEROS
                       MOVE K043R-CD-PAIS-SAI
                           TO S500V-CD-PAIS-BNFC
                   END-IF
                   IF  S500V-CD-CNL       EQUAL 'B' AND
                       S500V-CD-SWFT-BNFC NOT EQUAL SPACES
                       CONTINUE
                   ELSE
                       IF  K043R-CD-IFC-BNFC-SAI      NOT EQUAL ZEROS
                           MOVE K043R-CD-IFC-BNFC-SAI
                               TO S500V-CD-INST-BNFC
                       END-IF
                   END-IF
                   IF  K043R-CD-IFC-DSTR-SAI      NOT EQUAL ZEROS
                       MOVE K043R-CD-IFC-DSTR-SAI
                         TO S500V-CD-INST-DST
                   END-IF
                   IF  K043R-CD-PGDR-DSP-EXNO-SAI NOT EQUAL SPACES
                       MOVE K043R-CD-PGDR-DSP-EXNO-SAI
                         TO S500V-CD-PGDR-DSP-EXNO
                       MOVE K043R-VL-DSP-EXNO-MOE-SAI
                         TO S500V-VL-DSP-EXNO-MOEE
                   END-IF
                   IF  K043R-CD-CVN-ORD-PGTO-SAI  NOT EQUAL ZEROS
                       MOVE K043R-CD-CVN-ORD-PGTO-SAI
                         TO S500V-CD-CVN-ORD-PGTO
                   END-IF
               END-IF
           END-IF.
      *
       710-059-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-059-FIM.'
                      .
       710-059-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *----------------------------------------------------------------*
      *710-010-VRFR-RCBD-SWFT    SECTION.
      *----------------------------------------------------------------*
      * >> A Informação constante na consulta ao Livro de Banqueiros da
      *    BDD resultante da chamada a rotina BDDS833R referente a con-
      *    dição de enviar e receber mensagens SWIFT é verificada sob a
      *    perspectiva do SWIFT, por ex:  um código SWIFT com as infor-
      *    MAÇOES ENVIAR = 'S'  E  RECEBER = 'N'  está habilitado a en-
      *    viar mensagens e não está habilitado a receber mensagens.
      *
      *    INITIALIZE L-BDDS833R REPLACING ALPHANUMERIC BY SPACES
      *        NUMERIC BY ZEROS
      *
      *    MOVE 'O'             TO BDDA833A-CD-ACAO
      *    MOVE W-CD-SWFT-A8    TO BDDA833A-CD-SWFT-IFC
      *    MOVE 2               TO BDDA833A-CD-TRG-MSG-AUTZ
      *
      *    MOVE LENGTH OF L-BDDS833R TO EIBCALEN
      *    CALL BDDS833R USING DFHEIBLK L-BDDS833R.
      *
      * >> A DE 118479 objetiva validar a possibilidade de envio de
      *    mensagem SWIFT para um banqueiro junto a BDD.
      *
      * >> Para verificar a condição de recebimento de mensagem pelo
      *    SWIFT é necessário verificar o estado do SWIFT armazenado
      *    como CD-EST-MSG-AUTZ onde o valor 1 significa HABILITADO,
      *    isto é, o SWIFT informado recebe mensagens.
      *
      *    IF  BDDA833A-CD-EST-MSG-AUTZ NOT EQUAL 1
      *        GO TO 999160-ERRO-160
      *    END-IF.
      *
      *710-009-SAIDA.
      *    EXIT.
      *
      *----------------------------------------------------------------*
       710-200-TRATA-CSR-SWFT        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-200-TRATA-CSR-SWFT.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           PERFORM 710-300-CHAMA-BDDE142K.

           IF  BDDA142B-QT-REG EQUAL ZEROS
           AND S500V-CD-CNL NOT EQUAL 'B'
      * > Remessas em dolar devem transitar por CITIUS33
      * > Remessas em euro devem transitar por DEUTDEFF
               IF  S500V-CD-MOE NOT EQUAL 220
               AND S500V-CD-MOE NOT EQUAL 978
                   IF (S500V-CD-TIP-ITCE-REG EQUAL 12    OR
                       S500V-CD-TIP-ITCE-REG EQUAL 55    OR
                       S500V-CD-TIP-ITCE-REG EQUAL 20    OR
                       S500V-CD-TIP-ITCE-REG EQUAL 15    )
      * > ALM: 541955
      * > Quando o SWIFT informado não constar no livro de banqueiro,
      * > deve-se utilizar o banqueiro caixa como destinatario.
      * > adicionado interface mobile(20) no if acima
      * > adicionado interface aapf(12) no if acima
                      MOVE W-CD-IFC-CAIXA TO S500V-CD-INST-DST
                   ELSE
                       INITIALIZE L-BDDS145C

                       MOVE 4              TO BDDS145C-ACAO
                       MOVE W-CD-SWFT-PSQ(1:8)
                                           TO BDDS145C-CD-IDFR-ITBC-INTL

      * MOCK-POINT MCKV4 W-CD-SWFT-PSQ

                       IF  W-CD-SWFT-PSQ(9:3) EQUAL SPACES
                           MOVE 'XXX'
                                           TO BDDS145C-CD-AG-IFC-SEM-RLC
                       ELSE
                           MOVE W-CD-SWFT-PSQ(9:3)
                                           TO BDDS145C-CD-AG-IFC-SEM-RLC
                       END-IF

                       MOVE LENGTH OF L-BDDS145C TO EIBCALEN
                       CALL BDDS145C USING DFHEIBLK L-BDDS145C

                       IF  BDDS145C-CD-RTN-PGM NOT EQUAL ZEROS
                           MOVE ZEROS TO S500V-CD-INST-BNFC
                           GO TO 999043-ERRO-043
                       END-IF

                       IF  S500V-NM-BCO-BNFC EQUAL SPACES
                           MOVE BDDS145C-NM-IFC-SEM-RLC
                             TO S500V-NM-BCO-BNFC
                       END-IF

                       IF  S500V-NM-PRCA-BNFC EQUAL SPACES
                           MOVE BDDS145C-NM-PRCA-SEM-RLC                -BNFC
                             TO S500V-NM-PRCA-BNFC
                       END-IF
                   END-IF
               END-IF
           END-IF.

           MOVE ZEROS TO W-NR-NDX.

           PERFORM VARYING W-IC FROM 1 BY 1
               UNTIL W-IC > BDDA142B-QT-REG
               ADD 1 TO W-NR-NDX
               PERFORM 710-600-TRATA-BCO-BNFC
           END-PERFORM.

       710-299-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-299-SAIDA.'
                      .
       710-299-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-300-CHAMA-BDDE142K        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-300-CHAMA-BDDE142K.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BDDE142X
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE W-CD-SWFT-PSQ TO BDDA142B-CD-SWFT-ARG.

           MOVE LENGTH OF L-BDDE142X TO EIBCALEN
           CALL BDDE142K USING DFHEIBLK L-BDDE142X.

           IF  BDDAMSGS-CD-MSG OF L-BDDE142X NOT EQUAL ZEROS AND 90
               IF  S500V-CD-CNL NOT EQUAL 'B'
                   GO TO 999052-ERRO-052
               END-IF
           ELSE
               IF  BDDAMSGS-CD-MSG OF L-BDDE142X EQUAL 90
                   MOVE ZEROS TO BDDA142B-QT-REG
               END-IF
           END-IF.

       710-399-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-399-SAIDA.'
                      .
       710-399-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-210-TRATA-CSR-SWFT-INT    SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-210-TRATA-CSR-SWFT-INT.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE S500V-CD-SWFT-INT TO W-CD-SWFT-PSQ.

           PERFORM 710-300-CHAMA-BDDE142K.

           IF  BDDA142B-QT-REG EQUAL ZEROS
      *        IF  S500V-CD-INST-DST     EQUAL ZEROS AND (
               IF (S500V-CD-TIP-ITCE-REG EQUAL 12 OR
                   S500V-CD-TIP-ITCE-REG EQUAL 55 OR
                   S500V-CD-TIP-ITCE-REG EQUAL 20 OR
                   S500V-CD-TIP-ITCE-REG EQUAL 15    )
      * > ALM: 541955
      * > Quando o SWIFT informado não constar no livro de banqueiro,
      * > deve-se utilizar o banqueiro caixa como destinatario.
                  MOVE W-CD-IFC-CAIXA TO S500V-CD-INST-DST
               ELSE
                   GO TO 999043-ERRO-043
               END-IF
           END-IF.

           MOVE BDDA142B-CD-IDFR(1) TO S500V-CD-INST-INT
           PERFORM 820-000-TRATA-INST-INTM.

       710-299-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-299-SAIDA.'
                      .
       710-299-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-400-TRATA-CSR-BCO         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-400-TRATA-CSR-BCO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE ZEROS TO W-NR-NDX.

           PERFORM 710-500-FETCH-CSR-BCO.

           MOVE ZEROS TO W-NR-NDX.

           PERFORM VARYING W-IC FROM 1 BY 1
               UNTIL W-IC > BDDA142B-QT-REG
               ADD 1 TO W-NR-NDX
               PERFORM 710-600-TRATA-BCO-BNFC
           END-PERFORM.

       710-499-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-499-SAIDA.'
                      .
       710-499-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-500-FETCH-CSR-BCO         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-500-FETCH-CSR-BCO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BDDE142X
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE W-NM-BCO-PSQ TO BDDA142B-NM-HDNG-ARG.
           MOVE W-NM-PRCA-PSQ TO BDDA142B-NM-PRCA-ARG.

           MOVE LENGTH OF L-BDDE142X TO EIBCALEN
           CALL BDDE142J USING DFHEIBLK L-BDDE142X.

           IF  BDDAMSGS-CD-MSG OF L-BDDE142X NOT EQUAL ZEROS AND 90
               GO TO 999052-ERRO-052
           ELSE
               IF  BDDAMSGS-CD-MSG OF L-BDDE142X EQUAL 90
                   MOVE ZEROS TO BDDA142B-QT-REG
               END-IF
           END-IF.

       710-599-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-599-SAIDA.'
                      .
       710-599-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-600-TRATA-BCO-BNFC        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-600-TRATA-BCO-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  W-NR-NDX <= 10
               MOVE BDDA142B-NM-INST-FNCR(W-IC) TO
                   S500V-NM-BCO (W-NR-NDX)
               MOVE BDDA142B-NM-PRAC     (W-IC) TO
                   S500V-NM-PRCA(W-NR-NDX)
               MOVE BDDA142B-CD-SWFT     (W-IC) TO
                   S500V-CD-SWFT(W-NR-NDX)
               MOVE BDDA142B-CD-IDFR     (W-IC) TO
                   S500V-CD-INST(W-NR-NDX)
           END-IF.

           IF     (BDDA142B-CD-IDFR     (W-IC) EQUAL S500V-CD-INST-BNFC
               AND BDDA142B-NM-INST-FNCR(W-IC) EQUAL S500V-NM-BCO-BNFC
               AND BDDA142B-NM-PRAC     (W-IC) EQUAL S500V-NM-PRCA-BNFC
               AND BDDA142B-CD-SWFT     (W-IC) EQUAL S500V-CD-SWFT-BNFC)
      **
      ** Vrs022 - Para canal INTERNET comparar apenas COD INSTITUICAO ou
      ** NM-BANCO NM-PRACA - CD-SWIFT
      **
               OR ((S500V-CD-CNL EQUAL 'I' OR
                    S500V-CD-CNL EQUAL 'W' OR
                    S500V-CD-CNL EQUAL 'B') AND
                    BDDA142B-CD-IDFR (W-IC) EQUAL S500V-CD-INST-BNFC)
      **
               OR (S500V-CD-CNL EQUAL 'I'
               AND BDDA142B-NM-INST-FNCR(W-IC) EQUAL S500V-NM-BCO-BNFC
               AND BDDA142B-NM-PRAC     (W-IC) EQUAL S500V-NM-PRCA-BNFC
               AND BDDA142B-CD-SWFT     (W-IC) EQUAL S500V-CD-SWFT-BNFC)

      * >>        Evitar alteração indevida de banqueiro em oprs da PLT
      *           conforme exemplo do Resolve 7687747 para o caso do
      *           banqueiro UBSWUS33 sendo substituído por UBSWUS33PAK:
                 IF (S500V-CD-TIP-ITCE-REG EQUAL 55
                  AND W-NR-NDX = 1
                  AND S500V-CD-SWFT-BNFC(1:8) EQUAL
                                            S500V-CD-SWFT(W-NR-NDX)(1:8)
                  AND S500V-CD-SWFT-BNFC(9:3) EQUAL
                                           S500V-CD-SWFT(W-NR-NDX)(9:3))

                  MOVE BDDA142B-NM-INST-FNCR(W-IC) TO S500V-NM-BCO-BNFC
                  MOVE BDDA142B-NM-PRAC     (W-IC) TO S500V-NM-PRCA-BNFC
                  MOVE BDDA142B-CD-SWFT     (W-IC) TO S500V-CD-SWFT-BNFC

                 ELSE
                    CONTINUE
                 END-IF
      *
      *         MOVE BDDA142B-NM-INST-FNCR(W-IC) TO S500V-NM-BCO-BNFC
      *         MOVE BDDA142B-NM-PRAC     (W-IC) TO S500V-NM-PRCA-BNFC
      *         MOVE BDDA142B-CD-SWFT     (W-IC) TO S500V-CD-SWFT-BNFC
      *
               MOVE ZEROS TO W-NR-NDX
               MOVE 999   TO W-IC
           END-IF.
      *
       710-699-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-699-SAIDA.'
                      .
       710-699-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       710-700-FETCH-CSR-DSTR        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 710-700-FETCH-CSR-DSTR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BDDE142X
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE W-CD-PAIS-PSQ TO BDDA142B-CD-PAIS-ARG
           MOVE W-CD-SB-ARG   TO BDDA142B-CD-SB-ARG

           MOVE LENGTH OF L-BDDE142X TO EIBCALEN
           CALL BDDE142K USING DFHEIBLK L-BDDE142X.

           IF  BDDAMSGS-CD-MSG OF L-BDDE142X NOT EQUAL ZEROS AND 90
               IF  S500V-CD-CNL NOT EQUAL 'B'
                   GO TO 999052-ERRO-052
               END-IF
           ELSE
               IF  BDDAMSGS-CD-MSG OF L-BDDE142X EQUAL 90
                   MOVE ZEROS TO BDDA142B-QT-REG
               END-IF
           END-IF.

       710-799-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   710-799-SAIDA.'
                      .
       710-799-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       711-000-TRATA-CD-CPSO         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 711-000-TRATA-CD-CPSO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BDDE142H BDDA142A
               W-CD-FED-ABA
               W-CD-CPSO
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-CD-INST-BNFC TO BDDA142A-CD-IDTR.

           MOVE LENGTH OF L-BDDE142H TO EIBCALEN
           CALL BDDE142H USING DFHEIBLK L-BDDE142H.

           IF  BDDAMSGS-CD-MSG OF L-BDDE142H NOT EQUAL ZEROS
               PERFORM 999129-ERRO-129
           END-IF.

           IF  BDDA142A-CD-FED-ABA IS NUMERIC AND
               BDDA142A-CD-FED-ABA NOT EQUAL ZEROS
               MOVE BDDA142A-CD-FED-ABA TO W-CD-FED-ABA
               MOVE W-CD-CPSO           TO S500V-CD-CPSO
           END-IF.

       711-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   711-099-SAIDA.'
                      .
       711-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       711-500-TRATA-CD-CPSO         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 711-500-TRATA-CD-CPSO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE S500V-CD-CPSO           TO W-CD-CPSO-AUX

           IF  S500V-CD-CPSO(1:1)     EQUAL '/'
               MOVE S500V-CD-CPSO(2:13) TO W-CD-CPSO-AUX
           END-IF

           IF  S500V-CD-CPSO(2:1)     EQUAL '/'
               MOVE SPACES              TO W-CD-CPSO-AUX
               MOVE S500V-CD-CPSO(3:13) TO W-CD-CPSO-AUX
           END-IF

           EVALUATE W-CD-CPSO-AUX(1:2)
               WHEN 'FW'
                   IF  W-CD-CPSO-AUX(3:9) IS NOT NUMERIC OR
                       W-CD-CPSO-AUX(12:) NOT EQUAL SPACES
                       PERFORM 999212-ERRO-212
                   END-IF
               WHEN 'CP'
                   IF  W-CD-CPSO-AUX(3:4) IS NOT NUMERIC OR
                       W-CD-CPSO-AUX(7:)  NOT EQUAL SPACES
                       PERFORM 999213-ERRO-213
                   END-IF
               WHEN 'CH'
                   IF  W-CD-CPSO-AUX(3:6) IS NOT NUMERIC OR
                       W-CD-CPSO-AUX(9:)  NOT EQUAL SPACES
                       PERFORM 999214-ERRO-214
                   END-IF
               WHEN OTHER
                   PERFORM 999215-ERRO-215
           END-EVALUATE.

           STRING '//' W-CD-CPSO-AUX DELIMITED BY SIZE
             INTO S500V-CD-CPSO.

       711-599-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   711-599-SAIDA.'
                      .
       711-599-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       720-000-RECUPERA-CTR-CCA      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 720-000-RECUPERA-CTR-CCA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-CCAS006R
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 1                     TO K006R-CD-TIP-PSQ.
           MOVE S500V-NR-SEQL-CTR-VND TO K006R-NR-SEQL-CTR.

           MOVE LENGTH OF L-CCAS006R TO EIBCALEN
           CALL CCAS006R USING DFHEIBLK L-CCAS006R.

           IF  K006R-CD-RTN OF L-CCAS006R NOT EQUAL ZEROS
               PERFORM 999121-ERRO-121
           END-IF.

       720-099-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   720-099-FIM.'
                      .
       720-099-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       800-000-VALIDA-INST-FNCR      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 800-000-VALIDA-INST-FNCR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *--  Valida instituição financeira.

      * >> Entrega 1806474 História 1805008
      * >> Qdo for Dolar(220) APJ o utilizar BOFAUS3N senão CHASUS33
      * >> Exceto quando for BB Nova York ou BB Miami
      *
           IF S500V-CD-CVN-ORD-PGTO   EQUAL ZEROS
      * >> Quando SISBB as GECEX's informam o destinatario
              IF  S500V-CD-TIP-ITCE-REG NOT EQUAL 3
                  IF  S500V-CD-MOE           EQUAL 220
                  AND S500V-CD-SWFT-BNFC(1:6) NOT EQUAL 'BRASUS'
      *               J.P. MORGAN SE
                      MOVE 8500059801       TO S500V-CD-INST-DST
                      MOVE 'CHASUS33'       TO S500V-CD-SWFT-DST
                  END-IF
      *
      *           Qdo for Dolar(220) e inteface APJ o destinatario sera
      *           Bank of America BOFAUS3N
      *            IF S500V-CD-TIP-ITCE-REG EQUAL 15  AND
      *               S500V-CD-MOE          EQUAL 220 AND
      *               S500V-CD-SWFT-BNFC(1:6) NOT EQUAL 'BRASUS'
      *              BANK OF AMERICA N.A.
      *               MOVE 3460059801       TO S500V-CD-INST-DST
      *               MOVE 'BOFAUS3N'       TO S500V-CD-SWFT-DST
      *            END-IF

                  IF S500V-CD-SWFT-BNFC(1:3) NOT EQUAL 'BRAS'
                     PERFORM 805-000-VERF-RGR-ENVO-INST-DST
                  END-IF
              END-IF
           END-IF
      * -- Acao 246721
      * -- Historia 1449105: Enviar a remessa diretamente para a agencia
      * -- BB abaixo, quando ela for o banqueiro do beneficiario:
      * -- BB Miami, BB Nova York, BB Frankfurt, BB Lisboa, BB Londres
      * -- ou BB Toquio
      * >> Quando for interface SISBB, as GECEX's irao informar o
      * >> banqueiro destinatario
           IF  S500V-CD-TIP-ITCE-REG   NOT EQUAL 3
               EVALUATE S500V-CD-SWFT-BNFC
                   WHEN 'BRASUS3M'
                        MOVE 1000054801  TO S500V-CD-INST-DST
                        MOVE 'BRASUS3M'  TO S500V-CD-SWFT-DST
                   WHEN 'BRASUS33'
                        MOVE 1000059801  TO S500V-CD-INST-DST
                        MOVE 'BRASUS33'  TO S500V-CD-SWFT-DST
                   WHEN 'BRASDEFF'
                        MOVE 1000033001  TO S500V-CD-INST-DST
                        MOVE 'BRASDEFF'  TO S500V-CD-SWFT-DST
                   WHEN 'BRASPTPL'
                        MOVE 1000049401  TO S500V-CD-INST-DST
                        MOVE 'BRASPTPL'  TO S500V-CD-SWFT-DST
                   WHEN 'BRASGB2L'
                        MOVE 1000050801  TO S500V-CD-INST-DST
                        MOVE 'BRASGB2L'  TO S500V-CD-SWFT-DST
                   WHEN 'BRASJPJT'
                        MOVE 1000079801  TO S500V-CD-INST-DST
                        MOVE 'BRASJPJT'  TO S500V-CD-SWFT-DST
                   WHEN OTHER
                        CONTINUE
               END-EVALUATE
           END-IF
      *

           IF   S500V-CD-MOE           EQUAL 220
           AND  S500V-CD-PGDR-DSP-EXNO EQUAL 'T'
               MOVE S500V-TX-ITC-ADC-BNF TO W-TX-ITC-ADC-BNF
               MOVE ZEROS                TO W-NR-NDX-72
               PERFORM VARYING W-NR-NDX-72  FROM 01 BY 01
                         UNTIL W-NR-NDX-72  GREATER 05
                    IF W-TX-CMP-72(W-NR-NDX-72) EQUAL '/PAYPRO/'
                       MOVE SPACES TO W-TX-CMP-72(W-NR-NDX-72)
                    END-IF
                    IF W-TX-CMP-72(W-NR-NDX-72) EQUAL '/OURBB/'
                       MOVE 06           TO W-NR-NDX-72
                    ELSE
                       IF W-TX-CMP-72(W-NR-NDX-72) EQUAL SPACES
      *                   IF  S500V-CD-SWFT-DST(1:8) EQUAL 'CITIUS33'
      *                      MOVE '/PAYPRO/' TO W-TX-CMP-72(W-NR-NDX-72)
      *                      MOVE 06         TO W-NR-NDX-72
      *                   END-IF
                          IF  S500V-CD-SWFT-DST(1:6) EQUAL 'BRASUS'
                             MOVE '/OURBB/'  TO W-TX-CMP-72(W-NR-NDX-72)
                             MOVE 06         TO W-NR-NDX-72
                          END-IF
                       END-IF
                    END-IF
               END-PERFORM
               MOVE W-TX-ITC-ADC-BNF    TO S500V-TX-ITC-ADC-BNF
           END-IF

      *--  Valida banqueiro.
           PERFORM 810-000-TRATA-BANQUEIRO.

           IF  S500V-CD-INST-INT NOT EQUAL ZEROS
               PERFORM 820-000-TRATA-INST-INTM
           ELSE
               IF  S500V-CD-CNL          EQUAL 'I'  AND
                   S500V-CD-SWFT-INT NOT EQUAL SPACES
                   PERFORM 710-210-TRATA-CSR-SWFT-INT
               END-IF
           END-IF.



       800-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   800-099-SAIDA.'
                      .
       800-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       805-000-VERF-RGR-ENVO-INST-DST      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 805-000-VERF-RGR-ENVO-INST-DST.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * ENTREGA 1863094
      *
           INITIALIZE L-BCIS204U
                      REPLACING ALPHANUMERIC BY ' '
                      NUMERIC BY  0.
      *
           SET BCIS204U-CONSULTAR TO TRUE

           MOVE S500V-CD-TIP-ITCE-REG  TO BCIS204U-CD-ITCE-CNL-ATDT
           MOVE S500V-CD-MOE           TO BCIS204U-CD-TIP-MOE
           MOVE 'OPE'                  TO BCIS204U-SG-SIS

           MOVE LENGTH OF L-BCIS204U TO EIBCALEN
           CALL BCIS204U USING DFHEIBLK L-BCIS204U

           IF  BCIS204U-CD-RTN-PGM NOT EQUAL 0
           AND BCIS204U-CD-RTN-PGM NOT EQUAL +100
               PERFORM 999236-ERRO-236
           END-IF

           IF BCIS204U-IN-INST-DST-ATI = 'S'
              MOVE BCIS204U-CD-IDFR-IFC      TO S500V-CD-INST-DST
              MOVE BCIS204U-CD-SWFT-INST-DST TO S500V-CD-SWFT-DST
              SET RGR-ENVO-PGTO-ATIVA     TO TRUE
           ELSE
              SET RGR-ENVO-INATIVA        TO TRUE
           END-IF
           .
      *
       850-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   850-099-SAIDA.'
                      .
       850-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       810-000-TRATA-BANQUEIRO       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 810-000-TRATA-BANQUEIRO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           IF  (S500V-CD-INST-DST  EQUAL ZEROS) AND
               (S500V-CD-INST-BNFC EQUAL ZEROS) AND
               S500V-CD-CNL   NOT EQUAL 'I'    AND
               S500V-CD-CNL   NOT EQUAL 'B'
               GO TO 999038-ERRO-038
           ELSE
               IF  (S500V-CD-INST-DST  EQUAL ZEROS) AND
                   (S500V-CD-INST-BNFC EQUAL ZEROS)
                   PERFORM 860-010-TRATA-BANQ-PREFERENC
               END-IF
           END-IF.

           PERFORM 860-000-CHAMA-BDDSH143.
           IF  143-CD-ERRO NOT EQUAL 0
               IF  143-CD-ERRO EQUAL 99
                   MOVE 40 TO W-NR-CMP
                   GO TO 999039-ERRO-039
               ELSE
                   IF  143-CD-ERRO  EQUAL 88 AND
                       (S500V-CD-CNL EQUAL 'I' OR
                       S500V-CD-CNL EQUAL 'B')
                       PERFORM 860-010-TRATA-BANQ-PREFERENC
                       PERFORM 860-000-CHAMA-BDDSH143
                   ELSE
                       IF  143-CD-ERRO EQUAL 89
                           EVALUATE 143-CD-SQCL-CAMPOS
                               WHEN 1 MOVE 41 TO W-NR-CMP
                               WHEN 2 MOVE 43 TO W-NR-CMP
                           END-EVALUATE
                       ELSE
                           EVALUATE 143-CD-SQCL-CAMPOS
                               WHEN 1 MOVE 40 TO W-NR-CMP
                               WHEN 2 MOVE 42 TO W-NR-CMP
                           END-EVALUATE
                       END-IF
                       GO TO 999039-ERRO-039
                   END-IF
               END-IF
           END-IF.

           IF  (S500V-CD-FMA-ETGA-MOE NOT EQUAL 30 ) AND
               (143-IND-CHVE-SWFT-DSTR    EQUAL 'N')
               IF  S500V-CD-CNL EQUAL 'I' OR 'W' OR 'B'
                   PERFORM 860-010-TRATA-BANQ-PREFERENC
                   PERFORM 860-000-CHAMA-BDDSH143
               ELSE
                   IF  S500V-CD-HDNG-CBT NOT EQUAL 0
                       GO TO 999041-ERRO-041
                   ELSE
                       GO TO 999042-ERRO-042
                   END-IF
               END-IF
           END-IF.

           IF  143-CD-SWFT-DSTR EQUAL SPACES
               GO TO 999040-ERRO-040
           END-IF.

       810-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   810-099-SAIDA.'
                      .
       810-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       820-000-TRATA-INST-INTM       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 820-000-TRATA-INST-INTM.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  S500V-CD-INST-INT(11:2) EQUAL 00
               MOVE 01 TO S500V-CD-INST-INT(11:2)
           END-IF.

           INITIALIZE L-BDDE142F
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-CD-INST-INT TO BDDA142C-CD-IDTR
           MOVE S500V-CD-SWFT-INT TO BDDA142C-CD-SWFT

           MOVE LENGTH OF L-BDDE142F TO EIBCALEN
           CALL BDDE142F USING DFHEIBLK L-BDDE142F.

           IF  BDDAMSGS-CD-MSG OF L-BDDE142F NOT EQUAL ZEROS
               GO TO 999053-ERRO-053
           END-IF.

           MOVE BDDA142C-NM       TO S500V-NM-HDNG-INT.
           MOVE BDDA142C-NM-PRAC  TO S500V-NM-PRCA-INT.
           MOVE BDDA142C-CD-SWFT  TO S500V-CD-SWFT-INT.
           MOVE BDDA142C-CD-IDTR  TO S500V-CD-INST-INT.

       820-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   820-099-SAIDA.'
                      .
       820-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
      *822-000-VERIFICA-BANQ-BLQD     SECTION.
      *----------------------------------------------------------------*
      *
      *
      *    IF  S500V-CD-SWFT-BNFC NOT EQUAL SPACES
      *        INITIALIZE L-BDDSBLOQ
      *        MOVE S500V-CD-SWFT-BNFC TO BDDSBLOQ-CD-SWFT-ENT
      *        PERFORM 825-000-CHAMA-BDDSBLOQ
      *    END-IF
      *
      *    IF  S500V-CD-SWFT-DST  NOT EQUAL SPACES
      *        INITIALIZE L-BDDSBLOQ
      *        MOVE S500V-CD-SWFT-DST TO BDDSBLOQ-CD-SWFT-ENT
      *        PERFORM 825-000-CHAMA-BDDSBLOQ
      *    END-IF
      *
      *    IF  S500V-CD-SWFT-CBT  NOT EQUAL SPACES
      *        INITIALIZE L-BDDSBLOQ
      *        MOVE S500V-CD-SWFT-CBT TO BDDSBLOQ-CD-SWFT-ENT
      *        PERFORM 825-000-CHAMA-BDDSBLOQ
      *    END-IF
      *
      *    IF  S500V-CD-SWFT-CRS  NOT EQUAL SPACES
      *        INITIALIZE L-BDDSBLOQ
      *        MOVE S500V-CD-SWFT-CRS TO BDDSBLOQ-CD-SWFT-ENT
      *        PERFORM 825-000-CHAMA-BDDSBLOQ
      *    END-IF
      *
      *    IF  S500V-CD-SWFT-INT  NOT EQUAL SPACES
      *        INITIALIZE L-BDDSBLOQ
      *        MOVE S500V-CD-SWFT-INT TO BDDSBLOQ-CD-SWFT-ENT
      *        PERFORM 825-000-CHAMA-BDDSBLOQ
      *    END-IF
      *
      *    .
      *822-000-FIM.
      *    EXIT.
      *
      *----------------------------------------------------------------*
      *825-000-CHAMA-BDDSBLOQ     SECTION.
      *----------------------------------------------------------------*
      *
      *    MOVE LENGTH OF L-BDDSBLOQ TO EIBCALEN
      *    CALL BDDSBLOQ USING DFHEIBLK L-BDDSBLOQ
      *
      *    IF  BDDSBLOQ-CD-ERRO NOT EQUAL ZEROS
      *        PERFORM 999223-ERRO-223
      *    END-IF
      *
      *    IF  BDDSBLOQ-BANQ-BLQD
      *        PERFORM 999224-ERRO-224
      *    END-IF
      *
      *    .
      *825-000-FIM.
      *    EXIT.
      *----------------------------------------------------------------*
       830-000-VALIDA-DATA           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 830-000-VALIDA-DATA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           IF  W-AA OF W-DT-DMA < 2000
               GO TO 999046-ERRO-046
           END-IF.

           MOVE 'F01' TO F01-FUNCAO.
           MOVE ZEROS TO F01-ARG02.

           CALL SBDATA USING F01-FUNCAO F01-ARG01 F01-ARG02.

           IF  F01-ARG01 EQUAL 99999999
               GO TO 999046-ERRO-046
           END-IF.

           IF  F01-ARG01 EQUAL 88888888
               GO TO 999047-ERRO-047
           END-IF.

       830-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   830-099-SAIDA.'
                      .
       830-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       840-000-VALIDA-NATUREZA       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 840-000-VALIDA-NATUREZA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE S500V-CD-TIP-ORD-PGTO       TO S800V-CD-TIP-ORD-PGTO.
           MOVE S500V-CD-NTZ-FATO-OPRL      TO S800V-CD-NTZ-FATO-OPRL.
           MOVE S500V-CD-CPRD-VNDR-MOE      TO S800V-CD-CPRD-VNDR-MOE.
           MOVE S500V-CD-TIP-AVL-GOV        TO S800V-CD-TIP-AVL-GOV.
           MOVE S500V-CD-PGDR-RCBD-BC       TO S800V-CD-PGDR-RCBD-BC.
           MOVE S500V-CD-GR-OPR-CMB         TO S800V-CD-GR-OPR-CMB.
           MOVE ZEROS                       TO S800V-VCL-PGDR-RCBD.
           MOVE S500V-CD-TIP-MC             TO S800V-CD-IDFR-MC.
           MOVE S500V-CD-TIP-CLI            TO S800V-CD-TIP-CLI.
           MOVE SPACES                      TO S800V-CD-ERRO.
           MOVE S500V-CD-CNL                TO S800V-CD-CNL

           MOVE LENGTH OF L-OPES800V TO EIBCALEN
           CALL OPES800V USING DFHEIBLK L-OPES800V.

           IF  S800V-CD-ERRO NOT EQUAL SPACES
               GO TO 999049-ERRO-049
           END-IF.

           MOVE S800V-IN-OBGD-REG-BC        TO S500V-IN-OBGD-REG-BC.
           MOVE S800V-CD-TIP-CTR-CMB        TO S500V-CD-TIP-CTR-CMB.
      *
           INITIALIZE L-CCAS0080

           MOVE 'OPE'                  TO K0080-SG-SIS
           MOVE S500V-CD-NTZ-FATO-OPRL TO K0080-CD-FATO-OPRL-CMB
           MOVE S500V-CD-CPRD-VNDR-MOE TO K0080-CD-CPRD-VNDR-MOE
           MOVE S500V-CD-TIP-AVL-GOV   TO K0080-CD-AVL-GOV
           MOVE S500V-CD-PGDR-RCBD-BC  TO K0080-CD-TIP-PGDR-RCBD
           MOVE S500V-CD-GR-OPR-CMB    TO K0080-CD-GR-OPRL-CMB
           MOVE S500V-DT-MVT           TO K0080-DT-MVT
           MOVE 'V'                    TO K0080-CD-TIP-OPR
           MOVE S500V-CD-MOE           TO K0080-CD-MOE
           MOVE S500V-VL-MOEE          TO K0080-VL-OPR-MOEE

           MOVE LENGTH OF L-CCAS0080 TO EIBCALEN.
           CALL CCAS0080 USING DFHEIBLK L-CCAS0080.

      * MOCK-POINT MCKV2 K0080-CD-RTN

           IF  K0080-CD-RTN NOT EQUAL ZEROS
               PERFORM 999232-ERRO-232
           END-IF.

           MOVE K0080-CD-TIP-VCL TO S500V-CD-TIP-VCL-ATZD.

       840-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   840-099-SAIDA.'
                      .
       840-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *-----------------------------------------------------------------
       841-000-VALIDAR-CPRD-VNDR                               SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 841-000-VALIDAR-CPRD-VNDR.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------
           INITIALIZE L-BCIS1113

           MOVE S500V-CD-CLI             TO  BCIS1113-CD-CLI-BB
           MOVE 702                      TO  BCIS1113-CD-PRD
           MOVE 003                      TO  BCIS1113-CD-MDLD
           MOVE S500V-CD-TIP-ITCE-REG    TO  BCIS1113-CD-TIP-ITCE
           MOVE LENGTH OF L-BCIS1113     TO  EIBCALEN

           CALL BCIS1113 USING DFHEIBLK L-BCIS1113
      *
           IF BCIS1113-CD-RTN-PGM NOT EQUAL   ZEROS
              PERFORM 999209-ERRO-209
           ELSE
              MOVE 'N' TO W-AUX-REG-OK

              PERFORM VARYING W-IC2 FROM 1 BY 1
                    UNTIL W-IC2 > BCIS1113-QT-OCR-LS OR
                          W-AUX-REG-OK = 'S'

                   IF S500V-CD-CPRD-VNDR-MOE            =
                      BCIS1113-CD-CPRD-VNDR-MOE (W-IC2)

                      MOVE 'S' TO W-AUX-REG-OK
                   END-IF
              END-PERFORM
           END-IF

      * MOCK-POINT MCKV3 W-AUX-REG-OK

           IF W-AUX-REG-OK = 'N'
              PERFORM 999208-ERRO-208
           END-IF
           .
       841-099-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   841-099-SAI.'
                      .
       841-099-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *-----------------------------------------------------------------
       842-000-VERIFICA-NATUREZA                               SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 842-000-VERIFICA-NATUREZA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------
           INITIALIZE L-OPES035R

           MOVE 1                       TO S035R-CD-FUC
           MOVE S500V-CD-TIP-ORD-PGTO   TO S035R-CD-TIP-ORD-PGTO-ENT
           MOVE S500V-CD-TIP-ITCE-REG   TO S035R-CD-TIP-ITCE-ENT
           MOVE S500V-CD-NTZ-FATO-OPRL  TO S035R-CD-NTZ-FATO-OPRL-ENT
           MOVE S500V-CD-CPRD-VNDR-MOE  TO S035R-CD-CPRD-VNDR-MOE-ENT
           MOVE S500V-CD-TIP-AVL-GOV    TO S035R-CD-TIP-AVL-GOV-ENT
           MOVE S500V-CD-PGDR-RCBD-BC   TO S035R-CD-PGDR-RCBD-BC-ENT
           MOVE S500V-CD-GR-OPR-CMB     TO S035R-CD-GR-OPR-CMB-ENT

           MOVE LENGTH OF L-OPES035R TO EIBCALEN
           CALL OPES035R USING DFHEIBLK L-OPES035R
      *
           IF   S035R-CD-RTN NOT EQUAL ZEROS
            AND S035R-CD-SQLCODE-RTN NOT EQUAL +100
              PERFORM 999210-ERRO-210
           END-IF
           IF S035R-CD-SQLCODE-RTN EQUAL +100
              PERFORM 999211-ERRO-211
           END-IF
           .
       842-099-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   842-099-SAI.'
                      .
       842-099-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       845-000-VALIDA-DSP-EXNO       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 845-000-VALIDA-DSP-EXNO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      *    Somente despesa por conta do Tomador.
      *    Beneficiario BB Americas nao cobra despesa externa
           IF  S500V-CD-CVN-ORD-PGTO EQUAL ZEROS
               IF  S500V-CD-PGDR-DSP-EXNO     EQUAL 'T'
      *    AND S500V-CD-INST-BNFC     NOT EQUAL 125970127101
                   IF  S500V-CD-INST-DST-ANT NOT EQUAL S500V-CD-INST-DST
      *                INITIALIZE L-BCIS921S
      *                    REPLACING NUMERIC BY  0
      *                    ALPHANUMERIC BY ' '
      *
      * >> Acao 246721 - Historia 1449105
      * >> Para as interfaces abaixo, buscar a despesa externa cadastra-
      * >> da para o banqueiro PADRAO (999999999999).
      *                IF  S500V-CD-TIP-ITCE-REG EQUAL 12
      *                OR  S500V-CD-TIP-ITCE-REG EQUAL 15
      *                OR  S500V-CD-TIP-ITCE-REG EQUAL 20
      *                OR  S500V-CD-TIP-ITCE-REG EQUAL 55
      *                    MOVE 999999999999       TO W-CD-INST-IDFR
      *                ELSE
      *                    MOVE S500V-CD-INST-DST  TO W-CD-INST-IDFR
      *                END-IF
      *                MOVE S500V-CD-MOE           TO W-CD-MOE
      *
      *                PERFORM 847-000-CHAMA-BCIS921S
      *
      *                IF  BCIS921S-CD-RTN-PGM EQUAL 100
      *            Caso nao seja localizada a despesa para a moeda
      *            e banqueiro, busca valor generico em USD
      *                    MOVE 220          TO W-CD-MOE
      *                    PERFORM 847-000-CHAMA-BCIS921S
      *                END-IF
      *
      *                IF  BCIS921S-CD-RTN-PGM NOT EQUAL ZEROS
      *                    GO TO 999158-ERRO-158
      *                END-IF
      *
      *                IF  BCIS921S-VL-DSP-BANQ-RCBD NOT EQUAL ZEROS
      *                    MOVE BCIS921S-VL-DSP-BANQ-RCBD
      *                        TO W-VL-DSP-EXNO-MOEE
      *                ELSE
      *                    MOVE BCIS921S-VL-DSP-BANQ-RCBD-PDRO
      *                        TO W-VL-DSP-EXNO-MOEE
      *                END-IF
      *
      *                IF  S500V-CD-MOE NOT EQUAL W-CD-MOE
      *            Valor recuperado e em USD, realizar paridade
      *                    PERFORM 849-000-PARIDADE
      *                END-IF
      *
      *                MOVE W-VL-DSP-EXNO-MOEE TO S500V-VL-DSP-EXNO-MOEE
      *
                       INITIALIZE L-BCIS921T
                           REPLACING NUMERIC BY  0
                           ALPHANUMERIC BY ' '
      *
                       MOVE 1                TO BCIS921T-ACAO
                       MOVE S500V-CD-MOE     TO BCIS921T-CD-MOE-DSP-EXNO
                       MOVE S500V-CD-TIP-MC  TO BCIS921T-CD-TIP-MC
                       IF  S500V-CD-TIP-ITCE-REG EQUAL 12
                       OR  S500V-CD-TIP-ITCE-REG EQUAL 15
                       OR  S500V-CD-TIP-ITCE-REG EQUAL 20
                       OR  S500V-CD-TIP-ITCE-REG EQUAL 55
                           IF  S500V-CD-TIP-ITCE-REG EQUAL 15
                           AND W-CD-MOE              EQUAL 220
      *    UTILIZAR TOTAL PROTECT PARA O CANAL 15
                               MOVE 888888888888 TO BCIS921T-CD-IDFR-IFC
                           ELSE
                               MOVE 999999999999 TO BCIS921T-CD-IDFR-IFC
                           END-IF
                       ELSE
                           MOVE S500V-CD-INST-DST
                                                 TO BCIS921T-CD-IDFR-IFC
                       END-IF
      *
                       MOVE LENGTH OF L-BCIS921T TO EIBCALEN
                       CALL BCIS921T USING DFHEIBLK L-BCIS921T
      *
                       IF  BCIS921T-CD-RTN-PGM NOT EQUAL ZEROS
                           GO TO 999158-ERRO-158
                       END-IF
      *
                       MOVE BCIS921T-VL-DSP-BANQ-RCBD
                                               TO S500V-VL-DSP-EXNO-MOEE
                   END-IF
               ELSE
                   MOVE ZEROS TO S500V-VL-DSP-EXNO-MOEE
                       S500V-VL-DSP-EXNO-MOEN
               END-IF
           END-IF.
      *
           COMPUTE S500V-VL-DSP-EXNO-MOEN = S500V-VL-DSP-EXNO-MOEE *
               S500V-VL-TAXA-CMB
           ON SIZE ERROR
              PERFORM 999201-ERRO-201
           END-COMPUTE
           .
       845-099-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   845-099-FIM.'
                      .
       845-099-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *----------------------------------------------------------------*
       846-000-VLD-RMTE-DFRT-BNFC SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 846-000-VLD-RMTE-DFRT-BNFC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *    Verifica se a natureza exige que o tomador/remetente seja
      *    diferente do beneficiário.
      *    Uso da funcionalidade Regras de Validação - CAMBIO 96.
           INITIALIZE L-BCIS4000
           MOVE 'OPE'                  TO BCIS4000-SG-SIS
           MOVE 'NTZ-RMTE-DFRT-BNFC'   TO BCIS4000-SG-RGR
           PERFORM 980-000-RECUPERA-DT-HR-ATUAL
           MOVE L-DT-ATU(01:02)        TO BCIS4000-DT-MVT(01:02)
           MOVE '.'                    TO BCIS4000-DT-MVT(03:01)
           MOVE L-DT-ATU(03:02)        TO BCIS4000-DT-MVT(04:02)
           MOVE '.'                    TO BCIS4000-DT-MVT(06:01)
           MOVE L-DT-ATU(05:04)        TO BCIS4000-DT-MVT(07:04)
           MOVE 'OPES500V'             TO BCIS4000-NM-PGM-CHMR
           MOVE 'NTZ-FATO-OPRL'        TO BCIS4000-SG-CRIT-ENT(01)
           MOVE S500V-CD-NTZ-FATO-OPRL TO BCIS4000-TX-CRIT-ENT(01)
           PERFORM 930-000-CHAMA-BCIS4000
           IF BCIS4000-RGR-OK
      *       Por ora, a validação avaliará se os nomes são exatamente
      *       iguais. Conforme o texto da Ação 227668.
              IF S500V-CD-TIP-ORD-PGTO EQUAL 03
      *          Ordem Western Union
                 STRING S500V-NM-BNFC S500V-ULT-NM-BNFC
                    DELIMITED BY SIZE INTO TX-ENTD
                 END-STRING
                 PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                 MOVE TX-SAID TO NM-BNFC-SEM-ESP-DUPL

                 MOVE S500V-NM-TMDR TO TX-ENTD
                 PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                 MOVE TX-SAID TO NM-TMDR-SEM-ESP-DUPL
              ELSE
      *          Demais Ordens
                 IF S500V-CD-CNL EQUAL 'I' OR S500V-CD-CNL EQUAL ' '
                    MOVE S500V-NM-BNFC(1:35) TO TX-ENTD
                    PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                    MOVE TX-SAID TO NM-BNFC-SEM-ESP-DUPL

                    MOVE S500V-NM-TMDR(1:35) TO TX-ENTD
                    PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                    MOVE TX-SAID TO NM-TMDR-SEM-ESP-DUPL
                 ELSE
                    MOVE S500V-NM-BNFC TO TX-ENTD
                    PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                    MOVE TX-SAID TO NM-BNFC-SEM-ESP-DUPL

                    MOVE S500V-NM-TMDR TO TX-ENTD
                    PERFORM 846-100-REMOVER-ESPACOS-DUPLOS
                    MOVE TX-SAID TO NM-TMDR-SEM-ESP-DUPL
                 END-IF
              END-IF
              IF NM-BNFC-SEM-ESP-DUPL EQUAL NM-TMDR-SEM-ESP-DUPL
                 PERFORM 999230-ERRO-230
              END-IF
           END-IF
           .
       846-099-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   846-099-SAI.'
                      .
       846-099-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       846-100-REMOVER-ESPACOS-DUPLOS SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 846-100-REMOVER-ESPACOS-DUPLOS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           MOVE ZEROS TO IC-DST
           MOVE SPACE TO TX-SAID
           PERFORM VARYING IC-OGM FROM 01 BY 01
                   UNTIL IC-OGM > LENGTH OF TX-ENTD
              IF TX-ENTD(IC-OGM:01) EQUAL SPACES
                 IF IC-DST > ZERO
                    IF IC-OGM < LENGTH OF TX-ENTD
                       IF TX-ENTD(IC-OGM + 01:01) NOT EQUAL SPACES
                          ADD 01 TO IC-DST
                          MOVE TX-ENTD(IC-OGM:01)
                             TO TX-SAID(IC-DST:01)
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 ADD 01 TO IC-DST
                 MOVE TX-ENTD(IC-OGM:01) TO TX-SAID(IC-DST:01)
              END-IF
           END-PERFORM
           .
       846-199-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   846-199-SAI.'
                      .
       846-199-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *-----------------------------------------------------------------
      *847-000-CHAMA-BCIS921S        SECTION.
      *----------------------------------------------------------------*
      *
      *    INITIALIZE L-BCIS921S
      *        REPLACING NUMERIC BY  0
      *        ALPHANUMERIC BY ' '
      *
      *    MOVE 01                   TO BCIS921S-ACAO
      *    MOVE 04                   TO BCIS921S-CD-NVL-EXEA-ACAO
      *    MOVE W-CD-MOE             TO BCIS921S-CD-MOE-DSP-EXNO
      *
      *    IF  S500V-CD-TIP-ITCE-REG EQUAL 15 AND
      *        W-CD-MOE              EQUAL 220
      *    UTILIZAR TOTAL PROTECT PARA O CANAL 15
      *        MOVE 888888888888     TO BCIS921S-CD-IDFR-IFC
      *    ELSE
      *        MOVE W-CD-INST-IDFR   TO BCIS921S-CD-IDFR-IFC
      *    END-IF
      *
      *    MOVE LENGTH OF L-BCIS921S TO EIBCALEN
      *    CALL BCIS921S USING DFHEIBLK L-BCIS921S.
      *
      *847-099-FIM.
      *    EXIT.
      *
      * ----------------------------------------------------------------
      *849-000-PARIDADE              SECTION.
      * ----------------------------------------------------------------
      *
      *    INITIALIZE  L-BCIS218R
      *        REPLACING ALPHANUMERIC BY SPACES
      *        NUMERIC BY ZEROS.
      *
      *    MOVE 2                       TO K218R-CD-FUC
      *    MOVE S500V-CD-MOE            TO K218R-COD-MOED
      *    MOVE S500V-CD-TIP-MC         TO K218R-COD-TIPO-MERC.
      *
      *    MOVE LENGTH OF L-BCIS218R TO EIBCALEN
      *    CALL BCIS218R USING DFHEIBLK L-BCIS218R.
      *
      *    IF  K218R-CD-RTN NOT EQUAL ZEROS
      *        GO TO 999159-ERRO-159
      *    END-IF.
      *
      *    IF  K218R-IND-MULT-DIV EQUAL 1
      *        COMPUTE W-VL-DSP-EXNO-MOEE = W-VL-DSP-EXNO-MOEE *
      *            K218R-TAXA-PRDE-VNDA
      *        ON SIZE ERROR
      *           PERFORM 999202-ERRO-202
      *        END-COMPUTE
      *    ELSE
      *        COMPUTE W-VL-DSP-EXNO-MOEE = W-VL-DSP-EXNO-MOEE /
      *            K218R-TAXA-PRDE-VNDA
      *        ON SIZE ERROR
      *           PERFORM 999203-ERRO-203
      *        END-COMPUTE
      *    END-IF.
      *
      *849-099-FIM.
      *    EXIT.
      *
      *----------------------------------------------------------------*
       850-000-TRATA-DEPE            SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 850-000-TRATA-DEPE.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE LENGTH OF L-OPES0820 TO EIBCALEN
           CALL OPES0820 USING DFHEIBLK L-OPES0820.

           IF  K0820-CD-RTN NOT EQUAL ZEROS
               GO TO 999044-ERRO-044
           END-IF.

       850-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   850-099-SAIDA.'
                      .
       850-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       860-000-CHAMA-BDDSH143        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 860-000-CHAMA-BDDSH143.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BDDSH143 BDDA143B
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0

      *    Qdo for Euro(978) o destinatário será DEUTSCHE BANK: DEUTDEFF
      *    Excecao: Se pais destinatario for Cuba ou tomador da ordem PF
      *    for nascido em Cuba ou com nacionalidade Cubana, o banqueiro
      *    destinatario sera BB Frankfurt
           IF  S500V-CD-MOE EQUAL 978
               AND S500V-CD-INST-DST EQUAL ZEROS
               IF  S500V-CD-TIP-CLI EQUAL 1
                   PERFORM 212-000-VERIFICA-CUBANO
               END-IF
               IF  S500V-CD-PAIS-BNFC EQUAL 199
                OR W-IN-PAIS-NSC-CUBA EQUAL 'S'
                   MOVE 01000033001        TO S500V-CD-INST-DST
               ELSE
      *    Quando interface SISBB o CENOP informa banqueiro destinatario
      *    Autoatendimento informar DEUTDEFF
                   IF S500V-CD-TIP-ITCE-REG NOT EQUAL 3
                      MOVE 005560033001    TO S500V-CD-INST-DST
                   END-IF
               END-IF
           END-IF
      *
      * >> Qdo moeda for franco suico e interface de autoatendimento
      *    direcionar banqueiro para ZKBKCHZZ80A ZURCHER KANTONALBANK
           IF  S500V-CD-MOE EQUAL 425
           AND S500V-CD-TIP-ITCE-REG NOT EQUAL 3
               MOVE 055680088201            TO S500V-CD-INST-DST
           END-IF
      *
           IF  S500V-CD-INST-DST EQUAL ZEROS
               MOVE S500V-CD-INST-BNFC      TO 143-CD-INTT-FNCR-DSTR
               MOVE S500V-CD-INST-BNFC(1:5) TO 143-CD-HOLDING-DSTR
               MOVE S500V-CD-INST-BNFC(6:5) TO 143-CD-PRACA-DSTR
           ELSE
               MOVE S500V-CD-INST-DST       TO 143-CD-INTT-FNCR-DSTR
               MOVE S500V-CD-HDNG-DST       TO 143-CD-HOLDING-DSTR
               MOVE S500V-CD-PRCA-DST       TO 143-CD-PRACA-DSTR
           END-IF

           IF  143-CD-INTT-FNCR-DSTR(11:2) EQUAL 00
               MOVE 01 TO 143-CD-INTT-FNCR-DSTR(11:2)
           END-IF

           MOVE S500V-CD-SWFT-DST  TO 143-CD-SWFT-DSTR
           MOVE S500V-CD-INST-CBT  TO 143-CD-INTT-FNCR-CBT
           MOVE S500V-CD-HDNG-CBT  TO 143-CD-HOLDING-CBT
           MOVE S500V-CD-PRCA-CBT  TO 143-CD-PRACA-CBT
           MOVE S500V-CD-SWFT-CBT  TO 143-CD-SWFT-CBT
           MOVE S500V-CD-MOE       TO 143-CD-MOE

           MOVE LENGTH OF L-BDDSH143 TO EIBCALEN
           CALL BDDSH143 USING DFHEIBLK L-BDDSH143

           IF  EIBRESP NOT EQUAL DFHRESP( NORMAL )
               MOVE 3                  TO S500V-CD-TLA
               GO TO 999051-ERRO-051
           END-IF

      * >> Quando não houverem parametros cadastrados no menu OPE.88
           IF RGR-ENVO-INATIVA
              MOVE 143-NM-HOLDING-DSTR    TO S500V-NM-HDNG-DST
              MOVE 143-NM-PRACA-DSTR      TO S500V-NM-PRCA-DST
              MOVE 143-CD-SWFT-DSTR       TO S500V-CD-SWFT-DST
              MOVE 143-CD-INTT-FNCR-DSTR  TO S500V-CD-INST-DST
           END-IF

      *    Qdo for Euro(978) não será necessário cobertura
      *    Qdo for Dolar(220), interface nao for SISBB e banqueiro for
      *    Citibank, BB Nova Iorque ou BB Miami nao precisa cobertura
           IF  (S500V-CD-MOE EQUAL 978
               AND (S500V-CD-INST-DST EQUAL 075730033001
                 OR S500V-CD-INST-DST EQUAL 128660033001
                 OR S500V-CD-INST-DST EQUAL 01000033001))
               OR (S500V-CD-MOE EQUAL 220
               AND S500V-CD-TIP-ITCE-REG NOT EQUAL 3
               AND (S500V-CD-INST-DST EQUAL 06010059801
                 OR S500V-CD-INST-DST EQUAL 01000054801
                 OR S500V-CD-INST-DST EQUAL 01000059801))
               MOVE SPACES  TO S500V-NM-HDNG-CBT
               MOVE SPACES  TO S500V-NM-PRCA-CBT
               MOVE SPACES  TO S500V-CD-SWFT-CBT
               MOVE ZEROS   TO S500V-CD-INST-CBT
      *    Nos casos de Dolar(220) tambem nao serah necessario informar
      *    banqueiro correspondente
               IF S500V-CD-MOE EQUAL 220
                  MOVE SPACES  TO S500V-NM-HDNG-CRS
                  MOVE SPACES  TO S500V-NM-PRCA-CRS
                  MOVE SPACES  TO S500V-CD-SWFT-CRS
                  MOVE ZEROS   TO S500V-CD-INST-CRS
               END-IF
           ELSE
               MOVE 143-NM-HOLDING-CBT     TO S500V-NM-HDNG-CBT
               MOVE 143-NM-PRACA-CBT       TO S500V-NM-PRCA-CBT
               MOVE 143-CD-SWFT-CBT        TO S500V-CD-SWFT-CBT
               MOVE 143-CD-INTT-FNCR-CBT   TO S500V-CD-INST-CBT
               MOVE 143-NM-HOLDING-INTER   TO S500V-NM-HDNG-CRS
               MOVE 143-NM-PRACA-INTER     TO S500V-NM-PRCA-CRS
               MOVE 143-CD-SWFT-INTER      TO S500V-CD-SWFT-CRS
               MOVE 143-CD-INTT-FNCR-INTER TO S500V-CD-INST-CRS

               MOVE 143-CD-INTT-FNCR-CBT TO BDDA143B-F-INTTFNCR-COBTURA
               MOVE S500V-CD-INST-BNFC   TO BDDA143B-F-INTTFNCR-COBERTO
               MOVE S500V-CD-MOE         TO BDDA143B-F-TABMOEDA-COD
               MOVE 'O'                  TO BDDA143B-COD-ACAO

               MOVE LENGTH OF BDDA143B-PRM TO EIBCALEN
               CALL BDDE143H USING DFHEIBLK BDDA143B-PRM

               IF  EIBRESP NOT EQUAL DFHRESP( NORMAL )
                   MOVE 3                  TO S500V-CD-TLA
                   GO TO 999051-ERRO-051
               END-IF
           END-IF

           IF  BDDA143B-F-INTTFNCR-COBTURA NOT EQUAL ZEROS
           AND S500V-CD-TLA                    EQUAL 2

               INITIALIZE L-BDDE142F
                   REPLACING ALPHANUMERIC BY ' '
                   NUMERIC BY  0

               MOVE BDDA143B-F-INTTFNCR-COBTURA TO BDDA142C-CD-IDTR

               MOVE LENGTH OF L-BDDE142F TO EIBCALEN
               CALL BDDE142F USING DFHEIBLK L-BDDE142F

               IF  BDDAMSGS-CD-MSG OF L-BDDE142F NOT EQUAL ZEROS
                   GO TO 999053-ERRO-053
               END-IF

               MOVE BDDA142C-NM      TO S500V-NM-HDNG-CBT
               MOVE BDDA142C-NM-PRAC TO S500V-NM-PRCA-CBT
               MOVE BDDA142C-CD-SWFT TO S500V-CD-SWFT-CBT
               MOVE BDDA142C-CD-IDTR TO S500V-CD-INST-CBT

               IF  BDDA143B-F-ITTFNCR-INTRMDR > ZEROS
                   INITIALIZE L-BDDE142F
                       REPLACING ALPHANUMERIC BY ' '
                       NUMERIC BY  0

                   MOVE BDDA143B-F-ITTFNCR-INTRMDR TO BDDA142C-CD-IDTR

                   MOVE LENGTH OF L-BDDE142F TO EIBCALEN
                   CALL BDDE142F USING DFHEIBLK L-BDDE142F

                   IF  BDDAMSGS-CD-MSG OF L-BDDE142F NOT EQUAL ZEROS
                       GO TO 999053-ERRO-053
                   END-IF

                   MOVE BDDA142C-NM      TO S500V-NM-HDNG-CRS
                   MOVE BDDA142C-NM-PRAC TO S500V-NM-PRCA-CRS
                   MOVE BDDA142C-CD-SWFT TO S500V-CD-SWFT-CRS
                   MOVE BDDA142C-CD-IDTR TO S500V-CD-INST-CRS
               END-IF
           END-IF
           .
       860-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   860-099-SAIDA.'
                      .
       860-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       860-010-TRATA-BANQ-PREFERENC  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 860-010-TRATA-BANQ-PREFERENC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * Banqueiro Destinatário : A partir do país do favorecido, deverá
      * ----------------------   ser recuperado o banqueiro do tipo AG:
      * Agência ou SB: Subsidiária. No caso da existência de mais de um
      * banqueiro para a pesquisa, deverá ser definido como destinatário
      * o banqueiro Preferencial. No entanto, quando não existir banquei
      * ros do tipo AG ou SB no país, deverá ser definido como destina-
      * tário o banqueiro caixa da moeda.
      *
      * Banqueiro Preferencial: Banqueiro cadastrado no sistema BCI a
      * ----------------------  ser utilizado quando da existência de
      * mais de um banqueiro, tipo AG ou SB, localizado em um país.

      * >> Quando não houverem parametros cadastrados no menu OPE.88
           IF RGR-ENVO-INATIVA
              MOVE ZEROS              TO S500V-CD-INST-DST
              MOVE S500V-CD-PAIS-BNFC TO W-CD-PAIS-PSQ

              INITIALIZE W-QT-REG W-INST-FNCR
                  REPLACING ALPHANUMERIC BY ' '
                  NUMERIC BY  0

              MOVE 'AG' TO W-CD-SB-ARG
              PERFORM 710-700-FETCH-CSR-DSTR

              PERFORM VARYING W-IC FROM 1 BY 1
                              UNTIL W-IC > BDDA142B-QT-REG
                 ADD 1 TO W-QT-REG
                 MOVE BDDA142B-CD-IDFR     (W-IC) TO W-CD-INST(W-QT-REG)
                 MOVE BDDA142B-NM-INST-FNCR(W-IC) TO W-NM-HDNG(W-QT-REG)
                 MOVE BDDA142B-NM-PRAC     (W-IC) TO W-NM-PRCA(W-QT-REG)
                 MOVE BDDA142B-CD-SWFT     (W-IC) TO W-CD-SWFT(W-QT-REG)
              END-PERFORM

              MOVE 'SB' TO W-CD-SB-ARG
              PERFORM 710-700-FETCH-CSR-DSTR

              PERFORM VARYING W-IC FROM 1 BY 1
                              UNTIL W-IC > BDDA142B-QT-REG
                 ADD 1 TO W-QT-REG
                 MOVE BDDA142B-CD-IDFR     (W-IC) TO W-CD-INST(W-QT-REG)
                 MOVE BDDA142B-NM-INST-FNCR(W-IC) TO W-NM-HDNG(W-QT-REG)
                 MOVE BDDA142B-NM-PRAC     (W-IC) TO W-NM-PRCA(W-QT-REG)
                 MOVE BDDA142B-CD-SWFT     (W-IC) TO W-CD-SWFT(W-QT-REG)
              END-PERFORM

              PERFORM VARYING W-NR-NDX FROM 1 BY 1
                 UNTIL W-NR-NDX > W-QT-REG
                 MOVE W-CD-INST (W-NR-NDX) TO S500V-CD-INST-DST
                 MOVE W-NM-HDNG (W-NR-NDX) TO S500V-NM-HDNG-DST
                 MOVE W-NM-PRCA (W-NR-NDX) TO S500V-NM-PRCA-DST
                 MOVE W-CD-SWFT (W-NR-NDX) TO S500V-CD-SWFT-DST
      *
                 IF  W-NR-NDX GREATER 1
      *              Carrega banqueiro preferencial
                     PERFORM 860-020-CARREGA-BANQ-PFRL
                 END-IF
              END-PERFORM

      * > Quando não existir banqueiros do tipo AG ou SB no país, deverá
      * ser definido como destinatário o BANQUEIRO CAIXA DA MOEDA.
      * > Quando interface não for SISBB, assume Banqueiro Caixa Moeda
              IF  S500V-CD-INST-DST EQUAL ZEROS
              OR  S500V-CD-TIP-ITCE-REG NOT EQUAL 3
                  MOVE W-CD-IFC-CAIXA TO S500V-CD-INST-DST
              END-IF
           END-IF
           .
      *
       860-019-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   860-019-SAIDA.'
                      .
       860-019-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       860-020-CARREGA-BANQ-PFRL     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 860-020-CARREGA-BANQ-PFRL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      * Banqueiro Preferencial: Banqueiro cadastrado no sistema BCI a
      * ----------------------  ser utilizado quando da existência de
      * mais de um banqueiro, tipo AG ou SB, localizado em um país.
      *
           INITIALIZE L-BCIS094L S500V-CD-INST-DST
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-CD-PAIS-BNFC  TO BCIS094L-CD-PAIS.

           MOVE LENGTH OF L-BCIS094L TO EIBCALEN
           CALL BCIS094L USING DFHEIBLK L-BCIS094L.

           IF  BCIS094L-CD-RTN-PGM NOT EQUAL ZEROS
               PERFORM 999130-ERRO-130
           END-IF.

           PERFORM VARYING W-NR-NDX-PFRL FROM 01 BY 01
               UNTIL W-NR-NDX-PFRL GREATER 300
               OR BCIS094L-CD-IDFR-IFC(W-NR-NDX-PFRL) EQUAL ZEROS
               OR S500V-CD-INST-DST               NOT EQUAL ZEROS

               IF  BCIS094L-CD-PGDR-RCBD(W-NR-NDX-PFRL) EQUAL ZEROS OR
                   BCIS094L-CD-PGDR-RCBD(W-NR-NDX-PFRL) EQUAL
                   S500V-CD-PGDR-RCBD-BC
                   MOVE BCIS094L-CD-IDFR-IFC(W-NR-NDX-PFRL)
                       TO S500V-CD-INST-DST
               END-IF

           END-PERFORM.
      *
       860-029-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   860-029-SAIDA.'
                      .
       860-029-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       871-000-CHAMA-OPRS5900        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 871-000-CHAMA-OPRS5900.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           MOVE SPACES                 TO W5900-DADOS-CHAMADA
           MOVE S500V-CD-PRF-DEPE-OPR  TO W5900-CD-PRF-DEPE-CDU-E
           MOVE S500V-NR-CC-TMDR       TO W5900-NR-CTR-OPR-E
           MOVE 3                      TO W5900-FUNCAO
           MOVE 'OPES500V'             TO W5900-PRG-CHAMADA
           MOVE 'O'                    TO W5900-AMB-CHAMADA
           MOVE ZEROS                  TO W5900-NR-OCORR-CHAMADA-E
           MOVE 'S'                    TO W5900-CD-PSS-CTR-OPR-IND
           MOVE 'S'                    TO W5900-CD-PRD-IND
           MOVE 'S'                    TO W5900-NR-SEQL-PRTC-IND
           MOVE 'S'                    TO W5900-NR-CPF-CGC-IND
           MOVE 'S'                    TO W5900-CD-TIP-CPF-CGC-IND
           MOVE 1                      TO W5900-CD-TIP-PRTC-E
      ***  MOVE 1                      TO W5900-CD-EST-PRTC-E
           MOVE 'S'                    TO W5900-CD-EST-PRTC-IND
           MOVE 'S'                    TO W5900-CD-PRF-DEPE-CDU-IND
           MOVE 'S'                    TO W5900-NR-CTR-OPR-IND
           MOVE 'S'                    TO W5900-CD-MDLD-IND

           MOVE W-CD-PRD-OPRS5900      TO W5900-CD-PRD-E

      ***  EVALUATE S500V-CD-FMA-INTM
      ***     WHEN 1
      ***        Conta corrente
      ***        MOVE 006   TO W5900-CD-PRD-E
      ***     WHEN 6
      ***        Poupança/Poupex
      ***        MOVE 003   TO W5900-CD-PRD-E
      ***        MOVE 644   TO W5900-CD-PRD-M1-E
      ***     WHEN OTHER
      ***         GO TO 999006-ERRO-006
      ***  END-EVALUATE
      *
      ***  Instituição 1 - BB, se não localizar a conta
      ***  será feita nova consulta com instituição 151 - BNC
      *
           MOVE 1                      TO W5900-CD-INST-ORGC-E
               S500V-CD-INST-ORGC
      *
           MOVE LENGTH OF L-OPRS5900 TO EIBCALEN
           CALL OPRS5900 USING DFHEIBLK L-OPRS5900.
      *
      ***     Na chamada acima, passou-se a buscar ag/ct com todos os
      ***     estados. As contas espelho BB/BNC estão bloqueadas, o
      ***     bloqueio deve ser validado no EIF.
      *
           IF  W5900-COD-RETORNO EQUAL ZEROS
               PERFORM VARYING W-NR-NDX-OPR  FROM 1 BY 1
                   UNTIL W-NR-NDX-OPR  EQUAL 20
                   OR W-IN-CT-VLDO
                   IF  W5900-CD-PSS-CTR-OPR-S03(W-NR-NDX-OPR)
                       EQUAL S500V-CD-CLI
                       AND W5900-CD-PRF-DEPE-CDU-S03(W-NR-NDX-OPR)
                       EQUAL S500V-CD-PRF-DEPE-OPR
                       AND W5900-NR-CTR-OPR-S03(W-NR-NDX-OPR)
                       EQUAL S500V-NR-CC-TMDR
                       MOVE 1 TO W-CD-CT-VLDO
      *              Conta Bloqueada
                       IF  W5900-CD-EST-PRTC-S03 (W-NR-NDX-OPR) EQUAL 6
                           PERFORM 875-000-VALIDA-STATUS-EIF
      *                 Erro 14 Registro não encontrado. CT nao espelho
                           IF  EIFSB018-COD-RET EQUAL 14
      *                 Conta incorporada antes da data do movimento
                               OR  W-IN-CT-MIGD EQUAL 0
      *                     Nestes casos deve dar o erro de CT bloqueada
                               PERFORM 999140-ERRO-140
                           END-IF
                       ELSE
      *                 Contas não ativas
                           IF  W5900-CD-EST-PRTC-S03 (W-NR-NDX-OPR)
                               NOT EQUAL 1
                               PERFORM 999141-ERRO-141
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.
      *    Retorna estado de validacao para 'Nao validado'
           MOVE 0 TO W-CD-CT-VLDO.
      *
      ***     Se não encontrar a conta no BB, pesquisa se a
      ***     conta existe na BNC e recupera conta correspondente BB.
      *

           IF  W5900-COD-RETORNO NOT EQUAL ZEROS
           OR  W5900-QUANTIDADE      EQUAL ZEROS
               PERFORM 872-000-TRATA-CTA-BNC
           END-IF
           .
       871-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   871-099-SAIDA.'
                      .
       871-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       872-000-TRATA-CTA-BNC         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 872-000-TRATA-CTA-BNC.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
      *
      ***  Se não encontrar a conta no BB, pesquisa se a
      ***  conta é do BNC e recupera a conta correspondente no BB.
      ***  +001 - 'PESQUISA EFETUADA SEM ENCONTRAR REGISTROS'

           IF  W5900-COD-RETORNO = 1

               MOVE SPACES                 TO W5900-DADOS-CHAMADA
               MOVE S500V-CD-PRF-DEPE-OPR  TO W5900-CD-PRF-DEPE-CDU-E
               MOVE S500V-NR-CC-TMDR       TO W5900-NR-CTR-OPR-E
               MOVE 3                      TO W5900-FUNCAO
               MOVE 'OPES500V'             TO W5900-PRG-CHAMADA
               MOVE 'O'                    TO W5900-AMB-CHAMADA
               MOVE  ZEROS                 TO W5900-NR-OCORR-CHAMADA-E
               MOVE 'S'                    TO W5900-CD-PSS-CTR-OPR-IND
               MOVE 'S'                    TO W5900-CD-PRD-IND
               MOVE 'S'                    TO W5900-NR-SEQL-PRTC-IND
               MOVE 'S'                    TO W5900-NR-CPF-CGC-IND
               MOVE 'S'                    TO W5900-CD-TIP-CPF-CGC-IND
               MOVE 1                      TO W5900-CD-TIP-PRTC-E
      ***     MOVE 1                      TO W5900-CD-EST-PRTC-E
      ***     MOVE 'S'                    TO W5900-CD-EST-PRTC-IND
               MOVE 'S'                    TO W5900-CD-PRF-DEPE-CDU-IND
               MOVE 'S'                    TO W5900-NR-CTR-OPR-IND
               MOVE 'S'                    TO W5900-CD-MDLD-IND

               MOVE W-CD-PRD-OPRS5900      TO W5900-CD-PRD-E

      ***     EVALUATE S500V-CD-FMA-INTM
      ***        WHEN 1
      ***           Conta corrente
      ***           MOVE 006   TO W5900-CD-PRD-E
      ***        WHEN 6
      ***           Poupança/Poupex
      ***           MOVE 003   TO W5900-CD-PRD-E
      ***           MOVE 644   TO W5900-CD-PRD-M1-E
      ***        WHEN OTHER
      ***            GO TO 999006-ERRO-006
      ***     END-EVALUATE
      *
      ***     Instituição 151 - BNC
      *
               MOVE 151                    TO W5900-CD-INST-ORGC-E
                   S500V-CD-INST-ORGC
      *
               MOVE LENGTH OF L-OPRS5900 TO EIBCALEN
               CALL OPRS5900 USING DFHEIBLK L-OPRS5900

               IF  W5900-COD-RETORNO EQUAL ZEROS
                   PERFORM VARYING W-NR-NDX-OPR  FROM 1 BY 1
                       UNTIL W-NR-NDX-OPR  EQUAL 20
                       OR W-IN-CT-VLDO
                       IF  W5900-CD-PSS-CTR-OPR-S03(W-NR-NDX-OPR)
                           EQUAL S500V-CD-CLI
                           MOVE W5900-CD-PRF-DEPE-CDU-S03(W-NR-NDX-OPR)
                               TO   S500V-CD-PRF-DEPE-OPR
                           MOVE W5900-NR-CTR-OPR-S03(W-NR-NDX-OPR)
                               TO   S500V-NR-CC-TMDR
                           MOVE 1 TO W-CD-CT-VLDO
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      ***  Se não localizar a conta no BNC
      ***  será feita nova consulta com instituição 1 - BB
      ***  Verifica conta de POUPEX - W-CD-PRD-OPRS5900 = 644
      *
           IF  W5900-COD-RETORNO NOT EQUAL ZEROS
               IF  W5900-COD-RETORNO = 1
               OR  W5900-PRG-RETORNO = 'EIFSB018'
                   IF  W-CD-PRD-OPRS5900 = 3
                       MOVE 1   TO W5900-CD-INST-ORGC-E
                           S500V-CD-INST-ORGC
                       MOVE 644 TO W-CD-PRD-OPRS5900
                       PERFORM 226-000-VALIDA-AG-CONTA-POUP
                   ELSE
                       GO TO 999125-ERRO-125
                   END-IF
               ELSE
                   GO TO 999055-ERRO-055
               END-IF
           END-IF.
      *
       872-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   872-099-SAIDA.'
                      .
       872-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.


      *----------------------------------------------------------------*
       873-000-CALCULA-DIGITO-AGENCIA SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 873-000-CALCULA-DIGITO-AGENCIA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Referente a agência BB correspondente a agência no BNC.
      *--  Calcula dígito da agência.
           MOVE S500V-CD-PRF-DEPE-OPR TO W-CD-PRF-DEPE.

           MOVE 011           TO COD-FUNC.
           MOVE W-CD-PRF-DEPE TO VET-NMRO.

           CALL SBDIGITO USING COD-FUNC VET-NMRO DIG-CALC2 COD-ERRO.

           IF  RETURN-CODE NOT EQUAL ZEROS
               GO TO 999127-ERRO-127
           END-IF.

           MOVE  DIG-CALC1    TO S500V-DV-PRF-DEPE-OPR.

       873-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   873-099-SAIDA.'
                      .
       873-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       874-000-CALCULA-DIGITO-CONTA   SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 874-000-CALCULA-DIGITO-CONTA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *--  Referente a conta BB correspondente a conta no BNC.
      *--  Calcula dígito da conta corrente.

           MOVE 011              TO COD-FUNC.
           MOVE S500V-NR-CC-TMDR TO VET-NMRO.

           CALL SBDIGITO USING COD-FUNC VET-NMRO DIG-CALC2 COD-ERRO.

           IF  RETURN-CODE NOT EQUAL ZEROS
               GO TO 999128-ERRO-128
           END-IF.

           MOVE  DIG-CALC1       TO  S500V-DV-CC-TMDR.

       874-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   874-099-SAIDA.'
                      .
       874-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       875-000-VALIDA-STATUS-EIF      SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 875-000-VALIDA-STATUS-EIF.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      *    Trata Conta Corrente
           IF  W-CD-PRD-OPRS5900 EQUAL 6
               MOVE 2                     TO EIFSB018-FUNCAO
               MOVE S500V-NR-CC-TMDR      TO EIFSB018-CONTA
           END-IF.

      *    Trata Poupança
           IF  W-CD-PRD-OPRS5900 EQUAL 3
               MOVE 4                     TO EIFSB018-FUNCAO
               MOVE 1                     TO EIFSB018-VAR-POUP
               MOVE S500V-NR-CC-TMDR      TO W-CT-POUP
               MOVE W-NR-CT-POUP          TO EIFSB018-CONTA
           END-IF.

           MOVE 'O'                   TO EIFSB018-AMBIENTE.
           MOVE 1                     TO EIFSB018-INSTITUICAO.
           MOVE S500V-CD-PRF-DEPE-OPR TO EIFSB018-AGENCIA.

           MOVE LENGTH OF EIFSB018-DADOS TO EIBCALEN
           CALL EIFSB018 USING DFHEIBLK EIFSB018-DADOS.

           IF  EIFSB018-COD-RET NOT EQUAL ZEROS
      *    Registro não encontrado
               AND EIFSB018-COD-RET NOT EQUAL 14
               MOVE EIFSB018-COD-RET TO W-CD-RTN-EIFSB018
               PERFORM 999139-ERRO-139
           END-IF.

           IF  EIFSB018-COD-RET EQUAL ZEROS
               IF  EIFSB018-DTA-INPO-RET NOT EQUAL ZEROS
                   UNSTRING S500V-DT-MVT  DELIMITED BY '.'
                       INTO W-DD OF W-DT-AMD
                       W-MM OF W-DT-AMD
                       W-AA OF W-DT-AMD
                   MOVE W-DT-AMD TO W-DT-MVT
                   IF  EIFSB018-DTA-INPO-RET >= W-DT-MVT
      *                Data da incorporacao futura. As contas espelho
      *                terão esta data maior que data atual
      *                Desta forma esta OK
                       MOVE 1 TO W-IN-CT-MIGD
                   END-IF
               END-IF
           END-IF.

       875-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   875-099-SAIDA.'
                      .
       875-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       880-000-CHAMA-BECSO103        SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 880-000-CHAMA-BECSO103.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BECSO103
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

      *--  Captura data e hora do processamento
           PERFORM 980-000-RECUPERA-DT-HR-ATUAL

           UNSTRING S500V-DT-LQDC DELIMITED BY '.' INTO W-DD-LQDC
               W-MM-LQDC
               W-AA-LQDC.
           MOVE S500V-CD-PRF-DEPE-EXEC TO SO103-AG.
           MOVE S500V-CD-TIP-MC        TO SO103-TIP-MC.
           MOVE S500V-CD-MOE           TO SO103-CD-MOE.
           MOVE L-DT-ATU               TO SO103-DT-BASE.
           MOVE L-HR-ATU               TO SO103-HR-BASE.
           MOVE 'V'                    TO SO103-IND-CPR-VND.
      *
      *    Para ordens via Internet PJ utilizar IPTF Internet
           IF  S500V-CD-CNL EQUAL 'I'
               AND S500V-CD-TIP-CLI EQUAL 2
               MOVE 'INRTV'            TO SO103-TIP-TAXA
           ELSE
               MOVE 'ORPGV'            TO SO103-TIP-TAXA
           END-IF.
           MOVE W-DT-LQDC              TO SO103-DT-MOEE.
           MOVE S500V-DT-DEB(1:2)      TO SO103-DT-RSV(1:2)
           MOVE S500V-DT-DEB(4:2)      TO SO103-DT-RSV(3:2)
           MOVE S500V-DT-DEB(7:4)      TO SO103-DT-RSV(5:4)
           MOVE LENGTH OF L-BECSO103 TO EIBCALEN
           CALL BECSO103 USING DFHEIBLK L-BECSO103.

           IF  SO103-CD-RET NOT EQUAL ZEROS
               GO TO 999056-ERRO-056
           END-IF.

      *--  DE 90120 Ação 52379 Na intermediação 4 a chamada ao BEC é
      *    apenas para verificar data do câmbio pronto
           IF  S500V-CD-FMA-INTM NOT EQUAL 4
               MOVE SO103-TAXA-CMBL       TO S500V-VL-TAXA-CMB
               MOVE SO103-DT-BASE         TO S500V-DT-BASE-TAXA
               MOVE SO103-HR-BASE         TO S500V-HR-BASE-TAXA
           END-IF.

       880-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   880-099-SAIDA.'
                      .
       880-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       885-000-VALIDA-PROPOSTA       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 885-000-VALIDA-PROPOSTA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-BECSO320 REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-DT-EST           TO DT-AUX-ATU-A10.
           MOVE DD-AUX-ATU-A10         TO DD-AUX-EST-N8.
           MOVE MM-AUX-ATU-A10         TO MM-AUX-EST-N8.
           MOVE AA-AUX-ATU-A10         TO AA-AUX-EST-N8.
           MOVE 'OPE'                  TO KO320-SIS-OGM.
           MOVE 'V'                    TO KO320-TIP-OPR.
           MOVE S500V-CD-MOE           TO KO320-CD-MOE.
           MOVE S500V-NR-BLT-BEC       TO KO320-NR-BEC.
           MOVE S500V-CD-PRF-DEPE-EXEC TO KO320-PRF-AG-EXEC.
           MOVE 4                      TO KO320-TIP-TAXA.

           MOVE LENGTH OF L-BECSO320 TO EIBCALEN
           CALL BECSO320 USING DFHEIBLK L-BECSO320.

           IF  KO320-CD-RTN NOT EQUAL ZEROS
               GO TO 999057-ERRO-057
           END-IF.

           IF  KO320-TIP-MC NOT EQUAL S500V-CD-TIP-MC
               GO TO 999058-ERRO-058
           END-IF.

           IF  KO320-CD-FMA-ETGA-MOE NOT EQUAL S500V-CD-FMA-ETGA-MOE
               GO TO 999059-ERRO-059
           END-IF.

           MOVE S500V-DT-DEB         TO DT-AUX-ATU-A10.
           MOVE DD-AUX-ATU-A10       TO DD-AUX-EST-N8.
           MOVE MM-AUX-ATU-A10       TO MM-AUX-EST-N8.
           MOVE AA-AUX-ATU-A10       TO AA-AUX-EST-N8.
           IF  KO320-DT-DEB-CLI NOT EQUAL DT-AUX-EST-N8
               GO TO 999060-ERRO-060
           END-IF.

           IF  KO320-CD-CLI NOT EQUAL S500V-CD-CLI
               GO TO 999061-ERRO-061
           END-IF.

           IF  S500V-CD-OPC EQUAL '11'
               COMPUTE W-VL-DSPN-PRPT = KO320-VL-ME - KO320-VL-UTZD-ME
               ON SIZE ERROR
                  PERFORM 999204-ERRO-204
               END-COMPUTE

               IF  S500V-VL-MOEE > W-VL-DSPN-PRPT
                   GO TO 999062-ERRO-062
               END-IF
           END-IF.

           IF  (KO320-PRF-AG-CT-CLI NOT EQUAL ZEROS
               AND KO320-NR-CT-CLI     NOT EQUAL ZEROS)
               AND(KO320-PRF-AG-CT-CLI NOT EQUAL S500V-CD-PRF-DEPE-OPR
               OR  KO320-NR-CT-CLI     NOT EQUAL S500V-NR-CC-TMDR)
               GO TO 999063-ERRO-063
           END-IF.

           MOVE KO320-TAXA-CMBL TO S500V-VL-TAXA-CMB.
           .
       885-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   885-099-SAIDA.'
                      .
       885-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       890-000-BUSCA-TAXA            SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 890-000-BUSCA-TAXA.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

           INITIALIZE L-OPES824S
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.
      *--  DE 90120 Ação 52379 Movimenta data de emissão para pesquisa
      *    da taxa. O cálculo de datas é feito pelo BEC
           MOVE S500V-DT-EMS    TO S824S-DT-PSQ.
      *    MOVE S500V-DT-LQDC   TO S824S-DT-PSQ.
           MOVE S500V-CD-MOE    TO S824S-CD-MOE.
           MOVE S500V-CD-TIP-MC TO S824S-CD-TIP-MC-CMB.
           MOVE 'C'             TO S824S-CD-TIP-OPR.

           MOVE LENGTH OF L-OPES824S TO EIBCALEN
           CALL OPES824S USING DFHEIBLK L-OPES824S.

           IF  S824S-CD-MSG-RTN NOT EQUAL ZEROS
               GO TO 999085-ERRO-085
           END-IF.

           MOVE S824S-VL-TAXA-CMBL TO S500V-VL-TAXA-CMB.


           IF  S824S-IN-MTP-DVSR EQUAL 'M'
               COMPUTE S500V-VL-MOEN ROUNDED =
                   S500V-VL-MOEE * S500V-VL-TAXA-CMB
               ON SIZE ERROR
                  PERFORM 999205-ERRO-205
               END-COMPUTE
           ELSE
               COMPUTE S500V-VL-MOEN ROUNDED =
                   S500V-VL-MOEE / S500V-VL-TAXA-CMB
               ON SIZE ERROR
                  PERFORM 999206-ERRO-206
               END-COMPUTE
           END-IF.

       890-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   890-099-SAIDA.'
                      .
       890-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       900-000-VALIDA-FINAL          SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 900-000-VALIDA-FINAL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

      * -- Se a ordem for originada de processamento em lote, o codigo
      * -- da natureza sera 14

           IF  S500V-NR-RMS EQUAL ZEROS
               IF  S500V-CD-TIP-ORD-PGTO = 3
                   MOVE 13                    TO S500V-CD-NTZ
               ELSE
                   IF  S500V-CD-PAIS-BNFC     EQUAL       105
                       PERFORM  910-000-VALIDA-CD-NTZ
                       IF  BCIS051N-RTN-PSQ   EQUAL       1
                           MOVE 86            TO S500V-CD-NTZ
                       ELSE
                           IF  S500V-CD-FMA-ETGA-MOE EQUAL 30
                               MOVE 56            TO S500V-CD-NTZ
                           ELSE
                               MOVE 13            TO S500V-CD-NTZ
                           END-IF
                       END-IF
                   ELSE
                       IF  S500V-CD-FMA-ETGA-MOE EQUAL 30
                           MOVE 56            TO S500V-CD-NTZ
                       ELSE
                           MOVE 13            TO S500V-CD-NTZ
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE 14                 TO S500V-CD-NTZ
           END-IF

      *--  Verifica se o saldo da OBT eh suficiente para
      *--  todos os debitos da ORPAG
      *--  Validação retirada a pedido do Marlon conforme
      *--  documento anexo a ação 98387
      *    IF  (S500V-OPCAO EQUAL '11A' OR S500V-OPCAO EQUAL '12A')
      *    AND (S500V-CD-TIP-ORD-PGTO  = 1
      *    OR   S500V-CD-TIP-ORD-PGTO  = 2)
      *    AND  S500V-CD-FMA-INTM      = 3
      *    AND  S500V-CD-TLA          >= 6
      *        IF  S500V-CD-CNL     NOT EQUAL 'B'
      *        OR (S500V-CD-CNL         EQUAL 'B'
      *        AND S500V-SG-SIS-OGM NOT EQUAL ' '
      *        AND S500V-NR-RMS     NOT EQUAL 0)
      *            PERFORM 901000-CONSULTA-VL-OB
      *
      *            IF  S500V-VL-TTL-ORPG-MOEN NOT EQUAL S042G-VL-OB
      *                MOVE 1 TO S500V-CD-TLA
      *                PERFORM 999174-ERRO-174
      *            END-IF
      *        END-IF
      *    END-IF
           .
       900-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   900-099-SAIDA.'
                      .
       900-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *-----------------------------------------------------------------
      *901000-CONSULTA-VL-OB          SECTION.
      *-----------------------------------------------------------------
      *    INITIALIZE OPES042G-DADOS
      *
      *    MOVE 8                TO  S042G-CD-FUC
      *    MOVE S500V-CD-USU     TO  S042G-CD-CHV-USU
      *    MOVE S500V-AA-EMS-OB  TO  S042G-AA-EMS-OB
      *    MOVE S500V-NR-OB      TO  S042G-NR-OB
      *
      *    IF  EXECUCAO-ONLINE
      *        EXEC CICS
      *            LINK PROGRAM ( OPES042G)
      *            COMMAREA     ( OPES042G-DADOS )
      *            LENGTH       (LENGTH OF OPES042G-DADOS )
      *        END-EXEC
      *    ELSE
      *        MOVE LENGTH OF OPES042G-DADOS TO EIBCALEN
      *        CALL OPES042G USING DFHEIBLK OPES042G-DADOS
      *    END-IF
      *
      *
      *    IF  S042G-CD-RTN-PGM NOT ZERO
      *        MOVE S042G-CD-RTN-PGM TO W-CD-RTN
      *        PERFORM 999175-ERRO-175
      *    END-IF
      *    .
      *910000-SAI.
      *    EXIT.
      *
      *-----------------------------------------------------------------
       910-000-VALIDA-CD-NTZ          SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 910-000-VALIDA-CD-NTZ.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------
      *
           MOVE 0015                   TO  W-RGR-ENT.
      *
           PERFORM 919-000-CHAMA-BCIS051N.
      *
       910-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   910-099-SAIDA.'
                      .
       910-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *-----------------------------------------------------------------
       919-000-CHAMA-BCIS051N         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 919-000-CHAMA-BCIS051N.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------
      *
           INITIALIZE L-BCIS051N
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.
      *
           MOVE 'OPE'                  TO  BCIS051N-SG-SIS
           MOVE 'OPES500V'             TO  BCIS051N-NM-PGM-CHMR
           MOVE 'N'                    TO  BCIS051N-AVLR-EST-NTZ
           MOVE S500V-DT-EMS           TO  BCIS051N-DT-MVT
           MOVE S500V-CD-NTZ-FATO-OPRL TO  BCIS051N-CD-NTZ-FATO-OPRL
           MOVE W-RGR-ENT              TO  BCIS051N-RGR-ENT.
      *
           MOVE LENGTH OF L-BCIS051N TO EIBCALEN
           CALL BCIS051N USING DFHEIBLK L-BCIS051N.
      *
           EVALUATE BCIS051N-CD-RTN
               WHEN 0
                  CONTINUE
               WHEN OTHER
                  PERFORM 999162-ERRO-162
           END-EVALUATE
           .
       919-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   919-099-SAIDA.'
                      .
       919-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *-----------------------------------------------------------------
       920000-VRF-CD-CPSO             SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 920000-VRF-CD-CPSO.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------
           INITIALIZE  L-OPES043R

           IF  S500V-CD-CVN-ORD-PGTO NOT ZERO
               MOVE 2                   TO K043R-CD-FUC
           ELSE
               MOVE 3                   TO K043R-CD-FUC
           END-IF
           MOVE S500V-CD-CVN-ORD-PGTO   TO K043R-CD-CVN-ORD-PGTO-ENT
           MOVE S500V-CD-INST-BNFC      TO K043R-CD-IFC-BNFC-ENT
           MOVE S500V-CD-TIP-ORD-PGTO   TO K043R-CD-TIP-ORD-PGTO-ENT
           MOVE S500V-DT-EMS            TO K043R-DT-INC-VGC-RGR-ENT
           MOVE S500V-CD-TIP-ITCE-REG   TO K043R-CD-TIP-ITCE-ENT

           MOVE LENGTH OF L-OPES043R TO EIBCALEN
           CALL OPES043R USING DFHEIBLK L-OPES043R

           IF  K043R-CD-RTN         NOT EQUAL ZEROS AND
               K043R-CD-SQLCODE-RTN NOT EQUAL 100
               GO TO 999161-ERRO-161
           ELSE
               IF  K043R-CD-RTN EQUAL ZEROS
                   INITIALIZE L-BDDE142H BDDA142A
                       W-CD-FED-ABA
                       W-CD-CPSO

                   MOVE K043R-CD-IFC-BNFC-SAI TO BDDA142A-CD-IDTR

                   MOVE LENGTH OF L-BDDE142H TO EIBCALEN
                   CALL BDDE142H USING DFHEIBLK L-BDDE142H

                   IF  BDDAMSGS-CD-MSG OF L-BDDE142H NOT ZERO
                       PERFORM 999129-ERRO-129
                   END-IF

                   IF  BDDA142A-CD-FED-ABA IS NUMERIC AND
                       BDDA142A-CD-FED-ABA NOT EQUAL ZEROS
                       MOVE BDDA142A-CD-FED-ABA TO W-CD-FED-ABA
                       MOVE W-CD-CPSO           TO S500V-CD-CPSO
                   END-IF
               END-IF
           END-IF
           .
       920000-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   920000-SAI.'
                      .
       920000-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       930-000-CHAMA-BCIS4000 SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 930-000-CHAMA-BCIS4000.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           MOVE LENGTH OF L-BCIS4000 TO EIBCALEN
           CALL BCIS4000 USING DFHEIBLK L-BCIS4000
           IF BCIS4000-CD-RTN NOT EQUAL ZEROS
              PERFORM 999229-ERRO-229
           END-IF
           .
       930-099-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   930-099-SAI.'
                      .
       930-099-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *-----------------------------------------------------------------
       960-000-BUSCA-SEQL-TRML       SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 960-000-BUSCA-SEQL-TRML.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------

           INITIALIZE L-ORES0871
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE S500V-CD-PRF-DEPE-EMT TO K0871-CD-DEPE-RSP.

           EXEC CICS LINK
                PROGRAM ( ORES0871 )
                COMMAREA( L-ORES0871 )
                LENGTH  ( LENGTH OF L-ORES0871 )
           END-EXEC.

           IF  K0871-CD-MSG-RTN NOT EQUAL ZEROS
               GO TO 999026-ERRO-026
           END-IF.

           MOVE K0871-NR-SEQL-RMS TO S500V-NR-SEQL-TRML.

       960-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   960-099-SAIDA.'
                      .
       960-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *-----------------------------------------------------------------
       970-000-VALIDA-REGRAS            SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 970-000-VALIDA-REGRAS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *-----------------------------------------------------------------

           INITIALIZE L-BCIS4010
               REPLACING ALPHANUMERIC BY ' '
               NUMERIC BY  0.

           MOVE 'OPE'                   TO BCIK4010-SG-SIS-E
           MOVE S500V-CD-TIP-ITCE-REG   TO BCIK4010-CD-TIP-ITCE-REG-E
           MOVE 'OPES500V'              TO BCIK4010-NM-PGM-CHMR-E
           MOVE S500V-CD-CLI            TO BCIK4010-CD-CLI-E
           MOVE S500V-CD-NTZ-FATO-OPRL  TO BCIK4010-NTZ-FATO-OPRL-E
           MOVE S500V-CD-GR-OPR-CMB     TO BCIK4010-GR-FATO-OPRL-E
           MOVE S500V-CD-USU            TO BCIK4010-CD-USU-E
           MOVE S500V-CD-IDFR-ORD-PGTO  TO BCIK4010-NR-OPR-SIS-E
           MOVE 'T4CP'                  TO BCIK4010-TIP-CTR-E
      *
           IF S500V-VL-TTL-ORPG-MOEN NOT GREATER S500V-VL-MOEN
              MOVE S500V-VL-MOEN          TO BCIK4010-VL-OPR-MN-E
           ELSE
              MOVE S500V-VL-TTL-ORPG-MOEN TO BCIK4010-VL-OPR-MN-E
           END-IF
      *
           MOVE S500V-VL-MOEE             TO BCIK4010-VL-OPR-USD-E
           MOVE S500V-CD-MOE              TO BCIK4010-CD-MOE-E
      *
           MOVE LENGTH OF L-BCIS4010 TO EIBCALEN
           CALL BCIS4010 USING DFHEIBLK L-BCIS4010
      *
           MOVE BCIK4010-IN-OCOR-VLDC-S TO S500V-IN-OCOR-VLDC-RGR
           MOVE BCIK4010-TS-VLDC-RGR-S  TO S500V-TS-VLDC-RGR
      *
      * >> Repassa número somente para Matriz de Risco Ativa.
           IF  L-IN-RGR-MT-RSCO EQUAL 'S'
               MOVE BCIK4010-CD-IDFC-EXEA-ANL-S
                                        TO S500V-CD-IDFC-EXEA-ANL
           ELSE
               MOVE ZEROS               TO S500V-CD-IDFC-EXEA-ANL
           END-IF
      *
           IF BCIK4010-CD-RTN  NOT EQUAL ZEROS
              IF S500V-CD-TIP-ITCE-REG EQUAL 15
                 CONTINUE
              ELSE
                 PERFORM 971-000-GRV-INF-CMPT-ANL
                 PERFORM 999189-ERRO-189
              END-IF
           END-IF
      *
      * >> Grava complemento do log de análise da matriz de risco
           IF  BCIK4010-IN-OCOR-VLDC-S EQUAL 'S'
               PERFORM 971-000-GRV-INF-CMPT-ANL
           END-IF
           .
       970-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   970-099-SAIDA.'
                      .
       970-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       971-000-GRV-INF-CMPT-ANL         SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 971-000-GRV-INF-CMPT-ANL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           IF  S500V-CD-IDFC-EXEA-ANL EQUAL ZEROS
           OR  S500V-CD-IDFC-EXEA-ANL NOT NUMERIC
               GO TO 971-099-SAIDA
           END-IF
      *
           INITIALIZE L-BCISLOG1
      *
           SET BCISLOG1-INCLUIR-XML     TO TRUE
           MOVE S500V-CD-IDFC-EXEA-ANL  TO BCISLOG1-CD-IDFC-EXEA-E
      *
      * >> Grava Natureza
           MOVE S500V-CD-NTZ-FATO-OPRL  TO GDA-NATU-FATO-COD
           MOVE S500V-CD-CPRD-VNDR-MOE  TO GDA-TIPO-CPDR-VDDR
           MOVE S500V-CD-TIP-AVL-GOV    TO GDA-TIPO-AVAL-GVFD
           MOVE S500V-CD-PGDR-RCBD-BC   TO GDA-TIPO-PGDR-RCDR
           MOVE S500V-CD-GR-OPR-CMB     TO GDA-GRPO-OPER-CBIO
      *
      * >> Conta da Operação
           MOVE S500V-NR-CC-TMDR        TO W-TX-NUM-EDT-N11
           UNSTRING W-TX-NUM-N11 DELIMITED BY '+' INTO W-TX-VRV-EDT
                                                       W-NR-CTA-ALF1
      *
      * >> Tipo da Interface
           MOVE S500V-CD-TIP-ITCE-REG   TO W-TX-NUM-EDT-N9
           UNSTRING W-TX-NUM-N9 DELIMITED BY '+' INTO W-TX-VRV-EDT
                                                      W-TIP-ITCE-ALF1
      *
      * >> Trata Valores da Operação.
           MOVE S500V-VL-MOEN           TO W-VL-AUX-EDT
           UNSTRING W-VL-OPR-AUX DELIMITED BY '+' INTO W-TX-VRV-EDT
                                                       W-VL-MOEN-ALF1
      *
           MOVE S500V-VL-MOEE           TO W-VL-AUX-EDT
           UNSTRING W-VL-OPR-AUX DELIMITED BY '+' INTO W-TX-VRV-EDT
                                                       W-VL-MOEE-ALF1
      *
           STRING
               '<TLA>'
               '<vlOprMoee>'    W-VL-MOEE-ALF1        '</vlOprMoee>'
               '<vlOprMoen>'    W-VL-MOEN-ALF1        '</vlOprMoen>'
               '<cdNtzOpr>'     GDA-NATUREZA          '</cdNtzOpr>'
               '<cdDepeFcadCli>'S500V-CD-PRF-DEPE-FCAD'</cdDepeFcadCli>'
               '<cdMoeOpr>'     S500V-CD-MOE          '</cdMoeOpr>'
               '<cdDepeExecOpr>'S500V-CD-PRF-DEPE-EXEC'</cdDepeExecOpr>'
               '<tsExecAnl>'    S500V-TS-VLDC-RGR     '</tsExecAnl>'
               '<inRgrRcsd>'    S500V-IN-OCOR-VLDC-RGR'</inRgrRcsd>'
               '<cdMciCli>'     S500V-CD-CLI          '</cdMciCli>'
               '<cdTipCli>'     S500V-CD-TIP-CLI      '</cdTipCli>'
               '<cdPaisBnfc>'   S500V-CD-PAIS-BNFC    '</cdPaisBnfc>'
               '<nmBnfc>'       S500V-NM-BNFC         '</nmBnfc>'
               '<cdDepeCtaOpr>' S500V-CD-PRF-DEPE-OPR '</cdDepeCtaOpr>'
               '<nrCtaOpr>'     W-NR-CTA-ALF1         '</nrCtaOpr>'
               '<cdTipItce>'    W-TIP-ITCE-ALF1       '</cdTipItce>'
               '</TLA>'
               DELIMITED BY SIZE        INTO BCISLOG1-TX-CMPT-ANL-E
      *
           PERFORM 972-000-CHAMA-BCISLOG1
           .
       971-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   971-099-SAIDA.'
                      .
       971-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       972-000-CHAMA-BCISLOG1           SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 972-000-CHAMA-BCISLOG1.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN BCISLOG1-INCLUIR-XML
                    IF  BCISLOG1-TX-CMPT-ANL-E EQUAL SPACES
                        GO TO 972-099-SAIDA
                    END-IF
               WHEN OTHER
                    GO TO 972-099-SAIDA
           END-EVALUATE
      *
           MOVE LENGTH OF L-BCISLOG1 TO EIBCALEN
           CALL BCISLOG1 USING DFHEIBLK L-BCISLOG1
           .
       972-099-SAIDA.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   972-099-SAIDA.'
                      .
       972-099-SAIDA-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *----------------------------------------------------------------*
       980-000-RECUPERA-DT-HR-ATUAL SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 980-000-RECUPERA-DT-HR-ATUAL.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           MOVE 2 TO L-CD-FUC
           CALL SBCURDAT USING L-CD-FUC L-DT-ATU L-HR-ATU
           IF RETURN-CODE NOT EQUAL ZERO
              PERFORM 999228-ERRO-228
           END-IF
           .
       980-099-SAI.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   980-099-SAI.'
                      .
       980-099-SAI-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.

      *----------------------------------------------------------------*
       999000-ERROS                  SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 999000-ERROS.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*

       999001-ERRO-001.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999001-ERRO-001.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Operação permitida somente no mesmo dia do regist
      -        'ro.'             TO S500V-TX-MSG-RTN.
           GOBACK.

       999002-ERRO-002.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999002-ERRO-002.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Mercado de câmbio inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999003-ERRO-003.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999003-ERRO-003.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999               TO S500V-CD-RTN.
           MOVE 'S500V Estado da ordem não permite a operação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999004-ERRO-004.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999004-ERRO-004.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999               TO S500V-CD-RTN.
           MOVE 'S500V Ordem possui contrato globalizado. Alteração não
      -        'permitida.'      TO S500V-TX-MSG-RTN.
           GOBACK.

       999005-ERRO-005.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999005-ERRO-005.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999               TO S500V-CD-RTN.
           MOVE 'S500V Opção inválida.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999006-ERRO-006.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999006-ERRO-006.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  04   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de intermediação inválida.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

      *999007-ERRO-007.
      *    MOVE 9999                 TO S500V-CD-RTN.
      *    MOVE 'CONTROLE_PROCESSAM' TO W-NM-TAB.
      *    MOVE SQLCODE              TO W-CD-SQL
      *        S500V-CD-SQL.
      *    GO TO 999900-ERRO-DB2.

       999008-ERRO-008.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999008-ERRO-008.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  05   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Agência ou conta inválida.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999009-ERRO-009.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999009-ERRO-009.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999          TO S500V-CD-RTN.
           MOVE ZEROS         TO S500V-NR-CMP-ERRO.
           STRING 'S500V DMES502V ' K502V-CD-RTN ' ' K502V-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999010-ERRO-010.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999010-ERRO-010.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Opção inválida.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999011-ERRO-011.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999011-ERRO-011.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  31   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o país de destino.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999012-ERRO-012.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999012-ERRO-012.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  31   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Codigo do país inválido para esta opção.'
                      TO S500V-TX-MSG-RTN.
           GOBACK.

       999013-ERRO-013.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999013-ERRO-013.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  31   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Código país não encontrado.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999014-ERRO-014.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999014-ERRO-014.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  08   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro ' COD-ERRO ' ao validar dígito verificador
      -        'da conta.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999015-ERRO-015.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999015-ERRO-015.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  08   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Dígito verificador da conta inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999016-ERRO-016.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999016-ERRO-016.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  06   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro ' COD-ERRO ' ao validar dígito verificador
      -        'da agência.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999017-ERRO-017.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999017-ERRO-017.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  06   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Dígito verificador da agência inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999018-ERRO-018.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999018-ERRO-018.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  33   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Código da instituição inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999019-ERRO-019.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999019-ERRO-019.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  09   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Emissão não permitida para data informada.'
               TO S500V-TX-MSG-RTN
           GOBACK.

       999020-ERRO-020.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999020-ERRO-020.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  09   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data informada não é dia util.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999021-ERRO-021.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999021-ERRO-021.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  09   TO S500V-NR-CMP-ERRO.
           MOVE S500V-DT-VLZC TO SO030-DT-VLZC.
           INSPECT SO030-DT-VLZC REPLACING ALL '.' BY '/'.
           STRING 'S500V Data da emissão não pode ser anterior a '
               SO030-DT-VLZC '.' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999022-ERRO-022.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999022-ERRO-022.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  10   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data informada não é dia util.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999023-ERRO-023.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999023-ERRO-023.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  10   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data da liquidação menor que a data da emissão.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999024-ERRO-024.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999024-ERRO-024.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  11   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data informada não é dia util.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999025-ERRO-025.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999025-ERRO-025.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  11   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data da trava menor que a data da liquidação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999026-ERRO-026.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999026-ERRO-026.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  11   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data da trava menor que a data do movimento.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999027-ERRO-027.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999027-ERRO-027.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  12   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data informada não é dia util.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999028-ERRO-028.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999028-ERRO-028.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  12   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data do débito menor que a data da emissão.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999029-ERRO-029.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999029-ERRO-029.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  12   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Data do débito(' S500V-DT-DEB
               ') maior que a data da liquidação(' S500V-DT-LQDC ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999030-ERRO-030.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999030-ERRO-030.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999   TO S500V-CD-RTN.
           MOVE  ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Taxa cambial inválida.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999031-ERRO-031.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999031-ERRO-031.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  15   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda não cadastrada.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999032-ERRO-032.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999032-ERRO-032.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999           TO S500V-CD-RTN.
           MOVE  15            TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' K002R-TX-MSG-RTN DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           MOVE K002R-CD-SQL   TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999033-ERRO-033.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999033-ERRO-033.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  15   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda inativa.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999034-ERRO-034.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999034-ERRO-034.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999         TO S500V-CD-RTN.
           MOVE  15          TO S500V-NR-CMP-ERRO.
           MOVE K502V-CD-MOE TO W-CD-MOE.
           STRING 'S500V Código da moeda informado não confere com a moe
      -        'da da conta (' W-CD-MOE ').' DELIMITED BY SIZE
               INTO  S500V-TX-MSG-RTN.
           GOBACK.

       999035-ERRO-035.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999035-ERRO-035.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  25   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de entrega da moeda não encontrada.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999036-ERRO-036.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999036-ERRO-036.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                 TO S500V-CD-RTN.
           MOVE  25                  TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' K017R-TX-MSG-RTN DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           MOVE K017R-CD-SQL         TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999037-ERRO-037.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999037-ERRO-037.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  25   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de entrega da moeda não ativa.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999038-ERRO-038.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999038-ERRO-038.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  40   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe banqueiro destinatário.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999039-ERRO-039.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999039-ERRO-039.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999        TO S500V-CD-RTN.
           MOVE W-NR-CMP    TO S500V-NR-CMP-ERRO.
           MOVE 143-MSG-RET TO S500V-TX-MSG-RTN.
           GOBACK.

       999040-ERRO-040.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999040-ERRO-040.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  40   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Banqueiro não possui chave SWIFT trocada.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999041-ERRO-041.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999041-ERRO-041.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  40   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Instituição destinatária com SWIFT não trocado.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999042-ERRO-042.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999042-ERRO-042.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999         TO S500V-CD-RTN.
           MOVE  42          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe instituição de cobertura.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999043-ERRO-043.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999043-ERRO-043.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  35   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Banqueiro não encontrado.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999044-ERRO-044.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999044-ERRO-044.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE K0820-CD-RTN     TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE K0820-TX-MSG-RTN TO S500V-TX-MSG-RTN.
           GOBACK.

       999045-ERRO-045.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999045-ERRO-045.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999          TO S500V-CD-RTN.
           MOVE ZEROS         TO S500V-NR-CMP-ERRO.
           MOVE SO030-MSG-RET TO S500V-TX-MSG-RTN.
           GOBACK.

       999046-ERRO-046.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999046-ERRO-046.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           STRING 'S500V Data ' DELIMITED BY SIZE
               W-TX-TIP-DATA DELIMITED BY '#'
               ' inválida.'  DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999047-ERRO-047.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999047-ERRO-047.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Erro na subrotina SBDATA.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999048-ERRO-048.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999048-ERRO-048.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V SWIFT automático não permitido para forma de entr
      -        'ega 30-Cheque.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999049-ERRO-049.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999049-ERRO-049.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE S800V-CD-SQL     TO S500V-CD-SQL.
           MOVE S800V-TX-MSG-RTN TO S500V-TX-MSG-RTN.

      *--  Verifica qual o codigo de retorno do OPES800V para fazer
      *    a marcação correta na tela do Natural de acordo com a
      *    forma de intermediacao.

           MOVE S800V-CD-ERRO TO W-CD-ERRO.

           EVALUATE W-CD-ERRO
               WHEN 01 THRU 06
                   MOVE 18 TO S500V-NR-CMP-ERRO
               WHEN 07 THRU 10
                   MOVE 19 TO S500V-NR-CMP-ERRO
               WHEN 11 THRU 12
                   MOVE 20 TO S500V-NR-CMP-ERRO
               WHEN 13 THRU 14
                   MOVE 21 TO S500V-NR-CMP-ERRO
               WHEN 15 THRU 16
                   MOVE 22 TO S500V-NR-CMP-ERRO
               WHEN OTHER
                   MOVE ZEROS TO S500V-NR-CMP-ERRO
           END-EVALUATE.
           GOBACK.

       999050-ERRO-050.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999050-ERRO-050.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  46                          TO S500V-NR-CMP-ERRO.
           STRING ' S500V Moeda não cadastrada para o código do pais inf
      -        'ormado.'
               DELIMITED BY SIZE       INTO S500V-TX-MSG-RTN.
           GOBACK.

       999051-ERRO-051.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999051-ERRO-051.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Erro CICS ao acessar BDDSH143.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999052-ERRO-052.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999052-ERRO-052.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999               TO S500V-CD-RTN.
           MOVE ZEROS              TO S500V-NR-CMP-ERRO.
           STRING 'S500V' BDDAMSGS-MSG OF L-BDDE142X
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999053-ERRO-053.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999053-ERRO-053.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                       TO S500V-CD-RTN.
           MOVE ZEROS                      TO S500V-NR-CMP-ERRO.
           MOVE BDDAMSGS-MSG OF L-BDDE142F TO S500V-TX-MSG-RTN.
           GOBACK.

      *999054-ERRO-054.
      *    MOVE 9999                 TO S500V-CD-RTN.
      *    MOVE ZEROS                TO S500V-NR-CMP-ERRO.
      *    MOVE 'CONTROLE_PROCESSAM' TO W-NM-TAB.
      *    MOVE SQLCODE              TO W-CD-SQL S500V-CD-SQL.
      *    GO TO 999900-ERRO-DB2.

       999055-ERRO-055.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999055-ERRO-055.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999 TO S500V-CD-RTN
           MOVE ZEROS        TO S500V-NR-CMP-ERRO
           MOVE W5900-COD-RETORNO TO W-CD-RTN-MCI
           STRING 'S500V-OPRS5900: '
               W5900-PRG-RETORNO            DELIMITED BY SIZE
               '-'                          DELIMITED BY SIZE
               W-CD-RTN-MCI                 DELIMITED BY SIZE
               '-'                          DELIMITED BY SIZE
               W5900-MSG-RETORNO            DELIMITED BY '  '
               ' (55)'                      DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999056-ERRO-056.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999056-ERRO-056.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           STRING 'S500V-BECSO103'          DELIMITED BY SIZE
               SO103-MSG-RET             DELIMITED BY '  '
               ' BEC('                   DELIMITED BY SIZE
               SO103-CD-RET              DELIMITED BY SIZE
               ').'                      DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999057-ERRO-057.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999057-ERRO-057.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999         TO S500V-CD-RTN.
           MOVE ZEROS        TO S500V-NR-CMP-ERRO.
           STRING 'S500V-BECSO320'          DELIMITED BY SIZE
               KO320-MSG-RTN             DELIMITED BY '  '
               ' BEC('                   DELIMITED BY SIZE
               KO320-CD-RTN              DELIMITED BY SIZE
               ').'                      DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999058-ERRO-058.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999058-ERRO-058.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Mercado da proposta não confere com o mercado da
      -        'ordem.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999059-ERRO-059.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999059-ERRO-059.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de entrega da moeda não confere com a da or
      -        'dem.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999060-ERRO-060.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999060-ERRO-060.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Data da proposta não confere com a data da operaç
      -        'ão.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999061-ERRO-061.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999061-ERRO-061.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Cliente da proposta não confere com o da operação
      -        '.'  TO S500V-TX-MSG-RTN.
           GOBACK.

       999062-ERRO-062.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999062-ERRO-062.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  16   TO S500V-NR-CMP-ERRO.
           MOVE ' S500V Valor na moeda superior ao permitido pelo BEC.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999063-ERRO-063.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999063-ERRO-063.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Conta da proposta não confere com a conta da oper
      -        'ação.' TO S500V-TX-MSG-RTN.
           GOBACK.

      *999064-ERRO-064.
      *    MOVE 9999  TO S500V-CD-RTN.
      *    MOVE  03   TO S500V-NR-CMP-ERRO.
      *    MOVE 'S500V CPF informado inválido.' TO S500V-TX-MSG-RTN.
      *    GOBACK.

       999065-ERRO-065.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999065-ERRO-065.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  46                          TO S500V-NR-CMP-ERRO.
           MOVE 'DB2OPE.MOE-PAIS-WSUN'       TO W-NM-TAB.
           MOVE SQLCODE                      TO W-CD-SQL S500V-CD-SQL.
           PERFORM 999900-ERRO-DB2.

       999066-ERRO-066.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999066-ERRO-066.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  05   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Dependência '              DELIMITED BY SIZE
               W-TX-DEPE                        DELIMITED BY '#'
               ' (' K0820-CD-PRF-DEPE-PSQ ')'
               ' não possui autonomia contábil.' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999067-ERRO-067.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999067-ERRO-067.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  05   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Dependência '         DELIMITED BY SIZE
               W-TX-DEPE                   DELIMITED BY '#'
               ' (' K0820-CD-PRF-DEPE-PSQ ')'
               ' não localizada no Brasil.' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999069-ERRO-069.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999069-ERRO-069.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN
           MOVE  26       TO S500V-NR-CMP-ERRO
           MOVE 'S500V Código do seguro inválido.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999070-ERRO-070.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999070-ERRO-070.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  26       TO S500V-NR-CMP-ERRO.
           MOVE 'SGRA'    TO W-NM-TAB.
           MOVE SQLCODE   TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999071-ERRO-071.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999071-ERRO-071.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999       TO S500V-CD-RTN.
           MOVE ZEROS      TO S500V-NR-CMP-ERRO.
           STRING 'S500V Dependência ' S500V-CD-PRF-DEPE-EMT
               ' não pertence ao grupo CAMBIO.' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN
           GOBACK.

      *999072-ERRO-072.
      *    MOVE 9999              TO S500V-CD-RTN.
      *    MOVE ZEROS             TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Dependência ' S500V-CD-PRF-DEPE-EMT
      *        ' não pode emitir ORPAG Western Union.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
      *    GOBACK.

       999073-ERRO-073.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999073-ERRO-073.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN
           MOVE  27   TO S500V-NR-CMP-ERRO
           MOVE ' S500V Informe o nosso numero.' TO S500V-TX-MSG-RTN
           GOBACK.

       999074-ERRO-074.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999074-ERRO-074.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  27       TO S500V-NR-CMP-ERRO.
           EVALUATE 513-CD-RTN
               WHEN 02
                   MOVE 'S500V Nosso numero cancelado.'
                       TO S500V-TX-MSG-RTN
               WHEN 03
                   MOVE 'S500V Pagamento já efetuado para o nosso numero
      -                'digitado.'                 TO S500V-TX-MSG-RTN
               WHEN 04
                   MOVE 'S500V Nosso numero não encontrado.'
                       TO S500V-TX-MSG-RTN
               WHEN OTHER
                   STRING 'S500V-'          DELIMITED BY SIZE
                       513-TX-MSG-RTN            DELIMITED BY '  '
                       ' ('                      DELIMITED BY SIZE
                       513-SQL-ERRO              DELIMITED BY SIZE
                       ').SEGSB513'              DELIMITED BY SIZE
                       INTO S500V-TX-MSG-RTN
           END-EVALUATE.
           GOBACK.

       999075-ERRO-075.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999075-ERRO-075.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  17   TO S500V-NR-CMP-ERRO.

           MOVE 'S500V Valor em MN fora dos limites de arredondamento.'
               TO S500V-TX-MSG-RTN.

           GOBACK.

      *999076-ERRO-076.
      *    MOVE 9999  TO S500V-CD-RTN.
      *    MOVE  10   TO S500V-NR-CMP-ERRO.
      *    MOVE 'S500V Data inválida.' TO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999077-ERRO-077.
      *    MOVE 9999  TO S500V-CD-RTN.
      *    MOVE ZEROS TO S500V-NR-CMP-ERRO.
      *    MOVE 'S500V Erro na subrotina SBDATA.' TO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999078-ERRO-078.
      *    MOVE 9999  TO S500V-CD-RTN.
      *    MOVE  10   TO S500V-NR-CMP-ERRO.
      *    MOVE 'S500V Operaçoes futuras exigem proposta do BEC.'
      *        TO S500V-TX-MSG-RTN.
      *    GOBACK.

       999079-ERRO-079.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999079-ERRO-079.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  09   TO S500V-NR-CMP-ERRO.
           STRING 'S500V Data da emissão maior que a data do movimento.'
                  '(' W-DT-EMS ':' W-DT-MVT ')' DELIMITED BY SIZE
                  INTO S500V-TX-MSG-RTN.
           GOBACK.

       999080-ERRO-080.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999080-ERRO-080.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  10   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Operação futura não permitida para este tipo de o
      -        'rdem de pagamento.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999082-ERRO-082.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999082-ERRO-082.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  02   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Operaoes retroativas exigem proposta do BEC.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999084-ERRO-084.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999084-ERRO-084.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  04   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Não informar numero de cotação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999085-ERRO-085.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999085-ERRO-085.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE S824S-TX-MSG-RTN TO S500V-TX-MSG-RTN.
           GOBACK.

       999086-ERRO-086.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999086-ERRO-086.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999   TO S500V-CD-RTN.
           MOVE  23    TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de intermediação 4 não admite cobrança de I
      -        'OF.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999087-ERRO-087.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999087-ERRO-087.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999   TO S500V-CD-RTN.
           MOVE  23    TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de intermediação 3 não admite cobrança de I
      -        'OF.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999088-ERRO-088.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999088-ERRO-088.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                              TO S500V-CD-RTN.
           MOVE RETURN-CODE                       TO W-CD-SQL.
           STRING 'S500V Erro ao verificar ambiente. Código:'
               W-CD-SQL DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999089-ERRO-089.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999089-ERRO-089.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  05       TO S500V-NR-CMP-ERRO.
           STRING 'S500V-DEBSB972 ' DELIMITED BY SIZE
               PARM-LIN-MSG     DELIMITED BY '  '
               ' ('              DELIMITED BY SIZE
               PARM-COD-RETORN  DELIMITED BY SIZE
               ').'              DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999090-ERRO-090.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999090-ERRO-090.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           IF  W-AMBIENTE NOT EQUAL 'DES'
               MOVE K0821-CD-RTN     TO S500V-CD-RTN
               MOVE 3                TO S500V-NR-CMP-ERRO
               IF  S500V-CD-CNL EQUAL 'I'
                   STRING 'S500V Cadastro desatualizado. '
                       'Procure sua agência.'
                       DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
               ELSE
                   MOVE K0821-TX-MSG-RTN TO S500V-TX-MSG-RTN
               END-IF
           END-IF.
           GOBACK.

       999091-ERRO-091.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999091-ERRO-091.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  05       TO S500V-NR-CMP-ERRO.
           STRING 'S500V Poupança não permite livre movimentação ('
               RET-IND-CONTA '/' RET-IND-BLOQ ').' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999092-ERRO-092.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999092-ERRO-092.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  05       TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Agência/conta inválida para o cliente.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999093-ERRO-093.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999093-ERRO-093.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  05       TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro na rotina CPRSB001. Retorno: '
               CPR-CONTROLE-ERRO '.' DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999094-ERRO-094.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999094-ERRO-094.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  05       TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Agência/conta incompatível com o tipo de intermed
      -        'iação.'  TO S500V-TX-MSG-RTN.
           GOBACK.

       999095-ERRO-095.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999095-ERRO-095.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999      TO S500V-CD-RTN.
           MOVE  25       TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de entrega não permitida para esta opção (2
      -        ').' TO S500V-TX-MSG-RTN.
           GOBACK.

       999097-ERRO-097.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999097-ERRO-097.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  52                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Valor mínimo para informação de descrição física
      -        'é de USD 500,00.'           TO S500V-TX-MSG-RTN.
           GOBACK.

       999098-ERRO-098.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999098-ERRO-098.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  29                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o endereço.'  TO S500V-TX-MSG-RTN.
           GOBACK.

       999099-ERRO-099.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999099-ERRO-099.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  30                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe a cidade.'    TO S500V-TX-MSG-RTN.
           GOBACK.

       999100-ERRO-100.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999100-ERRO-100.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  47                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o estado.'    TO S500V-TX-MSG-RTN.
           GOBACK.

       999101-ERRO-101.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999101-ERRO-101.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  48                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o CEP.'       TO S500V-TX-MSG-RTN.
           GOBACK.

       999102-ERRO-102.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999102-ERRO-102.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  49                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o telefone.'  TO S500V-TX-MSG-RTN.
           GOBACK.

       999103-ERRO-103.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999103-ERRO-103.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  50                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o local de entrega.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999104-ERRO-104.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999104-ERRO-104.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  51                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o serviço de entrega.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999105-ERRO-105.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999105-ERRO-105.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  51                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Serviço de entrega inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999106-ERRO-106.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999106-ERRO-106.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  47                          TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o estado ou CEP' TO S500V-TX-MSG-RTN.
           GOBACK.

       999108-ERRO-108.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999108-ERRO-108.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE 'DB2OPE.TARF_FXA_VL_ORD_PG'  TO W-NM-TAB.
           MOVE SQLCODE                      TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999109-ERRO-109.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999109-ERRO-109.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.

           MOVE W-VL-TARF                       TO W-VL-OPR-EDT.
           UNSTRING W-VL-OPR DELIMITED BY '+' INTO W-TX-OPR-EDT
               W-VL-OPR-ALF1.
           MOVE VL-TARF-ORD-PGTO                TO W-VL-OPR-EDT.
           UNSTRING W-VL-OPR DELIMITED BY '+' INTO W-TX-OPR-EDT
               W-VL-OPR-ALF2.

           STRING 'S500V Valores de tarifas divergentes. ( WU '
               DELIMITED BY SIZE
               W-VL-OPR-ALF1      DELIMITED BY SPACES
               ' / BB '           DELIMITED BY SIZE
               W-VL-OPR-ALF2      DELIMITED BY SPACES
               ' ).'              DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999110-ERRO-110.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999110-ERRO-110.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE P802S-CD-RTN                   TO S500V-CD-RTN.
           MOVE P802S-MSG-ERRO                 TO S500V-TX-MSG-RTN.
           GOBACK.

       999111-ERRO-111.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999111-ERRO-111.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  46   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda não cadastrada.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999112-ERRO-112.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999112-ERRO-112.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999           TO S500V-CD-RTN.
           MOVE  46            TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' K002R-TX-MSG-RTN DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           MOVE K002R-CD-SQL   TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999113-ERRO-113.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999113-ERRO-113.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  46   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda inativa.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999114-ERRO-114.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999114-ERRO-114.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                         TO S500V-CD-RTN.
           MOVE  31                          TO S500V-NR-CMP-ERRO.
           MOVE S012R-TX-MSG-RTN             TO S500V-TX-MSG-RTN.
           GOBACK.

      *999115-ERRO-115.
      *    INITIALIZE L-ORES0855
      *        REPLACING ALPHANUMERIC BY ' '
      *        NUMERIC BY  0.
      *
      *    MOVE ZEROS                 TO K0855-NR-ORPG.
      *    MOVE S500V-CD-USU          TO K0855-CD-USU.
      *    MOVE S500V-CD-PRF-DEPE-EMT TO K0855-CD-DEPE.
      *    MOVE K0871-CD-MSG-RTN      TO K0855-CD-MSG-ENTD.
      *    MOVE K0871-TX-MSG-RTN      TO K0855-TX-MSG-ENTD.
      *
      *    MOVE LENGTH OF L-ORES0855 TO EIBCALEN
      *    CALL ORES0855 USING DFHEIBLK L-ORES0855.
      *
      *    MOVE 9999   TO S500V-CD-RTN.
      *    MOVE SPACES TO S500V-TX-MSG-RTN.
      *
      *    IF  K0855-CD-MSG-RTN EQUAL ZEROS
      *        STRING 'S500V ' K0855-TX-MSG-SAID
      *            DELIMITED BY SIZE  INTO S500V-TX-MSG-RTN
      *    ELSE
      *        MOVE K0855-TX-MSG-RTN  TO S500V-TX-MSG-RTN
      *    END-IF.
      *
      *    GOBACK.

       999116-ERRO-116.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999116-ERRO-116.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  04   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Forma de intermediação inválida para essa dependê
      -        'ncia.'                      TO S500V-TX-MSG-RTN.
           GOBACK.

       999117-ERRO-117.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999117-ERRO-117.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  46   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda não permitida para ordem WESTERN UNION.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999118-ERRO-118.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999118-ERRO-118.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE 'S500V Moeda não permitida para esta opção (2).'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999119-ERRO-119.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999119-ERRO-119.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE 'S500V Data da trava diferente da data de liquidação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999120-ERRO-120.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999120-ERRO-120.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE 'S500V Intermediação 4 não permite BEC vinculado.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999121-ERRO-121.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999121-ERRO-121.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE K006R-CD-RTN     OF L-CCAS006R TO S500V-CD-RTN.
           MOVE K006R-TX-MSG-RTN OF L-CCAS006R TO S500V-TX-MSG-RTN.
           GOBACK.

       999122-ERRO-122.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999122-ERRO-122.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 122              TO S500V-CD-RTN.
           MOVE ' S500V Contrato simplificado ja globalizado. Utilize a
      -        'opcao 13a.'     TO S500V-TX-MSG-RTN.
           GOBACK.

       999123-ERRO-123.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999123-ERRO-123.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 123              TO S500V-CD-RTN.
           MOVE ' S500V Contrato simplificado ainda nao globalizado. Uti
      -        'lize a opcao 11c.'     TO S500V-TX-MSG-RTN.
           GOBACK.

       999124-ERRO-124.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999124-ERRO-124.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 124   TO S500V-CD-RTN.
           MOVE 'S500V Informe se ha Imposto de renda.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999125-ERRO-125.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999125-ERRO-125.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *    MOVE 125   TO S500V-CD-RTN.
      *    MOVE 'S500V Agencia/conta nao encontrada para o cliente.'
      *               TO S500V-TX-MSG-RTN.
      *    GOBACK.
           MOVE 9999 TO S500V-CD-RTN
           MOVE ZEROS        TO S500V-NR-CMP-ERRO
           MOVE W5900-COD-RETORNO TO W-CD-RTN-MCI
           STRING 'S500V-OPRS5900: '
               W5900-PRG-RETORNO            DELIMITED BY SIZE
               '-'                          DELIMITED BY SIZE
               W-CD-RTN-MCI                 DELIMITED BY SIZE
               '-'                          DELIMITED BY SIZE
               W5900-MSG-RETORNO            DELIMITED BY '  '
               ' (125)'                     DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999126-ERRO-126.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999126-ERRO-126.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 126   TO S500V-CD-RTN.
           STRING
             'S500V Código da instituição inválida - ' DELIMITED BY SIZE
              S500V-CD-INST-ORGC                       DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999127-ERRO-127.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999127-ERRO-127.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 127   TO S500V-CD-RTN.
           STRING 'S500V Erro ' COD-ERRO ' ao validar dígito verificador
      -        'da agência.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999128-ERRO-128.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999128-ERRO-128.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 128   TO S500V-CD-RTN.
           STRING 'S500V Erro ' COD-ERRO ' ao validar dígito verificador
      -        'da conta.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999129-ERRO-129.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999129-ERRO-129.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999                       TO S500V-CD-RTN.
           MOVE ZEROS                      TO S500V-NR-CMP-ERRO.
           MOVE BDDAMSGS-MSG OF L-BDDE142H TO S500V-TX-MSG-RTN.
           GOBACK.

       999130-ERRO-130.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999130-ERRO-130.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999              TO S500V-CD-RTN.
           MOVE ZEROS             TO S500V-NR-CMP-ERRO.
           MOVE BCIS094L-MSG-ERRO TO S500V-TX-MSG-RTN.
           GOBACK.

       999131-ERRO-131.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999131-ERRO-131.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE S035R-TX-MSG-RTN TO S500V-TX-MSG-RTN.
           GOBACK.

       999132-ERRO-132.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999132-ERRO-132.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe detalhes do pagamento.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999133-ERRO-133.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999133-ERRO-133.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'Termo ' DELIMITED BY SIZE
               W-TX-TRM-AUX DELIMITED BY ' '
               ' inválido na informação complementar.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           GOBACK.

       999134-ERRO-134.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999134-ERRO-134.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  05   TO S500V-NR-CMP-ERRO.
           STRING 'S500V EIFSB014 - ' DELIMITED BY ' '
               W-CD-RTN-EIFSB014  DELIMITED BY SIZE
               EIFSB014-MSG       DELIMITED BY ' '
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999135-ERRO-135.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999135-ERRO-135.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 135   TO S500V-CD-RTN.
           STRING
             'S500V Código da instituição inválida - ' DELIMITED BY SIZE
              S500V-CD-INST-ORGC                       DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999136-ERRO-136.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999136-ERRO-136.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Valor em moeda estrangeira inválido.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999137-ERRO-137.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999137-ERRO-137.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 137   TO S500V-CD-RTN.
           STRING
               'S500V Poupanca nao validada - '   DELIMITED BY SIZE
               'Ag = '                  DELIMITED BY SIZE
               W-EIFSB014-AGENCIA-RET   DELIMITED BY ' '
               'Conta = '               DELIMITED BY SIZE
               W-EIFSB014-CONTA-RET     DELIMITED BY ' '
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999138-ERRO-138.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999138-ERRO-138.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 138   TO S500V-CD-RTN.
           MOVE 'Encontrados mais de um BANCO. Utilize pesquisa.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999139-ERRO-139.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999139-ERRO-139.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  05   TO S500V-NR-CMP-ERRO.
           STRING 'S500V EIFSB018 - ' DELIMITED BY ' '
               W-CD-RTN-EIFSB018  DELIMITED BY SIZE
               EIFSB018-MSG-RET   DELIMITED BY ' '
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999140-ERRO-140.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999140-ERRO-140.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 05               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Conta bloqueada.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999141-ERRO-141.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999141-ERRO-141.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 05               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Conta não está ativa.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999142-ERRO-142.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999142-ERRO-142.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 10               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe questão de verificação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999143-ERRO-143.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999143-ERRO-143.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 11               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Não informar questão de verificação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999144-ERRO-144.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999144-ERRO-144.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 11               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe numero do registro BACEN.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999145-ERRO-145.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999145-ERRO-145.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 'S500V Contrato prévio somente para Swift ou Seguro.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999146-ERRO-146.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999146-ERRO-146.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE  23              TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Intermediação DME inválida para Contrato prévio.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999147-ERRO-147.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999147-ERRO-147.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE  31   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V País não cadastrada.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999148-ERRO-148.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999148-ERRO-148.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE  31              TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' K006R-TX-MSG-RTN OF L-BCIS006R
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           MOVE K002R-CD-SQL     TO W-CD-SQL S500V-CD-SQL.
           GO TO 999900-ERRO-DB2.

       999149-ERRO-149.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999149-ERRO-149.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE K003R-CD-RTN-PGM TO W-CD-RTN
           STRING 'S500V BCIS003R - ' DELIMITED BY SIZE
               W-CD-RTN           DELIMITED BY SIZE
               K003R-MSG-ERRO     DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999150-ERRO-150.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999150-ERRO-150.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V CCAS052L - ' DELIMITED BY SIZE
               K052L-CD-RTN       DELIMITED BY SIZE
               K052L-TX-MSG-RTN   DELIMITED BY SIZE
               INTO S500V-TX-MSG-RTN.
           GOBACK.

       999151-ERRO-151.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999151-ERRO-151.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999               TO S500V-CD-RTN.
           MOVE ZEROS              TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Estado da mensagem STR não permite a operação.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999152-ERRO-152.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999152-ERRO-152.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 152                TO S500V-CD-RTN.
           MOVE ' S500V Ordem liquidada. Utilize primeiro a opcao 33c.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999153-ERRO-153.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999153-ERRO-153.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 16               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Valor em moeda estrangeira inferior ao permitido
      -        '(1,00).'             TO S500V-TX-MSG-RTN.
           GOBACK.

       999154-ERRO-154.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999154-ERRO-154.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 154                  TO S500V-CD-RTN.
           MOVE 15                   TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Moeda não permitida para o país informado.'
               TO S500V-TX-MSG-RTN.
           MOVE K108L-CD-SQLCODE-RTN TO S500V-CD-SQL.
           GOBACK.

      *999155-ERRO-155.
      *    MOVE 155         TO S500V-CD-RTN
      *    MOVE 'S500V- BB Americas. Moeda deve ser dólar americano.'
      *        TO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999156-ERRO-156.
      *    MOVE 156         TO S500V-CD-RTN
      *    MOVE 'S500V- BB Americas. Responsável pela despesa deve ser o
      *        ' favorecido.'   TO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999157-ERRO-157.
      *    MOVE 157         TO S500V-CD-RTN
      *    MOVE 'S500V- BB Americas. País do beneficiário deve ser Estad
      *        'os Unidos.'     TO S500V-TX-MSG-RTN.
      *    GOBACK.

       999158-ERRO-158.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999158-ERRO-158.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 158                 TO S500V-CD-RTN
           MOVE BCIS921T-CD-RTN-PGM TO W-CD-RTN
           STRING 'S500V-S921T ' W-CD-RTN ' ' BCIS921T-MSG-ERRO
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      * 999159-ERRO-159.
      *     MOVE 159                 TO S500V-CD-RTN
      *     MOVE K218R-CD-RTN        TO W-CD-RTN
      *     STRING 'S500V-S218R ' W-CD-RTN ' ' K218R-MSG-RTN
      *         DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *     GOBACK.

      *999160-ERRO-160.
      *    MOVE 160                      TO S500V-CD-RTN
      *    MOVE BDDA833A-CD-EST-MSG-AUTZ TO W-CD-RTN
      *    STRING 'S500V-S833R 'S500V-CD-RTN' Banq. beneficiário não rec
      *        'ebe swift. Escolha outro.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999161-ERRO-161.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999161-ERRO-161.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 161                      TO S500V-CD-RTN
           MOVE K043R-CD-RTN             TO W-CD-RTN
           STRING 'S500V-S043R ' W-CD-RTN ' ' K043R-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999162-ERRO-162.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999162-ERRO-162.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 162                      TO S500V-CD-RTN
           MOVE BCIS051N-CD-RTN          TO W-CD-RTN
           STRING 'S500V BCIK051N - ' W-CD-RTN ' ' BCIS051N-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999163-ERRO-163.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999163-ERRO-163.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 163         TO S500V-CD-RTN
           MOVE 'S500V- MCI titular da conta difere do MCI informado.'
                          TO S500V-TX-MSG-RTN.
           GOBACK.

       999164-ERRO-164.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999164-ERRO-164.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 164       TO S500V-CD-RTN.
           MOVE 05        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Conta informada não é conta depósito'
               ' vinculado.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999165-ERRO-165.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999165-ERRO-165.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 165       TO S500V-CD-RTN.
           MOVE 28        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Natureza exige remetente e beneficiário'
      -    ' iguais.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999166-ERRO-166.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999166-ERRO-166.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 166       TO S500V-CD-RTN.
           MOVE 32        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Natureza exige a conta do beneficiario.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999167-ERRO-167.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999167-ERRO-167.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 167       TO S500V-CD-RTN.
           MOVE 02        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Numero do boleto inválido.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999168-ERRO-168.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999168-ERRO-168.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 168       TO S500V-CD-RTN.
           MOVE 02        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Numero do boleto obrigatorio.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      *999169-ERRO-169.
      *    MOVE 169       TO S500V-CD-RTN.
      *    MOVE 53        TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Ano de emissao ou numero da OB invalido.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999170-ERRO-170.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999170-ERRO-170.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 170       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Ano de emissao e numero da OB obrigatorio.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      *999171-ERRO-171.
      *    MOVE 171       TO S500V-CD-RTN.
      *    MOVE 54        TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Ano de emissao ou numero da OB invalido.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999172-ERRO-172.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999172-ERRO-172.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 172       TO S500V-CD-RTN.
           MOVE 54        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Ano de emissao e numero da OB obrigatorio.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      *999173-ERRO-173.
      *    MOVE 173       TO S500V-CD-RTN.
      *    MOVE 53        TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V OBTSB020 OB não pode ser utilizada.'
      *        '(RC=' W-CD-RTN ')'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999174-ERRO-174.
      *    MOVE 174       TO S500V-CD-RTN.
      *    MOVE 53        TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Valor total da ORPAG MN '
      *        'difere do valor da OB'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999175-ERRO-175.
      *    MOVE 175       TO S500V-CD-RTN.
      *    MOVE 53        TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V ' S042G-MSG-ERRO-PGM
      *        '(RC=' W-CD-RTN ')'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999176-ERRO-176.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999176-ERRO-176.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 176       TO S500V-CD-RTN.
           STRING 'S500V Conta do beneficiario/IBAN '
               'maior 34 caracteres.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999177-ERRO-177.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999177-ERRO-177.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 177       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING 'S500V Não informe numero de OB para o convenio '
               'selecionado.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999178-ERRO-178.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999178-ERRO-178.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 178       TO S500V-CD-RTN.
           STRING 'S500V Codigo SWIFT do favorecido com espaços em '
               'branco.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999179-ERRO-179.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999179-ERRO-179.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 179       TO S500V-CD-RTN.
           STRING 'S500V Codigo SWIFT do favorecido deve conter 8 ou '
               '11 caracteres.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999180-ERRO-180.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999180-ERRO-180.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 28               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o nome do beneficiário.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999181-ERRO-181.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999181-ERRO-181.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE 55               TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Informe o sobrenome do beneficiário.'
               TO S500V-TX-MSG-RTN.
           GOBACK.

       999182-ERRO-182.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999182-ERRO-182.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V S1006 ' MCIS1006-MSG-RET-CODE DELIMITED BY '  '
                            '.(' MCIS1006-RET-CODE     '/'
                                 MCIS1006-SEQ-ERRO     '/'
                                 MCIS1006-SQLCODE      ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999183-ERRO-183.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999183-ERRO-183.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V S1449 ' MCIS1449-MSG-RET-CODE DELIMITED BY '  '
                            '.(' MCIS1449-RET-CODE     '/'
                                 MCIS1449-SEQ-ERRO     '/'
                                 MCIS1449-SQLCODE      ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      *999184-ERRO-184.
      *    MOVE 9999             TO S500V-CD-RTN.
      *    MOVE ZEROS            TO S500V-NR-CMP-ERRO.
      *    STRING 'Emissão não permitida, em desacordo com '
      *        'a IN documento Países e Peculiaridades.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999185-ERRO-185.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999185-ERRO-185.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V País de nascimento não preenchido no '
               'cadastro. Regularize na agência.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999186-ERRO-186.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999186-ERRO-186.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V Nacionalidade não preenchida no '
               'cadastro. Regularize na agência.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999187-ERRO-187.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999187-ERRO-187.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           STRING 'S500V Variação da conta poupança '
               'não informada.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999188-ERRO-188.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999188-ERRO-188.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE  27              TO S500V-NR-CMP-ERRO.
           MOVE K003R-CD-SQL     TO W-CD-SQL.
           MOVE K003R-CD-RTN-PGM TO W-CD-RTN.
           STRING 'S500V S003R ' K003R-MSG-ERRO        DELIMITED BY '  '
                            '.(' W-CD-RTN              '/'
                                 W-CD-SQL              ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999189-ERRO-189.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999189-ERRO-189.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999             TO S500V-CD-RTN.
           MOVE ZEROS            TO S500V-NR-CMP-ERRO.
           MOVE ZEROS            TO W-CD-SQL.
           MOVE BCIK4010-CD-RTN  TO W-CD-RTN.
           IF S500V-CD-TIP-ITCE-REG EQUAL 55
              MOVE 'Cliente sujeito à pré-qualificação IN 95.
      -       'Consulte a transação SISBB CAMBIO 96-41.'
                                 TO S500V-TX-MSG-RTN
           ELSE
              IF S500V-CD-TIP-ITCE-REG EQUAL 20
                 MOVE 'Sujeito à pré-qualificação. Procure sua agência.'
                                 TO S500V-TX-MSG-RTN
              ELSE
                 STRING 'S500V ' BCIK4010-TX-MSG-RTN   DELIMITED BY '  '
                               '.(' W-CD-RTN              '/'
                                    W-CD-SQL              ').'
                  DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
              END-IF
           END-IF
           GOBACK.

       999190-ERRO-190.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999190-ERRO-190.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 190       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor em moeda nacional.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999191-ERRO-191.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999191-ERRO-191.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 191       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor minimo em MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999192-ERRO-192.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999192-ERRO-192.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 192       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor maximo em MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999193-ERRO-193.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999193-ERRO-193.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 193       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do tamanho do numero da conta.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999194-ERRO-194.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999194-ERRO-194.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 194       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor total MOEE.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999195-ERRO-195.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999195-ERRO-195.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 195       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor total MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999196-ERRO-196.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999196-ERRO-196.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 196       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor total tarifa MOEE.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999197-ERRO-197.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999197-ERRO-197.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 197       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo do valor tarifa MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999198-ERRO-198.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999198-ERRO-198.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 198       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor tarifa adicional MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999199-ERRO-199.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999199-ERRO-199.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 199       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor tarifa mensagem MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999200-ERRO-200.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999200-ERRO-200.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 200       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor total tarifa MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999201-ERRO-201.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999201-ERRO-201.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 201       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor despesa externa MOEN.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

      *999202-ERRO-202.
      *    MOVE 202       TO S500V-CD-RTN.
      *    MOVE ZEROS     TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Erro no calculo valor despesa externa MOEE.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

      *999203-ERRO-203.
      *    MOVE 203       TO S500V-CD-RTN.
      *    MOVE ZEROS     TO S500V-NR-CMP-ERRO.
      *    STRING 'S500V Erro no calculo valor despesa externa MOEE.'
      *        DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
      *    GOBACK.

       999204-ERRO-204.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999204-ERRO-204.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 204       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor disponivel proposta.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999205-ERRO-205.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999205-ERRO-205.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 205       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor em moeda nacional.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999206-ERRO-206.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999206-ERRO-206.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 206       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V Erro no calculo valor em moeda nacional.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999207-ERRO-207.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999207-ERRO-207.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 207       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' BCIK4015-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999208-ERRO-208.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999208-ERRO-208.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 208       TO S500V-CD-RTN.
           MOVE 19        TO S500V-NR-CMP-ERRO.
           STRING 'S500V - Tipo de comprador/vendedor incompativel '
                  'para o cliente.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999209-ERRO-209.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999209-ERRO-209.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 209       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' BCIS1113-MSG-ERRO
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999210-ERRO-210.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999210-ERRO-210.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 210       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' S035R-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999211-ERRO-211.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999211-ERRO-211.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 211       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Composicao de natureza nao cadastrada no OPE-84.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999212-ERRO-212.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999212-ERRO-212.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 212       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           IF  S500V-CD-TIP-ITCE-REG EQUAL 55
               STRING
              'Código identificação complementar do banco inválido. '
              'Utilizar //FW mais código númerico com 9 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           ELSE
               STRING ' S500V '
              'Código compensação inválido. '
              'Utilizar //FW mais código númerico com 9 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           END-IF
           GOBACK.

       999213-ERRO-213.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999213-ERRO-213.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 213       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           IF  S500V-CD-TIP-ITCE-REG EQUAL 55
               STRING
              'Código identificação complementar do banco inválido. '
              'Utilizar //CP mais código númerico com 4 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           ELSE
               STRING ' S500V '
              'Código compensação inválido. '
              'Utilizar //CP mais código númerico com 4 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           END-IF
           GOBACK.

       999214-ERRO-214.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999214-ERRO-214.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 214       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           IF  S500V-CD-TIP-ITCE-REG EQUAL 55
               STRING
              'Código identificação complementar do banco inválido. '
              'Utilizar //CH mais código númerico com 6 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           ELSE
               STRING ' S500V '
              'Código compensação inválido. '
              'Utilizar //CH mais código númerico com 6 dígitos.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           END-IF
           GOBACK.

       999215-ERRO-215.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999215-ERRO-215.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 215       TO S500V-CD-RTN.
           MOVE ZEROS     TO S500V-NR-CMP-ERRO.
           IF  S500V-CD-TIP-ITCE-REG EQUAL 55
               STRING
              'Código identificação complementar do banco inválido. '
              '//FW(FedWire/ABA),//CP(Chips) ou //CH(Chips/UID).'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           ELSE
               STRING ' S500V '
              'Código compensação inválido.'
              '//FW(FedWire/ABA),//CP(Chips) ou //CH(Chips/UID).'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN
           END-IF
           GOBACK.

       999216-ERRO-216.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999216-ERRO-216.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 216       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Código do tipo de vinculação da OBK não informado.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999217-ERRO-217.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999217-ERRO-217.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 217       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Ordens bancárias informadas não podem ser iguais.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999218-ERRO-218.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999218-ERRO-218.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 218       TO S500V-CD-RTN.
           MOVE 047       TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Nome do estado Western Union inválido.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999219-ERRO-219.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999219-ERRO-219.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 219                 TO S500V-CD-RTN.
           MOVE OPES051U-CD-RTN-PGM TO W-CD-ERRO.
           MOVE 047                 TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Erro ao consultar estado Western Union('
               W-CD-ERRO  ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999220-ERRO-220.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999220-ERRO-220.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 220       TO S500V-CD-RTN.
           MOVE 030       TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Nome da cidade Western Union inválido.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999221-ERRO-221.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999221-ERRO-221.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 221                 TO S500V-CD-RTN.
           MOVE OPES051U-CD-RTN-PGM TO W-CD-ERRO.
           MOVE 030                 TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Erro ao consultar cidade Western Union('
               W-CD-ERRO  ').'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999222-ERRO-222.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999222-ERRO-222.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 222                 TO S500V-CD-RTN.
           MOVE 030                 TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Estado do beneficiário não informado.'
                                   TO S500V-TX-MSG-RTN.
           GOBACK.

      *999223-ERRO-223.
      *    MOVE 223                 TO S500V-CD-RTN
      *    STRING 'S500V Erro ' BDDSBLOQ-CD-ERRO DELIMITED BY SIZE
      *            BDDSBLOQ-TX-ERRO DELIMITED BY SIZE
      *                           INTO S500V-TX-MSG-RTN
      *    GOBACK.

      *999224-ERRO-224.
      *    MOVE 224                 TO S500V-CD-RTN
      *    STRING 'S500V Banqueiro '              DELIMITED BY SIZE
      *            BDDSBLOQ-CD-SWFT-ENT           DELIMITED BY SPACES
      *           ' sem relacionamento com o BB.' DELIMITED BY SIZE
      *                           INTO S500V-TX-MSG-RTN
      *    GOBACK.

       999225-ERRO-225.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999225-ERRO-225.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 225       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Obrigatório informar número da OBK da tarifa.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999226-ERRO-226.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999226-ERRO-226.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 226       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Obrigatório informar número da OBK da ordem.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999227-ERRO-227.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999227-ERRO-227.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 227       TO S500V-CD-RTN.
           MOVE 53        TO S500V-NR-CMP-ERRO.
           STRING
              'S500V Tipos de vinculação das ordens bancárias não podem'
              ' ser iguais.' DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999228-ERRO-228.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999228-ERRO-228.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 9999  TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE 'S500V Erro na subrotina SBCURDAT.' TO S500V-TX-MSG-RTN.
           GOBACK.

       999229-ERRO-229.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999229-ERRO-229.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 229             TO S500V-CD-RTN.
           MOVE BCIS4000-CD-RTN TO W-CD-RTN.
           STRING 'S500V BCIS4000 - ' W-CD-RTN ' ' BCIS4000-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.

       999230-ERRO-230.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999230-ERRO-230.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 230                 TO S500V-CD-RTN.
           MOVE ZEROS               TO S500V-NR-CMP-ERRO.
           EVALUATE S500V-CD-TIP-ITCE-REG
              WHEN 03
                 MOVE 'S500V Natureza não permitida p/ mesma titularidad
      -               'e.'          TO S500V-TX-MSG-RTN
              WHEN 12
              WHEN 20
                 MOVE 'S500V Finalidade não permitida p/ mesma titularid
      -               'ade.'        TO S500V-TX-MSG-RTN
              WHEN OTHER
                 MOVE 'S500V Motivo/finalidade não permitido p/ mesma ti
      -               'tularidade.' TO S500V-TX-MSG-RTN
           END-EVALUATE.
           GOBACK.

       999231-ERRO-231.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999231-ERRO-231.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      * -- ATENCAO!!! - Nao alterar o texto da mensagem abaixo!!
           MOVE 231                 TO S500V-CD-RTN.
           STRING 'Banco selecionado indisponível-Procure sua AGÊNCIA'
             DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           PERFORM 999-999-GRAVA-LOG-HLP
           GOBACK.

       999232-ERRO-232.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999232-ERRO-232.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 232             TO S500V-CD-RTN.
           MOVE 18              TO S500V-NR-CMP-ERRO.
           STRING 'S500V ' K0080-TX-MSG-RTN
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.
      *
       999233-ERRO-233.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999233-ERRO-233.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 233   TO S500V-CD-RTN.
           MOVE ZEROS TO S500V-NR-CMP-ERRO.
           MOVE RETURN-CODE TO W-CD-RTN.
           STRING 'S500V - ' W-CD-RTN ' - Erro na subrotina SBTIMEDT.'
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.
      *
       999234-ERRO-234.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999234-ERRO-234.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 234             TO S500V-CD-RTN.
           MOVE K003R-CD-RTN-PGM TO W-CD-RTN.
           STRING 'S500V BCIS003R - ' W-CD-RTN ' ' K003R-MSG-ERRO
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.
      *
       999235-ERRO-235.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999235-ERRO-235.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 235                 TO S500V-CD-RTN.
           STRING 'S500V - Tipo de comprador/vendedor não permitido. '
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.
      *
       999236-ERRO-236.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999236-ERRO-236.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           MOVE 236                    TO S500V-CD-RTN.
           MOVE BCIS204U-CD-RTN-PGM    TO W-CD-RTN.
           STRING 'S500V BCIS204U - ' W-CD-RTN ' ' BCIS204U-MSG-ERRO
               DELIMITED BY SIZE INTO S500V-TX-MSG-RTN.
           GOBACK.
      *
       999900-ERRO-DB2.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Chegou 999900-ERRO-DB2.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           STRING
               'S500V Erro DB2 ao acessar tabela ' DELIMITED BY SIZE
               W-NM-TAB                    DELIMITED BY ' '
               '. SQL= '                   DELIMITED BY SIZE
               W-CD-SQL                    DELIMITED BY SIZE
               '.'                         DELIMITED BY SIZE
               INTO  S500V-TX-MSG-RTN.
           GOBACK.
      *----------------------------------------------------------------*
       999-999-GRAVA-LOG-HLP                     SECTION.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Entrou 999-999-GRAVA-LOG-HLP.'
                      .
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
      *----------------------------------------------------------------*
           INITIALIZE L-BCISLOG0
      *
           MOVE 'OPE'                          TO KLOG0-CD-SIS-OGM
      *
           IF CD-PGM-CHMD NOT EQUAL SPACES
              MOVE CTE-PROG(5:8)               TO KLOG0-CD-PGM-CHMR
              MOVE CD-PGM-CHMD                 TO KLOG0-CD-PGM-CASD
           ELSE
              MOVE CTE-PROG(5:8)               TO KLOG0-CD-PGM-CASD
           END-IF
      *
           MOVE S500V-TX-MSG-RTN               TO KLOG0-TX-MSG-ERR
      *
           IF S500V-CD-RTN EQUAL 29
              MOVE 'Tentativa de envio de ordem para banqueiro inibido.'
                                               TO KLOG0-TX-MSG-ERR
              MOVE 'Banqueiro Inibido'         TO KLOG0-NM-VRV (01)
              MOVE S500V-CD-SWFT-BNFC(1:8)     TO KLOG0-CTU-VRV(01)
           END-IF
      *
           MOVE 'S500V-CD-RTN'                 TO KLOG0-NM-VRV    (02)
           MOVE  S500V-CD-RTN                  TO KLOG0-CTU-NRC-01(02)
           MOVE 'S500V-TX-MSG-RTN'             TO KLOG0-NM-VRV    (03)
           MOVE  S500V-TX-MSG-RTN              TO KLOG0-CTU-VRV   (03)
           MOVE 'S500V-CD-USU'                 TO KLOG0-NM-VRV    (04)
           MOVE  S500V-CD-USU                  TO KLOG0-CTU-VRV   (04)
           MOVE 'S500V-CD-IDFR-ORD-PGTO'       TO KLOG0-NM-VRV    (05)
           MOVE  S500V-CD-IDFR-ORD-PGTO        TO KLOG0-CTU-NRC-01(05)
           MOVE 'S500V-CD-TIP-OPR'             TO KLOG0-NM-VRV    (06)
           MOVE  S500V-CD-TIP-OPR              TO KLOG0-CTU-VRV   (06)
           MOVE 'S500V-CD-CLI'                 TO KLOG0-NM-VRV    (07)
           MOVE  S500V-CD-CLI                  TO KLOG0-CTU-NRC-01(07)
           MOVE 'S500V-CD-TIP-MC'              TO KLOG0-NM-VRV    (08)
           MOVE  S500V-CD-TIP-MC               TO KLOG0-CTU-VRV   (08)
           MOVE 'S500V-CD-IDFR-MT103'          TO KLOG0-NM-VRV    (09)
           MOVE  S500V-CD-IDFR-MT103           TO KLOG0-CTU-VRV   (09)
           MOVE 'S500V-CD-IDFR-MT202'          TO KLOG0-NM-VRV    (10)
           MOVE  S500V-CD-IDFR-MT202           TO KLOG0-CTU-VRV   (10)
           MOVE 'S500V-CD-NTZ'                 TO KLOG0-NM-VRV    (11)
           MOVE  S500V-CD-NTZ                  TO KLOG0-CTU-NRC-01(11)
           MOVE 'S500V-DT-MVT'                 TO KLOG0-NM-VRV    (12)
           MOVE  S500V-DT-MVT                  TO KLOG0-CTU-VRV   (12)
           MOVE 'S500V-DT-BLQ'                 TO KLOG0-NM-VRV    (13)
           MOVE  S500V-DT-BLQ                  TO KLOG0-CTU-VRV   (13)
           MOVE 'S500V-CD-PRF-DEPE-EMT'        TO KLOG0-NM-VRV    (14)
           MOVE  S500V-CD-PRF-DEPE-EMT         TO KLOG0-CTU-NRC-01(14)
           MOVE 'S500V-VL-MOEE'                TO KLOG0-NM-VRV    (15)
           MOVE  S500V-VL-MOEE                 TO KLOG0-CTU-NRC-02(15)
           MOVE 'S500V-CD-INST-DST'            TO KLOG0-NM-VRV    (16)
           MOVE  S500V-CD-INST-DST             TO KLOG0-CTU-NRC-01(16)
           MOVE 'S500V-CD-PRF-DEPE-OPR'        TO KLOG0-NM-VRV    (17)
           MOVE  S500V-CD-PRF-DEPE-OPR         TO KLOG0-CTU-NRC-01(17)
           MOVE 'S500V-CD-CPSO'                TO KLOG0-NM-VRV    (18)
           MOVE  S500V-CD-CPSO                 TO KLOG0-CTU-VRV   (18)
           MOVE 'S500V-NM-BCO-BNFC'            TO KLOG0-NM-VRV    (19)
           MOVE  S500V-NM-BCO-BNFC             TO KLOG0-CTU-VRV   (19)
           MOVE 'S500V-NM-PRCA-DST'            TO KLOG0-NM-VRV    (20)
           MOVE  S500V-NM-PRCA-DST             TO KLOG0-CTU-VRV   (20)
           MOVE 'S500V-NM-HDNG-DST'            TO KLOG0-NM-VRV    (21)
           MOVE  S500V-NM-HDNG-DST             TO KLOG0-CTU-VRV   (21)
           MOVE 'S500V-NM-BNFC'                TO KLOG0-NM-VRV    (22)
           MOVE  S500V-NM-BNFC                 TO KLOG0-CTU-VRV   (22)
           MOVE 'S500V-TX-END-BNFC'            TO KLOG0-NM-VRV    (23)
           MOVE  S500V-TX-END-BNFC             TO KLOG0-CTU-VRV   (23)
           MOVE 'S500V-NM-CID-BNFC'            TO KLOG0-NM-VRV    (24)
           MOVE  S500V-NM-CID-BNFC             TO KLOG0-CTU-VRV   (24)
           MOVE 'S500V-CD-PAIS-BNFC'           TO KLOG0-NM-VRV    (25)
           MOVE  S500V-CD-PAIS-BNFC            TO KLOG0-CTU-NRC-01(25)
           MOVE 'S500V-NM-PAIS-BNFC'           TO KLOG0-NM-VRV    (26)
           MOVE  S500V-NM-PAIS-BNFC            TO KLOG0-CTU-VRV   (26)
           MOVE 'S500V-CD-MOE'                 TO KLOG0-NM-VRV    (27)
           MOVE  S500V-CD-MOE                  TO KLOG0-CTU-NRC-01(27)
           MOVE 'S500V-CD-PGDR-DSP-EXNO'       TO KLOG0-NM-VRV    (28)
           MOVE  S500V-CD-PGDR-DSP-EXNO        TO KLOG0-CTU-VRV   (28)
           MOVE 'S500V-CD-SWFT-CBT'            TO KLOG0-NM-VRV    (29)
           MOVE  S500V-CD-SWFT-CBT             TO KLOG0-CTU-VRV   (29)
           MOVE 'S500V-CD-SWFT-CRS'            TO KLOG0-NM-VRV    (30)
           MOVE  S500V-CD-SWFT-CRS             TO KLOG0-CTU-VRV   (30)
           MOVE 'S500V-CD-SWFT-INT'            TO KLOG0-NM-VRV    (31)
           MOVE  S500V-CD-SWFT-INT             TO KLOG0-CTU-VRV   (31)
           MOVE 'S500V-TX-DET-PGTO'            TO KLOG0-NM-VRV    (32)
           MOVE  S500V-TX-DET-PGTO             TO KLOG0-CTU-VRV   (32)
           MOVE 'S500V-IN-ENV-SWFT'            TO KLOG0-NM-VRV    (33)
           MOVE  S500V-IN-ENV-SWFT             TO KLOG0-CTU-VRV   (33)
           MOVE 'S500V-CD-CT-BNFC'             TO KLOG0-NM-VRV    (34)
           MOVE  S500V-CD-CT-BNFC              TO KLOG0-CTU-VRV   (34)
           MOVE 'S500V-TX-ITC-ADC-BNF'         TO KLOG0-NM-VRV    (35)
           MOVE  S500V-TX-ITC-ADC-BNF          TO KLOG0-CTU-VRV   (35)
           MOVE 'S500V-CD-TIP-ITCE-REG'        TO KLOG0-NM-VRV    (36)
           MOVE  S500V-CD-TIP-ITCE-REG         TO KLOG0-CTU-NRC-01(36)
      *
           MOVE LENGTH OF L-BCISLOG0 TO EIBCALEN
           CALL BCISLOG0 USING DFHEIBLK L-BCISLOG0
           .
       999-999-FIM.
      *----------- DELECAO AUTOMATICA - INICIO  >>>>>>>>>>>>>>>>>>>
           DISPLAY 'OPES500V - Saiu   999-999-FIM.'
                      .
       999-999-FIM-S.
      *----------- DELECAO AUTOMATICA  -  FIM   <<<<<<<<<<<<<<<<<<<
           EXIT.
      *
      *
      *----------------------------------------------------------------*
      * VRS DATA     ACAO   RESP.    ALTERACAO
      * --- -------- ------ -------- -----------------------------------
      * 094 18052022 503580 Geovanne Retirar trava para cubano
      * 093 07032022 Manut  Geovanne Retirar MOVE indevido na variavel
      *                              W5900-CD-EST-PRTC-S03
      * 092 26012022 Manut  Geovanne Incluir tratamento do erro do OPR
      *                              quando se referir ao EIFSB018
      * 091 01062021 256523 Herbert  Nao envia cobertura/correspondente
      *                              quando a moeda for dolar e o
      *                              banqueiro for BRASUS33, BRASUS3M
      *                              ou CITIUS33
      *                              Historia ALM 1613187
      * 090 09042021 254435 F1680299 Inibir utilizacao de banqueiros
      *                              com base em parâmetro da tabela
      *                              DB2BCI.LDR_SIS.
      * 089 14122020 249516 Herbert  Quando for interface 3 (Sisbb) e
      *                              moeda dolar/euro, as GECEX's irao
      *                              informar o banqueiro destinatario
      *                              Historia 1492121
      *                              Nao envia informacoes de PAYPRO no
      *                              campo 72 da mensagem Swift
      * 088 25112020 248666 Herbert  Quando o pais destinatario for Cuba
      *                              e moeda for Euro, o banqueiro desti
      *                              natario sera BB Frankfurt
      *                              Quando cliente cubano ou nacionali-
      *                              zado cubano e moeda Euro, banqueiro
      *                              destinatario sera BB Frankfurt
      * 087 23112020 Manut. Herbert  Corrige remessas/ordens em Euros
      *                              para enviar banqueiro destinatário
      *                              igual a CHASDEFX.
      *     23112020 Manut. F1680299 Versiona o módulo e análise do
      *                              problema do banqueiro destinatário
      *                              do Dólar (USD).
      * 086 21102020 246721 Herbert  1-Altera chamada subrotina BCIS921S
      *                              para BCIS921T
      *                              2-Quando moeda dolar utiliza
      *                              CITIUS33 como destinatario
      *                              3-Remessas em EURO devem ser envia-
      *                              das para CHASDEFX. Nao utilizar
      *                              mais BRASDEFF
      * 085 21072020 239573 Jonathan Circular BACEN 4002. Recompilado
      *                              por alteracao BECKO320
      * 084 20072020 Manut F3867778 Manda data do debito para data da
      *                             reserva BECSO103
      * 083 13072020 manut C1038435 Inclui tratamento para canal 15
      *                              no retorno de erro BCIS4010.
      *                              (RDI20200028794)
      *     30062020 241596 Herbert  Corrige erro banqueiro quando
      *                              interface 12
      * 082 20122019 230392 C1038435 Correção tratamento p/ caracteres
      *                              inválidos para nome do tomador.
      * 081 18122019 230392 C1038435 Incluir tratamento para caracteres
      *                              inválidos para nome do tomador.
      * 080 11/11/19 228243 F2812778 Inclui tratamento TOTAL PROTECT
      * 079 18/10/19 227668 F6798146 Não permite nome do remetente igual
      *                              ao  nome  do  beneficiário  para as
      *                              naturezas 37004 e 37011.  Exigência
      *                              do BACEN.
      * 078 23/08/19 219987 Herbert  Ajustes no tratamento de intermedia
      *                              cao 3 (Ordens Bancarias Tesouro)
      * 077 09/05/19 216092 Luis M.  Movimenta cliente, agencia/conta e
      *                              interface para OPEP802S
      *                              Verifica se algum banqueiro envol-
      *                              vido na ordem encontra-se impedido
      *              220315          de operar com o BB
      *              221808 F1680299 Envia SG-PAIS-BNFC para o OPEP802S
      * 076 14/12/18 209228 C1038435 Verifica duplicidade p/ OBK tarifa
      *     26/11/18        Herbert  Inclui tratamento para OBK tarifa
      * 075 11/12/18 Manut. Elias    Inclui tratamento CD-CPSO p/ EUA
      * 074 16/11/18 Manut. Ricardo  Inclui tratamento do canal: 15.
      * 073 27/08/18 Manut. Herbert  Define banqueiro caixa como
      *                              destinatario quando interface <> 3
      *     24/08/18 Manut. Herbert  Tratamento para o ERRO-138 (mais de
      *                              um banco encontrado) para mobile
      *     21/08/18 Manut. Herbert  Corrige 710-600-TRATA-BCO-BNFC p/
      *                              regularizar operacoes seguro
      * 072 26/07/18 199229 Herbert  Envia interface para OPES035R
      *                              Valida cliente/natureza da operacao
      *                              com parametrizador do OPE
      * 071 23/04/18 191527 F3134833 Trata banqueiro quando swift
      *                              informado não constar no livro de
      *                              banqueiro, utiliza-se o banqueiro
      *                              caixa como destinatario. Canal
      *                              plataforma e mobile.
      * 070 06/03/18 Manut  F2812778 Tratamento do erro 43
      * 069 06/03/18 Manut  F2812778 Corrigir tratamento banco benefic.
      * 068 23/01/18 Manut  F1680299 Volta a versão 66 (ou versão 02.47
      *                               no Endevor).
      * 067 09/01/18 Manut  RClaudio Corrigir IF S500V-CD-CNL na section
      *                              710-600-TRATA-BCO-BNFC
      * 066 22/12/17 Manut  Luis M.  Corrige validacao de dependencia
      *                              para ordem da plafatorma(55)
      * 065 02/10/17 Manut  Amarildo Não valida regras para interface
      *                              23 - Web-service
      *                     Guilherm Chama BCIS4015 para verificar
      *                              se conta-corrente e conta facil PJ
      *                     Herbert  Altera mensagem de erro do matriz
      *                              de risco quando plataforma.
      * 064 20/09/17 182464 Joaquim  Tratamento para banqueiro não
      *                               cadastrado no livro SWIFT
      * 063 03/05/17 169233 Herbert  Libera chamada ao modulo BCIS4010
      *                              (Matriz de Risco)
      * 062 28/03/17 169233 Herbert  Projeto Matriz de Risco
      *     13/03/17 178699 Herbert  Corrige tamanho do campo CD-NSS-NR
      *                              para numerico de 17 posicoes
      *     17/11/16 172257 Herbert  Complemento de informacoes para WU
      * 061 16/09/16 148487 Herbert  Retira chamada SBVERSAO
      * 060 27/05/16 161382 F5116458 Ativa bloqueio emissao para
      *                              cubanos.
      * 059 13/05/16 MANUT  F5116458 Para Western Union verifica se
      *                              nome e sobrenome do beneficiario
      *                              estão preenchidos.
      *              161382 F5116458 Bloqueia emissão de ordem de paga-
      *                              mento para cubanos.
      *              MANUT  F5116458 Altera mensagem do erro 133.
      *              MANUT  F5116458 Para intermediacao 6-Poupanca
      *                              valida se numero da conta informa-
      *                              da possui variacao.
      *              161306 F5116458 Inclui tipo do cliente na chama-
      *                              da do OBT para a funcao 6.
      * 058 29/03/16 148487 Herbert  Verifica se o codigo swift do bene-
      *                              ficiario possui espacos em branco.
      *     16/02/16 148487 Herbert  Ignora erro da rotina BDDK142X quan
      *                              do processamento batch
      * 057 27/01/16 155418 Herbert  Quando intermediacao 3 (OBT) e co-
      *                              digo convenio 2 (INSS), nao devera
      *                              ser informado o numero da OB.
      * 056 15/01/16 MANUT  F5116458 Altera validacao do codigo de com-
      *                              pensacao somente p/ convenio
      *                              1-BB Americas.
      * 055 16/12/15 153330 F5116458 (JONATHAN) Inclui codigo de compen-
      *                              sacao para ordens BB Americas.
      *              153332 F5116458 (JONATHAN) Permite utilizar pais
      *                              105-BRASIL quando ordem SWIFT
      *                              com natureza de seguro.
      * 054 05/11/15 151305 F5116458 (JONATHAN) Valida se pais eh dife-
      *                              rente de 105-BRASIL para ordens
      *                              SWIFT e WESTERN UNION.
      *     01/07/15 98387  F5116458 (JONATHAN) Liquidacao automatica
      *                              OBT.
      *                     MANUT    (JONATHAN) Inclui validacao de
      *                              tamanho maximo da conta do benefi-
      *                              ciario no exterior.
      *     01/07/15 129967 Herbert  Inclui CD-NTZ = 14 para ordens
      *                              enviadas via processamento lote
      *                              Permite exclusao de ordens que es-
      *                              tejam com IN-CNFC = 'L' (ordens
      *                              geradas pelo processamento lote
      * 053 15122014 133039 C1038435 RECOMPILACAO - ALTERACAO NO BOOK
      *                              OPEK043R. DE:112446 (HERBERT)
      * 052 20/11/14 133851 F3867778 (Guilherme) Chama BCIS051N para
      *                              verificar regra 198 (beneficiario
      *                              e remetente devem ser iguais)
      * 051 03/02/14 117491 F5116458 (JONATHAN) Valida se o titular
      *                              razao eh 3102735XX, se a conta
      *                              inicia com 31027 e se o MCI DEB
      *                              eh igual ao MCI do cliente informa-
      *                              do na intermediacao CONTA VINCULADA
      * 050 04/07/14 Manut  POcca    Corrige DSP-EXNO que está sendo
      *                              cobrada indev quando há flxz total
      * 049 23/01/14 117871 Geovanne Inclui campo S500V-CD-TIP-VCL-ATZD
      * 048 03/12/13 115303 Jurandir Alteração regra de validação de
      *                              natureza Bacen.
      * 047 11/11/13 115694 Geovanne Corrige tratamento CD-CNL='B'
      *     11/11/13 Manut  Geovanne Acerta calculo tarifa MN WSUN
      * 046 10/09/13 114260 FELIPPE  Ajuste para exec batch OPE INSS
      * 045 10/09/13 Manut  Guilherm Verifica se data de emissao esta
      *                              preenchida para recuperar convenio
      * 044 15/07/13 Manut  Elias    Altera tratamento convenio INTERNET
      * 043 16/05/13 101086 Geovane  Incluir tratamento para convenio
      * --- -------- ------ -------- -----------------------------------
      * 042 26/04/13 Manut Geovanne Inibir PERFORM 710-010-VRFR-RCBD-SWF
      *                             RDI: 20130015308
      * 041 25/04/13 Manut Guilherm Para canal INTERNET, calcula despesa
      *                             externa apos validacao das institui-
      *                             coes financeiras
      * 040 04/04/13 105255 Karina  Tratamento para banqueiro não apto
      *                             a receber mensagem swift
      * 039 13/12/12 101295Guilherm Tratamento para banco beneficiario
      *                             igual a BB Americas
      *                             Recupera valor da despesa externa
      *                             por banqueiro RECEBEDOR
      * 038 10/11/11 83738 George   Nao permite emissao para país com
      *                             restrição na moeda da ordem, quando
      *                             tipo ordem 1 (Swift) ou 2 (Seguro)
      * 037 10/11/11 83690 Herbert  Nao permite emissao de ordem com vlr
      *                             moeda estrangeira menor do que 1.00
      * 036 06/10/11 74693 Geovanne Compila como BATCH/ONLINE
      * 035 27/10/11 Manut Guilherm Retira clausula ON SIZE ERROR dos
      *                             calculos devido a inconsistencia
      *                             nas operacoes
      * 034 05/10/11 69425 Amarildo Altera forma de entrega de 40 para
      *                             20.
      * 033 23/05/11 77131 Amarildo Inclui campos S500V-IN-OBGD-REG-BC e
      *                             S500V-CD-TIP-CTR-CMB, necessarios ao
      *                             sistema de mensageria.
      *                             Trata opção 11B com mensageria ativa
      *                             Trata opção 11C com mensageria ativa
      *                             Trata opção 13A com mensageria ativa
      * 032 24/09/10 66490 Geovanne Retirar acesso direto tabela BDD
      *             Manut Edison    Trata movimentacao CD-DOC-TMDR
      *                             para tipo de doc fora da faixa
      *                             definida
      * 031 01/07/10 Manut Edison   Inclui tratamento para estado e cep
      *                             do bnfc qdo pais = 249 ou 493
      *     14/05/10 65419 Amarildo Contrato Prévio - CCA.
      *              Manut R.Midori Acerta envio da instituição para pes
      *                             quisa poupex W-CD-PRD-OPRS5900=644.
      * 030 13/05/10 Manut Guilherm Move SPACES para W-NM-AJSD para evi-
      *                             tar erro quando cliente nao possui
      *                             sobrenome
      * 029 11/03/10 60586 Elias    Acerta tratamento questao de
      *                             verificação.
      * 028 17/03/10 Manut Marlos   Frankfurt opcional como DSTR 978
      *                             Altera retorno erro section 999090
      * 027 09/03/10 Manut Elias    Acerta tratamento de dependencia
      * 026 05/03/10 Manut Elias    Acerta retorno da dependencia exec
      * 025 08/02/10 62836 Marlos   Qdo moeda 978 Destinatario=Frankfurt
      *     11/02/10 62567 Marlos   Utiliza IPTF Internet para APJ
      * 024 02/02/10 Manut Elias    Acerta tratamento Banq.Destinatario
      * 023 08/01/10 Manut Marlos   Busca na OPRS5900 Inst 1 todos os
      *                             estados de conta 871-000...
      * 022 16/12/09 Manut Elias    Acerta tratamento banqueiro benef.
      *                             para canal INTERNET.
      * 021 09/12/09 Manut Amarildo Valida conta poupança BB correspon-
      *                             dente BNC no EIFSB014.
      * 020 04/12/09 Manut Amarildo Passa a buscar no OPRS5900 todos os
      *                             estados de conta para o BNC.
      * 019 14/09/09 57064 Amarildo Tratamento BNC - Banco Nossa Caixa
      *     27/08/09 55961 Amarildo Migração sistema TBC - IOF
      * 018 31/07/09 57116 Amarildo Retira trava envio Swift para pais
      *                             favorecido 105 (Brasil)
      *     28/07/09       Elias    Trata canal INTERNET.
      * 017 01/06/09 52230 Elias    Valida contrato de cambio simplif.
      * 016 18/06/09 Manut Luis M.  Ajuste para gravação do primeiro n
      *                             me quando possui apenas 1 nome
      * 015 10/06/09 Manut Luis M.  Evita loop infinito para cliente com
      *                             apenas 1 nome 211-000-AJUSTA-NOME
      * 014 18/02/09 52379 Guilherm Retira verificação de dias úteis no
      *                             Brasil, passando a considerar feria-
      *                             dos também na praça da moeda.
      * 013 30/01/09 Manut Caroline Inclui W5900-CD-TIP-CPF-CGC-IND
      * 012 26/01/09 Manut Caroline Inclui Prd Poupanca (003) e Poupex
      *                             (644) na chamada OPRS5900
      * 011 29/12/08 49860 Edison   Substitui MCISO005 POR OPRS5900
      * 010 12/11/08 Manut Guilherm Troca SELECT para validacao da moeda
      *                             por chamada ao BCIS002R
      * 009 29/07/08 45632 Amarildo Ordem tipo seguro - somente forma de
      *                             entrega 40 e moeda 220.
      *                             Inclui seguradora 34
      * 008 03/03/08 Manut Adriano  Valida moeda=220 para ordens WSUN
      * --- -------- ----- -------- ------------------------------------
      *VRS007 - 26/06/07 EDISON  - RECOMPILA
      *VRS006 - 24/05/07 Adriano - DE 78054 - trata dependencias CABB,
      *                  R.Midori  permitindo executar operaçoes câmbio
      *                          - Substitui CALL por CICS LINK
      *VRS005 - 12/02/07 Adriano - Alteração BOOK OPEK500V
      *VRS004 - 24/11/06 Adriano - Ordem WSUN não permite BEC com data
      *                            liquidação futura
      *VRS003 - 21/11/06 Adriano - Movimenta SO030-CD-TRAN-GRL='CB32'
      *                          - Controla carga lista de banqueiros
      *                          - Altera limite arredondamento
      *VRS002 - 20/11/06 Adriano - Corrige verificação intermediação
      *VRS001 - 08/11/06 Adriano - Desenvolvimento
      *-----------------------------------------------------------------
