       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEE-1024.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Nao existe como acessar a tabela ASCII no COBOL (não que eu
      * conheça, por isso montei uma variavel com os caracteres e sua
      * posicao correspondente a esta codificação.
       01 CARACTERES.
      *    01
           03 FILLER PIC X(01) VALUE SPACE.
      *    02
           03 FILLER PIC X(01) VALUE "!".
      *    03
           03 FILLER PIC X(01) VALUE '"'.
      *    04
           03 FILLER PIC X(01) VALUE "#".
      *    05
           03 FILLER PIC X(01) VALUE "$".
      *    06
           03 FILLER PIC X(01) VALUE "%".
      *    07
           03 FILLER PIC X(01) VALUE "&".
      *    08
           03 FILLER PIC X(01) VALUE "'".
      *    09
           03 FILLER PIC X(01) VALUE "(".
      *    10
           03 FILLER PIC X(01) VALUE ")".
      *    11
           03 FILLER PIC X(01) VALUE "*".
      *    12
           03 FILLER PIC X(01) VALUE "+".
      *    13
           03 FILLER PIC X(01) VALUE ",".
      *    14
           03 FILLER PIC X(01) VALUE "-".
      *    15
           03 FILLER PIC X(01) VALUE ".".
      *    16
           03 FILLER PIC X(01) VALUE "/".
      *    17
           03 FILLER PIC X(01) VALUE "0".
      *    18
           03 FILLER PIC X(01) VALUE "1".
      *    19
           03 FILLER PIC X(01) VALUE "2".
      *    20
           03 FILLER PIC X(01) VALUE "3".
      *    21
           03 FILLER PIC X(01) VALUE "4".
      *    22
           03 FILLER PIC X(01) VALUE "5".
      *    23
           03 FILLER PIC X(01) VALUE "6".
      *    24
           03 FILLER PIC X(01) VALUE "7".
      *    25
           03 FILLER PIC X(01) VALUE "8".
      *    26
           03 FILLER PIC X(01) VALUE "9".
      *    27
           03 FILLER PIC X(01) VALUE ":".
      *    28
           03 FILLER PIC X(01) VALUE ";".
      *    29
           03 FILLER PIC X(01) VALUE "<".
      *    30
           03 FILLER PIC X(01) VALUE "=".
      *    31
           03 FILLER PIC X(01) VALUE ">".
      *    32
           03 FILLER PIC X(01) VALUE "?".
      *    33
           03 FILLER PIC X(01) VALUE "@".
      *    34
           03 FILLER PIC X(01) VALUE "A".
      *    35
           03 FILLER PIC X(01) VALUE "B".
      *    36
           03 FILLER PIC X(01) VALUE "C".
      *    37
           03 FILLER PIC X(01) VALUE "D".
      *    38
           03 FILLER PIC X(01) VALUE "E".
      *    39
           03 FILLER PIC X(01) VALUE "F".
      *    40
           03 FILLER PIC X(01) VALUE "G".
      *    41
           03 FILLER PIC X(01) VALUE "H".
      *    42
           03 FILLER PIC X(01) VALUE "I".
      *    43
           03 FILLER PIC X(01) VALUE "J".
      *    44
           03 FILLER PIC X(01) VALUE "K".
      *    45
           03 FILLER PIC X(01) VALUE "L".
      *    46
           03 FILLER PIC X(01) VALUE "M".
      *    47
           03 FILLER PIC X(01) VALUE "N".
      *    48
           03 FILLER PIC X(01) VALUE "O".
      *    49
           03 FILLER PIC X(01) VALUE "P".
      *    50
           03 FILLER PIC X(01) VALUE "Q".
      *    51
           03 FILLER PIC X(01) VALUE "R".
      *    52
           03 FILLER PIC X(01) VALUE "S".
      *    53
           03 FILLER PIC X(01) VALUE "T".
      *    54
           03 FILLER PIC X(01) VALUE "U".
      *    55
           03 FILLER PIC X(01) VALUE "V".
      *    56
           03 FILLER PIC X(01) VALUE "W".
      *    57
           03 FILLER PIC X(01) VALUE "X".
      *    58
           03 FILLER PIC X(01) VALUE "Y".
      *    59
           03 FILLER PIC X(01) VALUE "Z".
      *    60
           03 FILLER PIC X(01) VALUE "[".
      *    61
           03 FILLER PIC X(01) VALUE "\".
      *    62
           03 FILLER PIC X(01) VALUE "]".
      *    63
           03 FILLER PIC X(01) VALUE "^".
      *    64
           03 FILLER PIC X(01) VALUE "_".
      *    65
           03 FILLER PIC X(01) VALUE "`".
      *    66
           03 FILLER PIC X(01) VALUE "a".
      *    67
           03 FILLER PIC X(01) VALUE "b".
      *    68
           03 FILLER PIC X(01) VALUE "c".
      *    69
           03 FILLER PIC X(01) VALUE "d".
      *    70
           03 FILLER PIC X(01) VALUE "e".
      *    71
           03 FILLER PIC X(01) VALUE "f".
      *    72
           03 FILLER PIC X(01) VALUE "g".
      *    73
           03 FILLER PIC X(01) VALUE "h".
      *    74
           03 FILLER PIC X(01) VALUE "i".
      *    75
           03 FILLER PIC X(01) VALUE "j".
      *    76
           03 FILLER PIC X(01) VALUE "k".
      *    77
           03 FILLER PIC X(01) VALUE "l".
      *    78
           03 FILLER PIC X(01) VALUE "m".
      *    79
           03 FILLER PIC X(01) VALUE "n".
      *    80
           03 FILLER PIC X(01) VALUE "o".
      *    81
           03 FILLER PIC X(01) VALUE "p".
      *    82
           03 FILLER PIC X(01) VALUE "q".
      *    83
           03 FILLER PIC X(01) VALUE "r".
      *    84
           03 FILLER PIC X(01) VALUE "s".
      *    85
           03 FILLER PIC X(01) VALUE "t".
      *    86
           03 FILLER PIC X(01) VALUE "u".
      *    87
           03 FILLER PIC X(01) VALUE "v".
      *    88
           03 FILLER PIC X(01) VALUE "w".
      *    89
           03 FILLER PIC X(01) VALUE "x".
      *    90
           03 FILLER PIC X(01) VALUE "y".
      *    91
           03 FILLER PIC X(01) VALUE "z".
      *    92
           03 FILLER PIC X(01) VALUE "{".
      *    93
           03 FILLER PIC X(01) VALUE "|".
      *    94
           03 FILLER PIC X(01) VALUE "}".
      *    95
           03 FILLER PIC X(01) VALUE "~".
      ******************************************************************
      * TABELA ASCII, MAS QUE COMEÇA COM 32 (= 1) ATÉ 127 (=95)
      ******************************************************************
      * Criei uma tabela para que fosse possível acessar esses
      * caracteres de acordo com a posicao.
       01 FILLER REDEFINES CARACTERES.
           03 TABELA-ASCII PIC X(01) OCCURS 95 TIMES
               INDEXED BY IC-TABELA.
       77 ENTRADA PIC X(20).
       77 TAMANHO-ENTRADA PIC 9(02).
       77 CARACTER-ENTRADA PIC X(01).
       77 SAIDA   PIC X(20).
       77 INDICE PIC 9(02).
       77 POSICAO PIC 9(02).
       77 POSICAO-INICIAL PIC 9(02).
       77 POSICAO-FLOAT PIC 9(02)V99.
       77 POSICAO-FINAL PIC 9(02).
       77 RESTO PIC 9(02).
       PROCEDURE DIVISION.
      * Obtendo e tratando a entrada
           ACCEPT ENTRADA FROM CONSOLE
           MOVE FUNCTION TRIM(ENTRADA) TO ENTRADA
      * Obtendo o tamaho da string de entrada
           MOVE LENGTH OF ENTRADA TO TAMANHO-ENTRADA
      * Percorre a string de forma inversa procurando o primeiro
      * caracter que nao seja um espaco. Este sera o tamanho da string.
           PERFORM VARYING INDICE FROM TAMANHO-ENTRADA BY -1 UNTIL
           INDICE < 1
               MOVE ENTRADA(INDICE:1) TO CARACTER-ENTRADA
               IF CARACTER-ENTRADA NOT EQUAL SPACE
                   MOVE INDICE TO POSICAO-FINAL
                   EXIT PERFORM
               END-IF
           END-PERFORM
      * Primeira passada: Desloca os caracteres 3 posicoes para a
      * direita, se forem letras (maiusculas ou minusculas)
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL
           INDICE > TAMANHO-ENTRADA
      * Extracao do caracter da string de entrada
               MOVE ENTRADA(INDICE:1) TO CARACTER-ENTRADA
               SET IC-TABELA TO 0
      * Varre a tabela correspondente a tabela ASCII para obter a
      * posicao
               SEARCH TABELA-ASCII VARYING IC-TABELA
                   WHEN CARACTER-ENTRADA EQUAL TABELA-ASCII(IC-TABELA)
                       MOVE IC-TABELA TO POSICAO
               END-SEARCH
      * Se forem letras (maiuculas ou minusculas), desloca 3 posicoes
      * para a direita. Se nao, passa para a proxima iteracao
               EVALUATE POSICAO
                   WHEN 34 THRU 59
                   WHEN 66 THRU 91
                       ADD 3 POSICAO GIVING POSICAO
                       SET IC-TABELA TO POSICAO
                       MOVE TABELA-ASCII(IC-TABELA) TO ENTRADA(INDICE:1)
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM
      * Segunda passada: inverter a string
           MOVE FUNCTION TRIM(FUNCTION REVERSE(ENTRADA)) TO ENTRADA
      * Obtem o resto da divisao por dois para descobrir se o tamaho
      * da string eh par ou nao
           MOVE FUNCTION MOD(POSICAO-FINAL 2) TO RESTO
      * Se for par, define a posicao inicial da metade da string
           IF RESTO EQUAL 0
               DIVIDE POSICAO-FINAL BY 2 GIVING POSICAO-INICIAL
           ELSE
      * Se nao for par, obtem a metade (float truncada) e adiciona mais
      * um para a posicao inicial da metade
               DIVIDE POSICAO-FINAL BY 2 GIVING POSICAO-FLOAT
               MOVE FUNCTION INTEGER(POSICAO-FLOAT + 1)
               TO POSICAO-INICIAL
           END-IF
      * Terceira passada: todo caracter tem que deslocado uma posicao
      * para a esquerda
      * Loop comeca na metade da string (POSICAO-INICIAL)
           PERFORM VARYING INDICE FROM POSICAO-INICIAL BY 1 UNTIL
           INDICE > TAMANHO-ENTRADA
      * Extrai o caracter
               MOVE ENTRADA(INDICE:1) TO CARACTER-ENTRADA
      * Procura a posicao do caracter na "tabela ASCII"
               SET IC-TABELA TO 0
               SEARCH TABELA-ASCII VARYING IC-TABELA
                   WHEN CARACTER-ENTRADA EQUAL TABELA-ASCII(IC-TABELA)
                       MOVE IC-TABELA TO POSICAO
               END-SEARCH
      * Subtrai a posicao do caracter por um
               SUBTRACT 1 FROM POSICAO
      * Se nao for espaco, desloca para a esquerda
               IF CARACTER-ENTRADA NOT EQUAL SPACE
                   MOVE TABELA-ASCII(POSICAO) TO ENTRADA(INDICE:1)
               END-IF
           END-PERFORM
      * Mostra a saida no final
           DISPLAY ENTRADA
           GOBACK
           .
