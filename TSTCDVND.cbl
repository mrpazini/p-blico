      *>----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *>----------------------------------------------------------------
       PROGRAM-ID.     TSTCDVND.
       AUTHOR.         MILTON ROGERIO PAZINI.
      *>----------------------------------------------------------------
      *>SISTEMA        : TESTE PROGRAMACAO
      *>PROGRAMA       : CADASTRO DE VENDEDOR
      *>----------------------------------------------------------------

      *>----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *>----------------------------------------------------------------
       CONFIGURATION SECTION.
      *>----------------------------------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *>----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      *>----------------------------------------------------------------
       FILE-CONTROL.
      *>----------------------------------------------------------------
       COPY    "CADVND.SEL".
       COPY    "CADVND.SEL"    REPLACING   CADVND      BY  eCADVND
                                           LB-CADVND   BY  LB-eCADVND
                               LEADING     ==VND-==    BY  ==eVND-==.
       COPY    "TXTVND.SEL".

      *>----------------------------------------------------------------
       DATA DIVISION.
      *>----------------------------------------------------------------
       FILE SECTION.
      *>----------------------------------------------------------------
       COPY    "CADVND.FD".
       COPY    "CADVND.FD"     REPLACING   CADVND      BY  eCADVND
                               LEADING     ==VND-==    BY  ==eVND-==.
       COPY    "TXTVND.FD".

      *>----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *>----------------------------------------------------------------
       COPY    "WSSVARS.LIB".

           05  AX-OPCAO                PIC  X(001)     VALUE   SPACES.

       01  LK-DADOS.
           05  LK-CPF.
               10  LK-NUMERO           PIC  9(009).
               10  LK-DIGITO           PIC  9(002).
           05  LK-RET                  PIC  9(001).
               88  LK-OK                               VALUE   1.

       77  AX-CAMPO                    PIC  9(002)     VALUE ZEROS.

      *>----------------------------------------------------------------
       LINKAGE SECTION.
      *>----------------------------------------------------------------

       01  LK-CHAMA                    PIC  9(002).

      *>----------------------------------------------------------------
       SCREEN SECTION.
      *>----------------------------------------------------------------
      $SET SOURCEFORMAT "FREE".
       01  T-MENU.
           05  LINE 01 COLUMN 01   BLANK   SCREEN.
           05  LINE 06 COLUMN 32   VALUE   "Selecione".
           05  LINE 08 COLUMN 32   VALUE   " I - Inclusao".
           05  LINE 10 COLUMN 32   VALUE   " A - Alteracao".
           05  LINE 12 COLUMN 32   VALUE   " E - Exclusao".
           05  LINE 14 COLUMN 32   VALUE   " M - Importacao".
           05  LINE 14 COLUMN 32   VALUE   " X - Finalizar".
           05  LINE 16 COLUMN 32   VALUE   "[ ]  Opcao".
           05  LINE 16 COLUMN 33   PIC Z9  USING AX-OPCAO.

       01  T-DISPLAY.
           05  LINE 01 COLUMN 01   BLANK   SCREEN.
           05  LINE 06 COLUMN 10   VALUE   "Cadastros De Vendedores".
           05  LINE 08 COLUMN 10   VALUE   "Codigo.......:".
           05  LINE 10 COLUMN 10   VALUE   "CPF..........:".
           05  LINE 12 COLUMN 10   VALUE   "Nome.........:".
           05  LINE 14 COLUMN 10   VALUE   "Latitude.....:".
           05  LINE 14 COLUMN 40   VALUE   "Longitude....:".

       01  T-ACCEPT.
           05  T-CODIGO.
               10  LINE 08 COLUMN 25 PIC ZZZZZ9 USING VND-CODIGO.
           05  T-CPF.
               10  LINE 10 COLUMN 25 PIC 999.999.999.99 USING VND-CPF.
           05  T-NOME.
               10  LINE 12 COLUMN 25 PIC X(040) USING VND-NOME.
           05  T-LATI.
               10  LINE 14 COLUMN 25 PIC --9,99999999 USING VND-LATITUDE.
           05  T-LONG.
               10  LINE 14 COLUMN 55 PIC --9,99999999 USING VND-LONGITUDE.

       01  T-ARQUIVO.
           05  LINE 01 COLUMN 01   BLANK SCREEN.
           05  LINE 10 COLUMN 20   VALUE   "Arquivo a Importar".
           05  LINE 11 COLUMN 20   PIC X(050) USING LB-TXTVND.

       01  T-MENS.
           05  LINE 24 COLUMN 01   VALUE "Mensagem:".
           05  LINE 24 COLUMN 10   PIC X(065) FROM     AX-MENSAGEM.
           05  LINE 24 COLUMN 77   PIC X(001) USING    AX-CONF.
      *>----------------------------------------------------------------
       PROCEDURE DIVISION      USING   LK-CHAMA.
      *>----------------------------------------------------------------
       R-INICIAL.
      *>----------------------------------------------------------------
           IF          LK-CHAMA    NOT EQUAL 85
                       STOP        RUN
           END-IF

           PERFORM     UNTIL   AX-OPCAO EQUAL "X"
                       INITIALIZE  AX-OPCAO
                       DISPLAY     T-MENU
                       ACCEPT      T-MENU
                       EVALUATE    AX-OPCAO
                           WHEN    "I"
                                   PERFORM     R-RECEIVER-000
                           WHEN    "A"
                                   PERFORM     R-RECEIVER-000
                           WHEN    "E"
                                   PERFORM     R-EXCLUI
                           WHEN    "M"
                                   PERFORM     R-IMPORTA
                       END-EVALUATE
           END-PERFORM

           GOBACK.

      *>----------------------------------------------------------------
       R-RECEIVER-000.
      *>----------------------------------------------------------------
           MOVE        1                       TO  AX-CAMPO
           DISPLAY     T-DISPLAY
           PERFORM     UNTIL   AX-CAMPO = 0
                       EVALUATE    AX-CAMPO
                           WHEN    01
                                   PERFORM     R-RECEIVER-001
                           WHEN    02
                                   PERFORM     R-RECEIVER-002
                           WHEN    03
                                   PERFORM     R-RECEIVER-003
                           WHEN    04
                                   PERFORM     R-RECEIVER-004
                           WHEN    05
                                   PERFORM     R-RECEIVER-005
                           WHEN    06
                                   PERFORM     R-RECEIVER-006
                       END-EVALUATE
           END-PERFORM

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-001.
      *>----------------------------------------------------------------
           INITIALIZE  VND-REGISTRO
           MOVE        "[ESC] Para Sair"       TO  AX-MENSAGEM
           DISPLAY     T-ACCEPT    T-MENS
           ACCEPT      T-CODIGO ON ESCAPE
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-ACCEPT

           IF          AX-OPCAO EQUAL "A"
                       PERFORM     R-IO-CADVND
                       IF          AX-STATUS NOT EQUAL "00" OR "05"
                                   MOVE        "Erro ao Abri o Arquivo de Clientes![ENTER]"  TO AX-MENSAGEM
                                   DISPLAY     T-MENS
                                   ACCEPT      T-MENS
                                   EXIT
                       END-IF

                       READ        CADVND
                       IF          AX-STATUS NOT EQUAL "00"
                                   MOVE        "Registro não Encontrado! [ENTER]"  TO AX-MENSAGEM
                                   DISPLAY     T-MENS
                                   ACCEPT      T-MENS
                                   EXIT
                       END-IF

                       DISPLAY     T-ACCEPT
           END-IF

           ADD         1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-002.
      *>----------------------------------------------------------------
           MOVE        "[ESC] Para Sair"       TO  AX-MENSAGEM
           DISPLAY     T-MENS
           ACCEPT      T-CPF ON ESCAPE
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-ACCEPT

           IF          VND-CPF     EQUAL ZEROS
                       MOVE        "CPF Obrigatorio! [ENTER]"     TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
                       EXIT
           END-IF

           INITIALIZE  LK-DADOS
           MOVE        VND-CPF                 TO  LK-CPF
           CALL        "CALCCPF"   USING   LK-DADOS
           CANCEL      "CALCCPF"

           IF          NOT LK-OK
                       MOVE        "CPF Informado invalido! [ENTER]"  TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
                       EXIT
           END-IF

           OPEN        INPUT   ECADVND
           IF          AX-STATUS NOT EQUAL "00"
                       MOVE        "Erro ao Abrir eCADVND! [ENTER]"    TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
                       EXIT
           END-IF

           MOVE        VND-CPF                 TO  EVND-CPF
           START       ECADVND     KEY NOT LESS EVND-CHAVE3
           IF          AX-STATUS   NOT EQUAL "00"
                       CLOSE       ECADVND
           ELSE
                       READ        ECADVND NEXT
                       IF          AX-STATUS NOT EQUAL "00"
                                   CLOSE       ECADVND
                       ELSE
                                   IF          EVND-CPF NOT EQUAL VND-CPF
                                               CLOSE       ECADVND
                                   ELSE
                                               MOVE        "CNPJ Ja Cadastrado! [ENTER]"   TO  AX-MENSAGEM
                                               DISPLAY     T-MENS
                                               ACCEPT      T-MENS
                                               CLOSE       ECADVND
                                               EXIT
                                   END-IF
                       END-IF
           END-IF

           ADD         1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-003.
      *>----------------------------------------------------------------
           MOVE        "[ESC] Para Sair"       TO  AX-MENSAGEM
           DISPLAY     T-MENS
           ACCEPT      T-NOME ON ESCAPE
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-ACCEPT

           IF          VND-NOME    EQUAL SPACES
                       MOVE        "Nome Obrigatorio! [ENTER]" TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
                       EXIT
           END-IF

           ADD         1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-004.
      *>----------------------------------------------------------------
           MOVE        "[ESC] Para Sair"       TO  AX-MENSAGEM
           DISPLAY     T-MENS
           ACCEPT      T-LATI ON ESCAPE
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-ACCEPT

           ADD         1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-005.
      *>----------------------------------------------------------------
           MOVE        "[ESC] Para Sair"       TO  AX-MENSAGEM
           DISPLAY     T-MENS
           ACCEPT      T-LONG ON ESCAPE
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-ACCEPT

           ADD         1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-RECEIVER-006.
      *>----------------------------------------------------------------
           MOVE        "Confirma (S/N):"       TO  AX-MENSAGEM
           DISPLAY     T-MENS
           ACCEPT      T-MENS

           IF          AX-CONF NOT EQUAL "s"AND "S" AND "n" AND "N"
                       EXIT
           END-IF

           IF          AX-CONF EQUAL "n" OR "N"
                       SUBTRACT 1 FROM AX-CAMPO
                       EXIT
           END-IF

           PERFORM     R-IO-CADVND
           WRITE       VND-REGISTRO INVALID KEY
               REWRITE VND-REGISTRO

           CLOSE       CADVND

           MOVE        1                       TO  AX-CAMPO

           EXIT.

      *>----------------------------------------------------------------
       R-EXCLUI.
      *>----------------------------------------------------------------
           PERFORM     R-IO-CADVND
           IF          AX-STATUS NOT EQUAL "00" OR "05"
                       EXIT
           END-IF

           PERFORM     UNTIL   EXIT
                       INITIALIZE  VND-REGISTRO
                       MOVE        "[ESC] para Sair"   TO  AX-MENSAGEM
                       DISPLAY     T-DISPLAY   T-ACCEPT    T-MENS
                       ACCEPT      T-CODIGO    ON ESCAPE
                                   EXIT        PERFORM
                       END-ACCEPT

                       READ        CADVND
                       IF          AX-STATUS NOT EQUAL "00"
                                   CLOSE       CADVND
                                   MOVE        "Vendedor não encontrado! [ENTER]"   TO  AX-MENSAGEM
                                   DISPLAY     T-MENS
                                   ACCEPT      T-MENS
                                   EXIT        PERFORM     CYCLE
                       END-IF

                       DISPLAY     T-ACCEPT
                       MOVE        "Confirma a Exclusao do Vendedor? {S/N)"     TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
                       IF          AX-CONF NOT EQUAL "s" AND "S"
                                   EXIT        PERFORM     CYCLE
                       END-IF

                       DELETE      CADVND

           END-PERFORM

           CLOSE       CADVND

           EXIT.

      *>----------------------------------------------------------------
       R-IMPORTA.
      *>----------------------------------------------------------------
           PERFORM     UNTIL   EXIT
                       INITIALIZE  LB-TXTVND
                       MOVE        "ESPACOS PARA SAIR" TO AX-MENSAGEM
                       DISPLAY     T-ARQUIVO   T-MENS
                       ACCEPT      T-ARQUIVO

                       IF          LB-TXTVND   EQUAL SPACES
                                   EXIT        PERFORM
                       END-IF

                       OPEN        INPUT       TXTVND
                       IF          AX-STATUS NOT EQUAL "35"
                                   MOVE        "Arquivo Texto não Localizado! [ENTER]"     TO  AX-MENSAGEM
                                   EXIT        PERFORM     CYCLE
                       END-IF

                       PERFORM     R-IO-CADVND
                       IF          AX-STATUS NOT EQUAL "00" OR "05"
                                   CLOSE       TXTVND
                                   EXIT        PERFORM     CYCLE
                       END-IF

                       PERFORM     UNTIL   EXIT
                                   READ        TXTVND  NEXT    AT END
                                               MOVE        "Final da Importacao! [ENTER]"  TO  AX-MENSAGEM
                                               DISPLAY     T-MENS
                                               ACCEPT      T-MENS
                                               CLOSE       TXTVND      CADVND
                                               EXIT        PERFORM
                                   END-READ

                                   MOVE        TVND-CPF        TO  LK-CPF
                                   CALL        "CALCCPF"      USING   LK-DADOS
                                   CANCEL      "CALCCPF"
                                   IF          NOT LK-OK
                                               STRING      "CPF "     TVND-CPF(1:11)
                                                           " Invalido! Nao sera importado. [ENTER]"
                                                   INTO    AX-MENSAGEM
                                               DISPLAY     T-MENS
                                               ACCEPT      T-MENS
                                               EXIT        PERFORM     CYCLE
                                   END-IF

                                   MOVE        TVND-REGISTRO           TO  VND-REGISTRO
                                   WRITE       VND-REGISTRO    INVALID KEY
                                               REWRITE     VND-REGISTRO

                       END-PERFORM
           END-PERFORM.

           EXIT.

      *>----------------------------------------------------------------
       R-IO-CADVND.
      *>----------------------------------------------------------------
           OPEN        I-O         CADVND
           EVALUATE    AX-STATUS
               WHEN    "39"
                       MOVE        "Estrutura do Arquivo de Vendedores Inválida! [ENTER]"    TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
               WHEN    "9A"
                       MOVE        "Arquivo de Vendedores Bloqueado em Outro Terminal! [ENTER]"  TO  AX-MENSAGEM
                       DISPLAY     T-MENS
                       ACCEPT      T-MENS
           END-EVALUATE

           EXIT.

      *>----------------------------------------------------------------
