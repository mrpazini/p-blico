      *-----------------------------------------------------------------
      * ESTE PROGRAMA NAO PASSOU POR NENHUMA COMPILACAO
      *-----------------------------------------------------------------
        IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
        PROGRAM-ID.     CALCCNPJ
        AUTHOR.         MILTON ROGERIO PAZINI.
      *-----------------------------------------------------------------
      * SISTEMA       : TESTE PROGRAMACAO
      * PROGRAMA      : VALIDACAO DE CNPJ
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
        ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
        CONFIGURATION SECTION.
      *-----------------------------------------------------------------
        SPECIAL-NAMES.
                DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
        INPUT-OUTPUT SECTION.
      *-----------------------------------------------------------------
        FILE-CONTROL.

      *-----------------------------------------------------------------
        DATA DIVISION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
        WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
        COPY    "WSSVARS.LIB".

            05  AX-RESTO                PIC  9(003)     VALUE   ZEROS.
            05  AX-RESULT               PIC  9(018)     VALUE   ZEROS.
            05  AX-RESULT-1             PIC  9(018)     VALUE   ZEROS.
            05  AX-RESULT-2             PIC  9(018)     VALUE   ZEROS.
            05  AX-DIG-CALC             PIC  9(002)     VALUE   ZEROS.
            05  RX-DIG-CALC             REDEFINES       AX-DIG-CALC.
                10  AX-DIG-CALC-1       PIC  9(001).
                10  AX-DIG-CALC-2       PIC  9(001).

            05  AX-CNPJ.
                10  AX-CNPJ-01          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-02          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-03          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-04          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-05          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-06          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-07          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-08          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-09          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-10          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-11          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-12          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-D1          PIC  9(001)     VALUE   ZEROS.
                10  AX-CNPJ-D2          PIC  9(001)     VALUE   ZEROS.

            05  CT-CNPJ.
                10  CT-CNPJ-01          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-02          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-03          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-04          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-05          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-06          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-07          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-08          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-09          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-10          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-11          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-12          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-D1          PIC  9(002)     VALUE   ZEROS.
                10  CT-CNPJ-D2          PIC  9(002)     VALUE   ZEROS.

      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
        01  LK-DADOS.
            05  LK-CNPJ.
                10  LK-NUMERO           PIC  9(012).
                10  LK-DIGITO           PIC  9(002).
            05  LK-RET                  PIC  9(001).
                88  LK-OK                               VALUE   1.

        01  LK-CHAMA                    PIC  9(002).

      *-----------------------------------------------------------------
        PROCEDURE DIVISION      USING   LK-DADOS    LK-CHAMA.
      *-----------------------------------------------------------------
        R-INICIAL.
            IF          LK-CHAMA    NOT EQUAL 85
                        STOP        RUN
            END-IF
            
            INITIALIZE  CT-CNPJ         LK-RET
            MOVE        LK-CNPJ         TO  AX-CNPJ

      * PRIMEIRO DIGITO
            INITIALIZE  CT-CNPJ
            MULTIPLY    AX-CNPJ-01      BY  5   GIVING  CT-CNPJ-01
            MULTIPLY    AX-CNPJ-02      BY  4   GIVING  CT-CNPJ-02
            MULTIPLY    AX-CNPJ-03      BY  3   GIVING  CT-CNPJ-03
            MULTIPLY    AX-CNPJ-04      BY  2   GIVING  CT-CNPJ-04
            MULTIPLY    AX-CNPJ-05      BY  9   GIVING  CT-CNPJ-05
            MULTIPLY    AX-CNPJ-06      BY  8   GIVING  CT-CNPJ-06
            MULTIPLY    AX-CNPJ-07      BY  7   GIVING  CT-CNPJ-07
            MULTIPLY    AX-CNPJ-08      BY  6   GIVING  CT-CNPJ-08
            MULTIPLY    AX-CNPJ-09      BY  5   GIVING  CT-CNPJ-09
            MULTIPLY    AX-CNPJ-10      BY  4   GIVING  CT-CNPJ-10
            MULTIPLY    AX-CNPJ-11      BY  3   GIVING  CT-CNPJ-11
            MULTIPLY    AX-CNPJ-12      BY  2   GIVING  CT-CNPJ-12

            COMPUTE     AX-RESULT-1     = CT-CNPJ-01 + CT-CNPJ-02
                                        + CT-CNPJ-03 + CT-CNPJ-04
                                        + CT-CNPJ-05 + CT-CNPJ-06
                                        + CT-CNPJ-07 + CT-CNPJ-08
                                        + CT-CNPJ-09 + CT-CNPJ-10
                                        + CT-CNPJ-11 + CT-CNPJ-12

            DIVIDE      AX-RESULT-1     BY  11  GIVING  AX-RESULT-2
                                                REMAINDER AX-RESTO

            COMPUTE     CT-CNPJ-D1 = 11 - AX-RESTO

            MOVE        CT-CNPJ-D1      TO  AX-DIG-CALC-1

      * SEGUNDO DIGITO
            INITIALIZE  CT-CNPJ
            MULTIPLY    AX-CNPJ-01      BY  5   GIVING  CT-CNPJ-01
            MULTIPLY    AX-CNPJ-02      BY  4   GIVING  CT-CNPJ-02
            MULTIPLY    AX-CNPJ-03      BY  3   GIVING  CT-CNPJ-03
            MULTIPLY    AX-CNPJ-04      BY  2   GIVING  CT-CNPJ-04
            MULTIPLY    AX-CNPJ-05      BY  9   GIVING  CT-CNPJ-05
            MULTIPLY    AX-CNPJ-06      BY  8   GIVING  CT-CNPJ-06
            MULTIPLY    AX-CNPJ-07      BY  7   GIVING  CT-CNPJ-07
            MULTIPLY    AX-CNPJ-08      BY  6   GIVING  CT-CNPJ-08
            MULTIPLY    AX-CNPJ-09      BY  5   GIVING  CT-CNPJ-09
            MULTIPLY    AX-CNPJ-10      BY  4   GIVING  CT-CNPJ-10
            MULTIPLY    AX-CNPJ-11      BY  3   GIVING  CT-CNPJ-11
            MULTIPLY    AX-CNPJ-12      BY  2   GIVING  CT-CNPJ-12
            MULTIPLY    AX-CNPJ-D1      BY  2   GIVING  CT-CNPJ-D1

            COMPUTE     AX-RESULT-1     = CT-CNPJ-01 + CT-CNPJ-02
                                        + CT-CNPJ-03 + CT-CNPJ-04
                                        + CT-CNPJ-05 + CT-CNPJ-06
                                        + CT-CNPJ-07 + CT-CNPJ-08
                                        + CT-CNPJ-09 + CT-CNPJ-10
                                        + CT-CNPJ-11 + CT-CNPJ-12
                                        + CT-CNPJ-D1

            DIVIDE      AX-RESULT-1     BY  11  GIVING  AX-RESULT-2
                                                REMAINDER AX-RESTO

            COMPUTE     CT-CNPJ-D2      = 11 - AX-RESTO

            MOVE        CT-CNPJ-D2      TO  AX-DIG-CALC-2

      * VERIFICACAO
            IF          AX-DIG-CALC EQUAL LK-DIGITO
                        SET         LK-OK       TO  TRUE
            END-IF

            GOBACK.
      *-----------------------------------------------------------------
