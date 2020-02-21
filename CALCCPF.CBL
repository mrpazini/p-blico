      *>----------------------------------------------------------------
      *>ESTE PROGRAMA NAO PASSOU POR NENHUMA COMPILACAO
      *>----------------------------------------------------------------
        IDENTIFICATION DIVISION.
      *>----------------------------------------------------------------
        PROGRAM-ID.    CALCCPF.
        AUTHOR.        MILTON ROGERIO PAZINI.
      *>----------------------------------------------------------------
      *>SISTEMA        : TESTE PROGRAMACAO
      *>PROGRAMA       : VALIDACAO DE CPF
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

      *>----------------------------------------------------------------
       DATA DIVISION.
      *>----------------------------------------------------------------

      *>----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *>----------------------------------------------------------------
       COPY    "WSSVARS.LIB".

           05  AX-RESTO                PIC  9(003)     VALUE   ZEROS.
           05  AX-RESULT               PIC  9(018)     VALUE   ZEROS.
           05  AX-RESULT-1             PIC  9(018)     VALUE   ZEROS.
           05  AX-RESULT-2             PIC  9(018)     VALUE   ZEROS.
           05  AX-DIG-CALC             PIC  9(002)     VALUE   ZEROS.
           05  RX-DIG-CALC             REDEFINES       AX-DIG-CALC.
               10  AX-DIG-CALC-1       PIC  9(001).
               10  AX-DIG-CALC-2       PIC  9(001).

           05  AX-CPF.
               10  AX-CPF-01           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-02           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-03           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-04           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-05           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-06           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-07           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-08           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-09           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-D1           PIC  9(001)     VALUE   ZEROS.
               10  AX-CPF-D2           PIC  9(001)     VALUE   ZEROS.

           05  CT-CPF.
               10  CT-CPF-01           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-02           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-03           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-04           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-05           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-06           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-07           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-08           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-09           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-D1           PIC  9(002)     VALUE   ZEROS.
               10  CT-CPF-D2           PIC  9(002)     VALUE   ZEROS.

      *>----------------------------------------------------------------
       LINKAGE SECTION.
      *>----------------------------------------------------------------
       01  LK-DADOS.
           05  LK-CPF.
               10  LK-NUMERO           PIC  9(009).
               10  LK-DIGITO           PIC  9(002).
           05  LK-RET                  PIC  9(001).
               88  LK-OK                               VALUE   1.

       01  LK-CHAMA                    PIC  9(002).

      *>----------------------------------------------------------------
       PROCEDURE DIVISION      USING   LK-DADOS    LK-CHAMA.
      *>----------------------------------------------------------------
       R-INICIAL.
           IF          LK-CHAMA    NOT EQUAL 85
                       STOP        RUN
           END-IF

           INITIALIZE  LK-RET
           MOVE        LK-CPF          TO  AX-CPF

      *>PRIMEIRO DIGITO
           INITIALIZE  CT-CPF
           MULTIPLY    AX-CPF-01       BY  10  GIVING  CT-CPF-01
           MULTIPLY    AX-CPF-02       BY   9  GIVING  CT-CPF-02
           MULTIPLY    AX-CPF-03       BY   8  GIVING  CT-CPF-03
           MULTIPLY    AX-CPF-04       BY   7  GIVING  CT-CPF-04
           MULTIPLY    AX-CPF-05       BY   6  GIVING  CT-CPF-05
           MULTIPLY    AX-CPF-06       BY   5  GIVING  CT-CPF-06
           MULTIPLY    AX-CPF-07       BY   4  GIVING  CT-CPF-07
           MULTIPLY    AX-CPF-08       BY   3  GIVING  CT-CPF-08
           MULTIPLY    AX-CPF-09       BY   2  GIVING  CT-CPF-09

           COMPUTE     AX-RESULT-1 = CT-CPF-01 + CT-CPF-02 + CT-CPF-03
                                   + CT-CPF-04 + CT-CPF-05 + CT-CPF-06
                                   + CT-CPF-07 + CT-CPF-08 + CT-CPF-09

           DIVIDE      AX-RESULT-1     BY  11  GIVING  AX-RESULT-2
                                               REMAINDER AX-RESTO

           IF          AX-RESTO LESS 2
                       MOVE        ZEROS       TO  CT-CPF-D1
           ELSE
                       SUBTRACT    AX-RESTO    FROM    11
                                               GIVING  CT-CPF-D1
           END-IF

           MOVE        CT-CPF-D1               TO  AX-DIG-CALC-1

      *>SEGUNDO DIGITO
           INITIALIZE  CT-CPF
           MULTIPLY    AX-CPF-01       BY  10  GIVING  CT-CPF-01
           MULTIPLY    AX-CPF-02       BY   9  GIVING  CT-CPF-02
           MULTIPLY    AX-CPF-03       BY   8  GIVING  CT-CPF-03
           MULTIPLY    AX-CPF-04       BY   7  GIVING  CT-CPF-04
           MULTIPLY    AX-CPF-05       BY   6  GIVING  CT-CPF-05
           MULTIPLY    AX-CPF-06       BY   5  GIVING  CT-CPF-06
           MULTIPLY    AX-CPF-07       BY   4  GIVING  CT-CPF-07
           MULTIPLY    AX-CPF-08       BY   3  GIVING  CT-CPF-08
           MULTIPLY    AX-CPF-09       BY   2  GIVING  CT-CPF-09
           MULTIPLY    AX-CPF-D1       BY   2  GIVING  CT-CPF-D1

           COMPUTE     AX-RESULT-1 = CT-CPF-01 + CT-CPF-02 + CT-CPF-03
                                   + CT-CPF-04 + CT-CPF-05 + CT-CPF-06
                                   + CT-CPF-07 + CT-CPF-08 + CT-CPF-09
                                   + CT-CPF-D1

           DIVIDE      AX-RESULT-1     BY  11  GIVING  AX-RESULT-2
                                               REMAINDER AX-RESTO

           IF          AX-RESTO LESS 2
                       MOVE        ZEROS       TO  CT-CPF-D2
           ELSE
                       SUBTRACT    AX-RESTO    FROM    11
                                               GIVING  CT-CPF-D2
           END-IF

           MOVE        CT-CPF-D2               TO  AX-DIG-CALC-2

      *>VERIFICACAO
           IF          AX-DIG-CALC EQUAL LK-DIGITO
                       SET         LK-OK       TO  TRUE
           END-IF

           GOBACK.
      *>----------------------------------------------------------------
