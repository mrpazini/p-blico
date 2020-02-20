      *-----------------------------------------------------------------
      * ESTE PROGRAMA NAO PASSOU POR NENHUMA COMPILACAO
      *-----------------------------------------------------------------
        IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
        PROGRAM-ID.     TSTMENU.
        AUTHOR.         MILTON ROGERIO PAZINI.
      *-----------------------------------------------------------------
      * SISTEMA       : TESTE PROGRAMACAO
      * PROGRAMA      : MENU
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
        ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
        CONFIGURATION SECTION.
      *-----------------------------------------------------------------
        SPECIAL-NAMES.
                        DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------
        DATA DIVISION.
      *-----------------------------------------------------------------
        FILE SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
        WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
        COPY    "WSSVARS.LIB".

            05  AX-OP00                     PIC 9(002)  VALUE   ZEROS.
            05  AX-OP01                     PIC 9(002)  VALUE   ZEROS.
            
        77  LK-CHAMA                        PIC 9(002)  VALUE   85.

      *-----------------------------------------------------------------
        SCREEN SECTION.
      *-----------------------------------------------------------------
      $SET SOURCEFORMAT "FREE".
        01  T-MENU-00.
            05  LINE 01 COLUMN  01  BLANK   SCREEN.
            05  LINE 06 COLUMN  32  VALUE   "Menu".
            05  LINE 08 COLUMN  32  VALUE   " 01 - Cadastros".
            05  LINE 10 COLUMN  32  VALUE   " 02 - Relatorios".
            05  LINE 12 COLUMN  32  VALUE   " 03 - Executar".
            05  LINE 14 COLUMN  32  VALUE   " 99 - Sair".
            05  LINE 16 COLUMN  32  VALUE   "[  ]  Opcao".
            05  LINE 16 COLUMN  33  PIC Z9  USING AX-OP00.

        01  T-MENU-10.
            05  LINE 01 COLUMN  01  BLANK   SCREEN.
            05  LINE 06 COLUMN  27  VALUE   "Cadastros".
            05  LINE 08 COLUMN  27  VALUE   " 01 - Cadastro de Cliente".
            05  LINE 10 COLUMN  27  VALUE   " 02 - Cadastro de Vendedor".
            05  LINE 12 COLUMN  32  VALUE   " 99 - Sair".
            05  LINE 14 COLUMN  27  VALUE   "[  ]  Opcao".
            05  LINE 14 COLUMN  28  PIC Z9  USING AX-OP01.

        01  T-MENU-20.
            05  LINE 01 COLUMN  01  BLANK   SCREEN.
            05  LINE 06 COLUMN  27  VALUE   "Relatorios".
            05  LINE 08 COLUMN  27  VALUE   " 01 - Relatorio de Cliente".
            05  LINE 10 COLUMN  27  VALUE   " 02 - Relatorio de Vendedor".
            05  LINE 12 COLUMN  32  VALUE   " 99 - Sair".
            05  LINE 14 COLUMN  27  VALUE   "[  ]  Opcao".
            05  LINE 14 COLUMN  28  PIC Z9  USING AX-OP01.

        01  T-MENU-30.
            05  LINE 01 COLUMN  01  BLANK   SCREEN.
            05  LINE 06 COLUMN  27  VALUE   "Executar".
            05  LINE 08 COLUMN  27  VALUE   " 01 - Executar Distribuicao de Clientes".
            05  LINE 10 COLUMN  32  VALUE   " 99 - Sair".
            05  LINE 12 COLUMN  27  VALUE   "[  ]  Opcao".
            05  LINE 12 COLUMN  28  PIC Z9  USING AX-OP01.

      *-----------------------------------------------------------------
        PROCEDURE DIVISION.
      *-----------------------------------------------------------------
        R-INICIAL.
      *-----------------------------------------------------------------
            PERFORM     UNTIL   AX-OP00 EQUAL 99
                        INITIALIZE  AX-OP00.
                        DISPLAY     T-MENU-00
                        ACCEPT      T-MENU-00
                        INITIALIZE  AX-OP01
                        EVALUATE    AX-OP00
                            WHEN    01
                                    PERFORM     R-MENU-01
                            WHEN    02
                                    PERFORM     R-MENU-02
                            WHEN    03
                                    PERFORM     R-MENU-03
                        END-EVALUATE
            END-PERFORM

            GOBACK.

      *-----------------------------------------------------------------
        R-MENU-01.
      *-----------------------------------------------------------------
            PERFORM     UNTIL   AX-OP01 EQUAL 99
                        DISPLAY     T-MENU-01
                        ACCEPT      T-MENU-01
                        EVALUATE    AX-OP01
                            WHEN    01
                                    CALL        "TSTCDCLI"  USING   LK-CHAMA
                                    CANCEL      "TSTCDCLI"
                            WHEN    02
                                    CALL        "TSTCDVND"  USING   LK-CHAMA
                                    CANCEL      "TSTCDVND"
                        END-EVALUATE
            END-PERFORM.

      *-----------------------------------------------------------------
        R-MENU-02.
      *-----------------------------------------------------------------
            PERFORM     UNTIL   AX-OP01 EQUAL 99
                        DISPLAY     T-MENU-02
                        ACCEPT      T-MENU-02
                        EVALUATE    AX-OP01
                            WHEN    01
                                    CALL        "TSTRLCLI"  USING   LK-CHAMA
                                    CANCEL      "TSTRLCLI"
                            WHEN    02
                                    CALL        "TSTRLVND"  USING   LK-CHAMA
                                    CANCEL      "TSTRLVND"
                        END-EVALUATE
            END-PERFORM.

      *-----------------------------------------------------------------
        R-MENU-03.
      *-----------------------------------------------------------------
            PERFORM     UNTIL   AX-OP01 EQUAL 99
                        DISPLAY     T-MENU-03
                        ACCEPT      T-MENU-03
                        EVALUATE    AX-OP01
                            WHEN    01
                                    CALL        "TSTDISTR"  USING   LK-CHAMA
                                    CANCEL      "TSTDISTR"
                        END-EVALUATE
            END-PERFORM.
      *-----------------------------------------------------------------
