*&---------------------------------------------------------------------*
*& Report  ZR_ABAP_FILE_LOCAL_EXAMPLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zr_abap_file_local_example.

INCLUDE zi_abap_file_local.

"Parametros do arquivo
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_path TYPE lif_abap_file=>ty_filepath LOWER CASE,                            "Caminho do arquivo
  p_syst TYPE lif_abap_file=>ty_system DEFAULT lif_abap_file=>c_system-serv,    "Sistema do arquivo (LOCL / SERV)
  p_hdri TYPE lif_abap_file=>ty_hdrind DEFAULT 'X',                             "Ind. cabecalho (para tabelas)
  p_sepa TYPE lif_abap_file=>ty_separator DEFAULT ';'.                          "Separador (para CSV/TXT)
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
"Leitura
PARAMETERS:
  p_read RADIOBUTTON GROUP g1 DEFAULT 'X',                                      "Le arquivo
  p_prev AS CHECKBOX DEFAULT 'X',                                               "Recupera dados lidos
  p_down AS CHECKBOX.                                                           "Download dados lidos

SELECTION-SCREEN SKIP.

"Criacao
PARAMETERS:
  p_save RADIOBUTTON GROUP g1,                                                  "Salvar arquivo
  p_bin  TYPE lif_abap_file=>ty_filepath LOWER CASE.                            "Arq. qqr para testes
SELECTION-SCREEN END OF BLOCK b2.
PARAMETERS p_fact AS CHECKBOX DEFAULT 'X'.                                      "Usa classe LCL_ABAP_FILE_FACTORY

START-OF-SELECTION.
  TRY .
      IF p_fact = abap_true.
        CASE abap_true.
          WHEN p_read. PERFORM f_read_factory.
          WHEN p_save. PERFORM f_save_factory.
        ENDCASE.
      ELSE.
        CASE abap_true.
          WHEN p_read. PERFORM f_read.
          WHEN p_save. PERFORM f_save.
        ENDCASE.
      ENDIF.
    CATCH lcx_abap_file INTO DATA(lx_exception).
      MESSAGE lx_exception->get_text( ) TYPE 'E'.
  ENDTRY.

FORM f_read_factory RAISING lcx_abap_file.

  DATA:
    lt_bsegkey TYPE STANDARD TABLE OF bsegkey.

  "Recupera extensao
  CASE lcl_abap_file=>get_extension( p_path ).
    WHEN lif_abap_file=>c_extension-pdf.
      lcl_abap_file_factory=>read_file_to_binary( EXPORTING iv_path    = p_path
                                                            iv_syst    = p_syst
                                                  IMPORTING et_binary  = DATA(lt_binary)
                                                            ev_xstring = DATA(lv_xstring) ).

    WHEN OTHERS.
      lcl_abap_file_factory=>read_file_to_itab( EXPORTING iv_path = p_path
                                                          iv_syst = p_syst
                                                          iv_hdri = p_hdri
                                                          iv_sepa = p_sepa
                                                IMPORTING et_data = lt_bsegkey[] ).

  ENDCASE.

ENDFORM.

FORM f_save_factory RAISING lcx_abap_file.

  DATA:
    lt_bsegkey TYPE STANDARD TABLE OF bsegkey,
    lt_bintab  TYPE solix_tab,
    lv_binlen  TYPE i.

  "Recupera extensao
  CASE lcl_abap_file=>get_extension( p_path ).
    WHEN lif_abap_file=>c_extension-pdf.
      "importa PDF para teste
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = p_bin
          filetype                = 'BIN'
        IMPORTING
          filelength              = lv_binlen
        CHANGING
          data_tab                = lt_bintab[]
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19
      ).

      "Cria arquivo binario
      lcl_abap_file_factory=>create_file_from_binary( it_binary = lt_bintab
                                                      iv_binlen = lv_binlen
                                                      iv_path   = p_path
                                                      iv_syst   = p_syst ).

    WHEN OTHERS.
      "Le dados
      SELECT bukrs, belnr, gjahr, buzei
        UP TO 10 ROWS
        FROM bseg
        INTO TABLE @lt_bsegkey[].

      "Cria arquivo de tabela
      lcl_abap_file_factory=>create_file_from_itab( it_data = lt_bsegkey
                                                    iv_path = p_path
                                                    iv_syst = p_syst
                                                    iv_hdri = p_hdri
                                                    iv_sepa = p_sepa ).

  ENDCASE.

ENDFORM.

FORM f_read RAISING lcx_abap_file.

  DATA:
    lo_fileread TYPE REF TO lcl_abap_fileread,
    lt_tbfields TYPE abap_component_tab,
    lt_tbkeys   TYPE abap_keydescr_tab.

  TRY.
      "Recupera extensao
      DATA(lv_extension) = lcl_abap_file=>get_extension( p_path ).

      "Recupera instancia da classe correspondente
      lo_fileread = lcl_abap_fileread=>get_instance( lv_extension ).

      "Seta atributos do arquivo
      lo_fileread->set_fileattr( iv_path = p_path     "caminho do arquivo / url ou destination do servico
                                 iv_syst = p_syst     "origem do arquivo (SERV / LOCL)
                                 iv_hdri = p_hdri     "indicador de cabecalho
                                 iv_sepa = p_sepa ).  "separador (para CSV)

      CASE lv_extension.
        WHEN lif_abap_file=>c_extension-pdf.
        WHEN OTHERS.
          "Busca estrutura da tab interna a partir da tabela interna
          lcl_abap_itab_factory=>get_components_from_ddic(
            EXPORTING
              iv_ddic = 'BSEGKEY'
            IMPORTING
              et_flds = lt_tbfields[]  "campos
              et_keys = lt_tbkeys[]    "chaves
          ).

          "Seta tabela interna
          lo_fileread->set_itabref(
            io_itab = NEW lcl_abap_itab( flds = lt_tbfields[] keys = lt_tbkeys[] )
          ).
      ENDCASE.

      "Le arquivo
      lo_fileread->read( ).

      CASE abap_true.
        WHEN p_prev.
          "Exibe arquivo
          lo_fileread->display( ).

*          CASE lv_extension.
*            WHEN lif_abap_file=>c_extension-pdf.
*              lo_fileread->get_binary( IMPORTING et_data = DATA(lt_binary)
*                                                 ev_size = DATA(lv_binlen) ).
*
*            WHEN OTHERS.
*              "Visualiza dados lidos
*              lo_fileread->get_itabref( )->display( ).
**              lo_fileread->get_itabref( )->get_values( IMPORTING et_values = lt_bsegkey[] ).
*
*          ENDCASE.

        WHEN p_down.
          "Realiza download
          lo_fileread->download( ).

      ENDCASE.

    CATCH lcx_abap_itab  INTO DATA(lx_itab).
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          previous = lx_itab.
  ENDTRY.

ENDFORM.

FORM f_save RAISING lcx_abap_file.

  DATA:
    lo_filecreate TYPE REF TO lcl_abap_filecreate,
    lt_tbfields   TYPE abap_component_tab,
    lt_tbkeys     TYPE abap_keydescr_tab,
    lt_bsegkey    TYPE STANDARD TABLE OF bsegkey,
    lt_bintab     TYPE solix_tab,
    lv_binlen     TYPE i.

  TRY.
      "Recupera extensao
      DATA(lv_extension) = lcl_abap_file=>get_extension( p_path ).

      "Recupera instancia da classe da extensao
      lo_filecreate = lcl_abap_filecreate=>get_instance( lv_extension ).

      "Seta atributos do arquivo a ser gerado
      lo_filecreate->set_fileattr( iv_path = p_path
                                   iv_syst = p_syst
                                   iv_hdri = p_hdri
                                   iv_sepa = p_sepa ).

      CASE lv_extension.
        WHEN lif_abap_file=>c_extension-pdf.
          "importa PDF para teste
          cl_gui_frontend_services=>gui_upload(
            EXPORTING
              filename                = p_bin
              filetype                = 'BIN'
            IMPORTING
              filelength              = lv_binlen
            CHANGING
              data_tab                = lt_bintab[]
            EXCEPTIONS
              file_open_error         = 1
              file_read_error         = 2
              no_batch                = 3
              gui_refuse_filetransfer = 4
              invalid_type            = 5
              no_authority            = 6
              unknown_error           = 7
              bad_data_format         = 8
              header_not_allowed      = 9
              separator_not_allowed   = 10
              header_too_long         = 11
              unknown_dp_error        = 12
              access_denied           = 13
              dp_out_of_memory        = 14
              disk_full               = 15
              dp_timeout              = 16
              not_supported_by_gui    = 17
              error_no_gui            = 18
              OTHERS                  = 19
          ).

          "Seta binario do PDF
          lo_filecreate->set_binary( it_solix  = lt_bintab[]
                                     iv_lenght = lv_binlen ).

        WHEN OTHERS.
          "Le dados
          SELECT bukrs, belnr, gjahr, buzei
            UP TO 10 ROWS
            FROM bseg
            INTO TABLE @lt_bsegkey[].

          "Busca campos/chaves a partir da tabela interna
          lcl_abap_itab_factory=>get_components_from_itab(
            EXPORTING
              it_itab = lt_bsegkey[]
            IMPORTING
              et_flds = lt_tbfields[]  "campos
              et_keys = lt_tbkeys[]    "chaves
          ).

          "Cria tabela interna
          DATA(lo_itab) = NEW lcl_abap_itab( flds = lt_tbfields[] keys = lt_tbkeys[] ).

          "Seta dados na tabela interna
          lo_itab->set_values( lt_bsegkey[] ).

          "Seta ref da tabela interna
          lo_filecreate->set_itabref( lo_itab ).

      ENDCASE.

      "Salva dados
      lo_filecreate->save( ).

    CATCH lcx_abap_itab  INTO DATA(lx_itab).
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          previous = lx_itab.
  ENDTRY.

ENDFORM.