*&---------------------------------------------------------------------*
*&  Include  ZI_ABAP_FILE_LOCAL
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Interface com tipos
*&---------------------------------------------------------------------*
INTERFACE lif_abap_file.
  TYPES:
    ty_extension TYPE c LENGTH 4,
    ty_filepath  TYPE string,
    ty_system    TYPE c LENGTH 10,
    ty_hdrind    TYPE abap_bool,
    ty_separator TYPE c LENGTH 1.

  CONSTANTS:
    BEGIN OF c_extension,
      xlsx TYPE ty_extension VALUE 'XLSX',
      xls  TYPE ty_extension VALUE 'XLS',
      csv  TYPE ty_extension VALUE 'CSV',
      txt  TYPE ty_extension VALUE 'TXT',
      pdf  TYPE ty_extension VALUE 'PDF',
    END OF c_extension .

  CONSTANTS:
    BEGIN OF c_system,
      locl TYPE ty_system VALUE 'LOCL',
      serv TYPE ty_system VALUE 'SERV',
    END OF c_system .
ENDINTERFACE.

*&---------------------------------------------------------------------*
* Classe de excecao p/ classe lcl_abap_itab
*&---------------------------------------------------------------------*
CLASS lcx_abap_itab DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING previous LIKE previous OPTIONAL
                textid   LIKE textid OPTIONAL
                symsg    TYPE symsg OPTIONAL
                msgtxt   TYPE string OPTIONAL .
    METHODS get_symsg
      RETURNING VALUE(symsg) TYPE symsg .
    METHODS get_text REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA symsg TYPE symsg .
    DATA txmsg TYPE string .
ENDCLASS.
CLASS lcx_abap_itab IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous textid = textid ).
    me->symsg = symsg.
    me->txmsg = msgtxt.
  ENDMETHOD.
  METHOD get_symsg.
    CLEAR symsg.
    symsg = me->symsg.
  ENDMETHOD.
  METHOD get_text.
    CLEAR result.
    result = me->txmsg.
    CHECK result IS INITIAL.
    IF me->symsg-msgty IS NOT INITIAL.
      MESSAGE ID me->symsg-msgid TYPE me->symsg-msgty NUMBER me->symsg-msgno
        WITH me->symsg-msgv1 me->symsg-msgv2 me->symsg-msgv3 me->symsg-msgv4
        INTO result.
    ENDIF.
    CHECK result IS INITIAL.
    IF me->previous IS BOUND.
      result = me->previous->get_text( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
* Classes auxiliar para tratamento de tabelas dinamicas
*&---------------------------------------------------------------------*
CLASS lcl_abap_itab DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_fldslist,
        name TYPE name_feld,
        posn TYPE i,
        ispk TYPE keyflag,
        ddic TYPE rollname,
        BEGIN OF predefined,
          type TYPE name_feld,
          leng TYPE ddleng,
          deci TYPE decimals,
        END OF predefined,
      END OF ty_fldslist ,
      tt_fldslist TYPE STANDARD TABLE OF ty_fldslist WITH KEY name.

    CLASS-METHODS get_components_from_ddic
      IMPORTING iv_ddic TYPE tabname
      EXPORTING et_flds TYPE abap_component_tab
                et_keys TYPE abap_keydescr_tab
      RAISING   lcx_abap_itab .

    CLASS-METHODS get_components_from_itab
      IMPORTING it_itab TYPE ANY TABLE
      EXPORTING et_flds TYPE abap_component_tab
                et_keys TYPE abap_keydescr_tab
      RAISING   lcx_abap_itab .

    CLASS-METHODS get_components_from_list
      IMPORTING it_list TYPE tt_fldslist
      EXPORTING et_flds TYPE abap_component_tab
                et_keys TYPE abap_keydescr_tab
      RAISING   lcx_abap_itab .

    CLASS-METHODS create_from_itab
      IMPORTING it_itab        TYPE ANY TABLE
      RETURNING VALUE(ro_itab) TYPE REF TO lcl_abap_itab
      RAISING   lcx_abap_itab .

    METHODS constructor
      IMPORTING flds TYPE abap_component_tab
                keys TYPE abap_keydescr_tab
      RAISING   lcx_abap_itab .

    METHODS set_values
      IMPORTING it_values TYPE ANY TABLE
      RAISING   lcx_abap_itab .

    METHODS get_values
      EXPORTING et_values TYPE ANY TABLE
      RAISING   lcx_abap_itab .

    METHODS get_data
      RETURNING VALUE(ro_data) TYPE REF TO data.

    METHODS get_xstring
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   lcx_abap_itab .

    METHODS display
      RAISING lcx_abap_itab .

  PRIVATE SECTION.
    DATA:
      BEGIN OF s_itabattr,
        t_flds TYPE abap_component_tab,
        t_keys TYPE abap_keydescr_tab,
      END OF s_itabattr,
      o_data TYPE REF TO data.

    METHODS create_itab
      RAISING lcx_abap_itab .

ENDCLASS.

CLASS lcl_abap_itab IMPLEMENTATION.

  METHOD get_components_from_ddic.

    DATA:
      lo_typedescr   TYPE REF TO cl_abap_typedescr,
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    CLEAR: et_flds[], et_keys[].

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = iv_ddic
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      "Tipo &1 desconhecido
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tipo ' iv_ddic ' desconhecido'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE lo_typedescr->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        "Realiza cast do objeto
        lo_structdescr ?= lo_typedescr.

        "Recupera chaves
        DATA(lt_ddic) = lo_structdescr->get_ddic_field_list( ).
        DELETE lt_ddic WHERE keyflag = abap_false.
        et_keys[] = CORRESPONDING #( lt_ddic[] MAPPING name = fieldname ).

      WHEN cl_abap_typedescr=>kind_table.
        "Realiza cast do objeto
        lo_tabledescr  ?= lo_typedescr.

        "Recupera tipo da linha
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

        "Recupera chaves
        et_keys[] = lo_tabledescr->key[].

      WHEN OTHERS.
        "Tipo &1 desconhecido
        MESSAGE e001(00) INTO lv_dummy
          WITH 'Tipo ' iv_ddic ' desconhecido'.
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.

    "Recupera campos
    et_flds[] = lo_structdescr->get_components( ).

    IF et_keys[] IS INITIAL.
      "Recupera chaves
      et_keys[] = CORRESPONDING #( et_flds[] MAPPING name = name ).
    ENDIF.

  ENDMETHOD.

  METHOD get_components_from_itab.

    DATA:
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.

    CLEAR: et_flds[], et_keys[].

    TRY.
        "Recupera chaves
        lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data( it_itab ).
        et_keys[] = lo_tabledescr->key[].

        "Recupera campos
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
        et_flds[] = lo_structdescr->get_components( ).

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_components_from_list.

    DATA:
      lo_typedescr TYPE REF TO cl_abap_typedescr.

    CLEAR: et_flds[], et_keys[].
    CHECK it_list[] IS NOT INITIAL.

    DATA(lt_fieldlist) = it_list[].
    SORT lt_fieldlist BY posn.

    LOOP AT lt_fieldlist[] ASSIGNING FIELD-SYMBOL(<fs_field>).
      "Insere componente
      APPEND INITIAL LINE TO et_flds[] ASSIGNING FIELD-SYMBOL(<fs_component>).
      <fs_component>-name = <fs_field>-name.

      IF <fs_field>-ddic IS NOT INITIAL.
        CASE <fs_field>-ddic.
          WHEN 'STRING'.
            <fs_component>-type  = cl_abap_elemdescr=>get_string( ).
          WHEN 'XSTRING'.
            <fs_component>-type  = cl_abap_elemdescr=>get_xstring( ).
          WHEN OTHERS.
            "Elemento de dados
            cl_abap_elemdescr=>describe_by_name(
              EXPORTING
                p_name         = <fs_field>-ddic
              RECEIVING
                p_descr_ref    = lo_typedescr
              EXCEPTIONS
                type_not_found = 1
                OTHERS         = 2
            ).
            IF sy-subrc <> 0.
              MESSAGE e001(00) INTO DATA(lv_dummy)
                WITH 'Tipo ' <fs_field>-ddic ' desconhecido'.
              RAISE EXCEPTION TYPE lcx_abap_itab
                EXPORTING
                  symsg = CORRESPONDING #( sy ).
            ENDIF.

            "Seta tipo do campo
            <fs_component>-type ?= lo_typedescr.
        ENDCASE.

      ELSE.
        "Tipos basicos
        CASE <fs_field>-predefined-type.
          WHEN 'C'. <fs_component>-type = cl_abap_elemdescr=>get_c( CONV #( <fs_field>-predefined-leng ) ).
          WHEN 'D'. <fs_component>-type = cl_abap_elemdescr=>get_d( ).
          WHEN 'F'. <fs_component>-type = cl_abap_elemdescr=>get_f( ).
          WHEN 'I'. <fs_component>-type = cl_abap_elemdescr=>get_i( ).
          WHEN 'N'. <fs_component>-type = cl_abap_elemdescr=>get_n( CONV #( <fs_field>-predefined-leng ) ).
          WHEN 'P'.
            <fs_component>-type = cl_abap_elemdescr=>get_p( p_length   = CONV #( <fs_field>-predefined-leng )
                                                            p_decimals = CONV #( <fs_field>-predefined-deci ) ).
          WHEN 'T'. <fs_component>-type = cl_abap_elemdescr=>get_t( ).
          WHEN 'X'. <fs_component>-type = cl_abap_elemdescr=>get_x( CONV #( <fs_field>-predefined-leng ) ).
          WHEN OTHERS.
            MESSAGE e001(00) INTO lv_dummy
              WITH 'Tipo ' <fs_field>-predefined ' desconhecido'.
            RAISE EXCEPTION TYPE lcx_abap_itab
              EXPORTING
                symsg = CORRESPONDING #( sy ).
        ENDCASE.

      ENDIF.

      IF <fs_field>-ispk = abap_true.
        APPEND VALUE #( name = <fs_field>-name ) TO et_keys[].
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_from_itab.

    CLEAR ro_itab.

    lcl_abap_itab=>get_components_from_itab( EXPORTING it_itab = it_itab
                                             IMPORTING et_flds = DATA(lt_flds)
                                                       et_keys = DATA(lt_keys) ).

    ro_itab = NEW lcl_abap_itab( flds = lt_flds[] keys = lt_keys[] ).

  ENDMETHOD.

  METHOD constructor.

    "Salva atributos da tab interna
    me->s_itabattr-t_flds[] = flds[].
    me->s_itabattr-t_keys[] = keys[].

    "Remove linhas vazias
    DELETE me->s_itabattr-t_flds[] WHERE name IS INITIAL.
    DELETE me->s_itabattr-t_keys[] WHERE name IS INITIAL.

    "Remove linhas duplicadas
    DELETE ADJACENT DUPLICATES FROM me->s_itabattr-t_flds[] COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM me->s_itabattr-t_keys[] COMPARING ALL FIELDS.

    "Cria tabela interna
    me->create_itab( ).

  ENDMETHOD.

  METHOD create_itab.

    DATA:
      lo_tabdescr TYPE REF TO cl_abap_tabledescr.

    "Valida campos
    IF me->s_itabattr-t_flds[] IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Estrutura do arquivo não definida'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Valida chaves
    IF me->s_itabattr-t_keys[] IS INITIAL.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Estrutura de chaves do arquivo não definida'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        "Cria tipo de tabela
        lo_tabdescr = cl_abap_tabledescr=>create(
          p_line_type  = cl_abap_structdescr=>create( me->s_itabattr-t_flds[] )
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false
          p_key        = me->s_itabattr-t_keys[]
          p_key_kind   = cl_abap_tabledescr=>keydefkind_user
        ).

      CATCH cx_sy_table_creation INTO DATA(lx_tabcreate).
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_tabcreate.
      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_root.
    ENDTRY.

    "Cria tabela interna
    CREATE DATA me->o_data TYPE HANDLE lo_tabdescr.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD set_values.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      "Erro ao criar tabela interna
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Seta informacoes
    <ft_data> = it_values[].

  ENDMETHOD.

  METHOD get_values.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR et_values[].

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    et_values[] = <ft_data>.

  ENDMETHOD.

  METHOD get_data.

    CLEAR ro_data.
    ro_data = me->o_data.

  ENDMETHOD.

  METHOD get_xstring.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <ft_data>
        ).

        "Recupera catalogo de campos
        DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lo_salv->get_columns( )
          r_aggregations = lo_salv->get_aggregations( )
        ).

        "Recupera resultado
        DATA(lo_result_data_table) = cl_salv_ex_util=>factory_result_data_table(
          r_data         = me->o_data
          t_fieldcatalog = lt_fcat[]
        ).

        "Gera XSTRING
        cl_salv_bs_lex=>export_from_result_data_table(
          EXPORTING
            is_format            = if_salv_bs_lex_format=>mc_format_xlsx
            ir_result_data_table = lo_result_data_table
          IMPORTING
            er_result_file       = rv_xstring
        ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_salv_msg.

      CATCH cx_salv_unexpected_param_value INTO DATA(lx_unexpected_param_value).
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_unexpected_param_value.
    ENDTRY.


  ENDMETHOD.

  METHOD display.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    "Recupera tabela interna
    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_itab
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <ft_data>
        ).

        "Habilita funcoes
        lo_salv->get_functions( )->set_all( if_salv_c_bool_sap=>true ).

        "Seta todas colunas como otimizada
        lo_salv->get_columns( )->set_optimize( ).

        "Seta ZEBRA
        lo_salv->get_display_settings( )->set_striped_pattern( if_salv_c_bool_sap=>true ).

        "Seta linha selecionavel
        lo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        "Exibe ALV
        lo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        RAISE EXCEPTION TYPE lcx_abap_itab
          EXPORTING
            previous = lx_salv_msg.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Classe de excecao p/ classe LCL_ABAP_FILE*
*&---------------------------------------------------------------------*
CLASS lcx_abap_file DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING previous LIKE previous OPTIONAL
                textid   LIKE textid OPTIONAL
                symsg    TYPE symsg OPTIONAL
                msgtxt   TYPE string OPTIONAL .
    METHODS get_symsg RETURNING
                        VALUE(symsg) TYPE symsg .
    METHODS get_text REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA symsg TYPE symsg .
    DATA txmsg TYPE string .
ENDCLASS.
CLASS lcx_abap_file IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous textid = textid ).
    me->symsg = symsg.
    me->txmsg = msgtxt.
  ENDMETHOD.
  METHOD get_symsg.
    CLEAR symsg.
    symsg = me->symsg.
  ENDMETHOD.
  METHOD get_text.
    CLEAR result.
    result = me->txmsg.
    CHECK result IS INITIAL.
    IF me->symsg-msgty IS NOT INITIAL.
      MESSAGE ID me->symsg-msgid TYPE me->symsg-msgty NUMBER me->symsg-msgno
        WITH me->symsg-msgv1 me->symsg-msgv2 me->symsg-msgv3 me->symsg-msgv4
        INTO result.
    ENDIF.
    CHECK result IS INITIAL.
    IF me->previous IS BOUND.
      result = me->previous->get_text( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
* Classe pai
*&---------------------------------------------------------------------*
CLASS lcl_abap_file DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_extension
      IMPORTING iv_path             TYPE lif_abap_file=>ty_filepath
      RETURNING VALUE(rv_extension) TYPE lif_abap_file=>ty_extension .

    METHODS set_fileattr
      IMPORTING iv_path TYPE lif_abap_file=>ty_filepath
                iv_syst TYPE lif_abap_file=>ty_system DEFAULT 'LOCL'
                iv_hdri TYPE lif_abap_file=>ty_hdrind DEFAULT 'X'
                iv_sepa TYPE lif_abap_file=>ty_separator DEFAULT ';'
      RAISING   lcx_abap_file .

  PROTECTED SECTION.
    DATA:
      BEGIN OF s_fileattr,
        v_exte TYPE lif_abap_file=>ty_extension,
        v_path TYPE lif_abap_file=>ty_filepath,
        v_syst TYPE lif_abap_file=>ty_system,
        v_hdri TYPE lif_abap_file=>ty_hdrind,
        v_sepa TYPE lif_abap_file=>ty_separator,
      END OF s_fileattr .

    METHODS check_fileattr
      RAISING lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_file IMPLEMENTATION.

  METHOD check_fileattr.

    "Valida caminho do arquivo
    IF me->s_fileattr-v_path IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tipo do arquivo é inválido'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE me->s_fileattr-v_exte.
      WHEN lif_abap_file=>c_extension-csv OR lif_abap_file=>c_extension-txt.
        IF me->s_fileattr-v_sepa IS INITIAL.
          "Separador obrigatório
          MESSAGE e001(00) INTO lv_dummy
            WITH 'Separador obrigatório'.
          RAISE EXCEPTION TYPE lcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD get_extension.

    DATA:
      lv_path   TYPE string,
      lv_offset TYPE i,
      lv_size   TYPE i.

    CLEAR rv_extension.
    CHECK iv_path IS NOT INITIAL.

    lv_path = iv_path.

    CLEAR lv_offset.
    FIND ALL OCCURRENCES OF '\' IN lv_path IN CHARACTER MODE MATCH OFFSET lv_offset.
    IF lv_offset <> 0.
      ADD 1 TO lv_offset.
      SHIFT lv_path BY lv_offset PLACES LEFT.
    ENDIF.

    CLEAR lv_offset.
    FIND ALL OCCURRENCES OF '.' IN lv_path IN CHARACTER MODE MATCH OFFSET lv_offset.
    IF lv_offset <> 0.
      ADD 1 TO lv_offset.
      lv_size = strlen( lv_path ) - lv_offset.
      rv_extension = lv_path+lv_offset(lv_size).
    ENDIF.

    rv_extension = to_upper( condense( replace( val = rv_extension sub = '.' with = '' ) ) ).

  ENDMETHOD.

  METHOD set_fileattr.

    "Salva atributos do arquivo
    me->s_fileattr-v_path = iv_path.
    me->s_fileattr-v_syst = iv_syst.
    me->s_fileattr-v_hdri = iv_hdri.
    me->s_fileattr-v_sepa = iv_sepa.

    IF me->s_fileattr-v_exte IS INITIAL.
      me->s_fileattr-v_exte = get_extension( me->s_fileattr-v_path ).
    ENDIF.

*    "Seta separador para arq txt
*    IF me->s_fileattr-v_exte = 'TXT' AND me->s_fileattr-v_sepa IS INITIAL.
*      me->s_fileattr-v_sepa = cl_abap_char_utilities=>horizontal_tab.
*    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Definicao das classes para leitura de arquivo
*&---------------------------------------------------------------------*
CLASS lcl_abap_fileread DEFINITION
  INHERITING FROM lcl_abap_file
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING iv_fileexte        TYPE lif_abap_file=>ty_extension
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_abap_fileread
      RAISING   lcx_abap_file .

    METHODS set_itabref
      IMPORTING io_itab TYPE REF TO lcl_abap_itab
      RAISING   lcx_abap_file .

    METHODS get_itabref
      RETURNING VALUE(ro_itab) TYPE REF TO lcl_abap_itab
      RAISING   lcx_abap_file .

    METHODS read
      RAISING lcx_abap_file .

    METHODS get_binary
      EXPORTING et_data TYPE solix_tab
                ev_size TYPE i
      RAISING   lcx_abap_file .

    METHODS get_xstring
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   lcx_abap_file .

    METHODS download
      RAISING lcx_abap_file .

    METHODS display
      RAISING lcx_abap_file .

  PROTECTED SECTION.
    DATA:
      t_solix  TYPE solix_tab,
      v_binlen TYPE i,
      o_itab   TYPE REF TO lcl_abap_itab.

    METHODS check_filepath
      RAISING lcx_abap_file .

    METHODS read_locl
      RAISING lcx_abap_file .

    METHODS read_serv
      RAISING lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_fileread_csv DEFINITION
  INHERITING FROM lcl_abap_fileread
  CREATE PUBLIC .

  PROTECTED SECTION.
    METHODS read_locl REDEFINITION .
    METHODS read_serv REDEFINITION .

  PRIVATE SECTION.
    DATA:
      t_csvtab TYPE stringtab.

    METHODS parse
      RAISING lcx_abap_file .

    METHODS conversion_decimal
      IMPORTING input  TYPE string
      EXPORTING output TYPE any
      RAISING   lcx_abap_file .

    METHODS conversion_date
      IMPORTING input  TYPE string
      EXPORTING output TYPE d
      RAISING   lcx_abap_file .

    METHODS conversion_time
      IMPORTING input  TYPE string
      EXPORTING output TYPE t
      RAISING   lcx_abap_file .

    METHODS conversion_alpha_exit
      IMPORTING input  TYPE string
                outtyp TYPE REF TO cl_abap_typedescr OPTIONAL
      EXPORTING output TYPE any
      RAISING   lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_fileread_xls DEFINITION
  INHERITING FROM lcl_abap_fileread
  CREATE PUBLIC .

  PROTECTED SECTION.
    METHODS read_locl REDEFINITION .
    METHODS read_serv REDEFINITION .

  PRIVATE SECTION.
    METHODS parse
      RAISING lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_fileread_pdf DEFINITION
  INHERITING FROM lcl_abap_fileread
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS display REDEFINITION.

ENDCLASS.

*&---------------------------------------------------------------------*
* Implementacao das classes para leitura de arquivo
*&---------------------------------------------------------------------*
CLASS lcl_abap_fileread IMPLEMENTATION.

  METHOD get_instance.

    CLEAR ro_instance.

    "Valida tipo do arquivo
    IF iv_fileexte IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tipo do arquivo é inválido'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE iv_fileexte.
      WHEN lif_abap_file=>c_extension-xls
        OR lif_abap_file=>c_extension-xlsx.
        "Instancia classe de leitura de excel
        ro_instance = NEW lcl_abap_fileread_xls( ).

      WHEN lif_abap_file=>c_extension-csv
        OR lif_abap_file=>c_extension-txt.
        "Instancia classe de leitura de csv
        ro_instance = NEW lcl_abap_fileread_csv( ).

      WHEN lif_abap_file=>c_extension-pdf.
        "Instancia classe de leitura de pdf
        ro_instance = NEW lcl_abap_fileread_pdf( ).

      WHEN OTHERS.
        MESSAGE e001(00) INTO lv_dummy
          WITH 'Tipo ' iv_fileexte ' desconhecido'.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
    ENDCASE.

    "Seta extensao
    ro_instance->s_fileattr-v_exte = iv_fileexte.

  ENDMETHOD.

  METHOD set_itabref.

    me->o_itab = io_itab.
    IF me->o_itab IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tabela interna inexistente'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD get_itabref.

    ro_itab = me->o_itab.

  ENDMETHOD.

  METHOD read.

    "Valida caminho do arquivo
    me->check_filepath( ).

    CLEAR: me->t_solix[], me->v_binlen.

    CASE me->s_fileattr-v_syst.
      WHEN lif_abap_file=>c_system-locl.
        "Le arquivo local
        me->read_locl( ).

      WHEN lif_abap_file=>c_system-serv.
        "Le arquivo do servidor (AL11)
        me->read_serv( ).

      WHEN OTHERS.
        MESSAGE e001(00) INTO DATA(lv_dummy)
          WITH 'Origem do arquivo desconhecida'.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.

  ENDMETHOD.

  METHOD read_locl.

    "Importa arquivo em binario
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = me->s_fileattr-v_path
        filetype                = 'BIN'
      IMPORTING
        filelength              = me->v_binlen
      CHANGING
        data_tab                = me->t_solix[]
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
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD read_serv.

    DATA:
      ls_solix  TYPE solix,
      lv_length TYPE i.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao abrir o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Le arquivo
    DO.
      READ DATASET me->s_fileattr-v_path INTO ls_solix
        MAXIMUM LENGTH 255 LENGTH lv_length.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      "Insere na tabela
      APPEND ls_solix TO me->t_solix[].

      "Calcula tamanho total
      me->v_binlen = me->v_binlen + lv_length.
    ENDDO.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.

  ENDMETHOD.

  METHOD check_filepath.

    DATA:
      lv_exists TYPE c LENGTH 1.

    "Valida caminho do arquivo
    IF me->s_fileattr-v_path IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Caminho do arquivo é inválido'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Valida existencia do arquivo
    CASE me->s_fileattr-v_syst.
      WHEN lif_abap_file=>c_system-locl.
        "Busca arq na maquina local
        cl_gui_frontend_services=>file_exist(
          EXPORTING
            file                 = me->s_fileattr-v_path
          RECEIVING
            result               = lv_exists
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5
        ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

      WHEN lif_abap_file=>c_system-serv.
        "Busca arq no servidor (AL11)
        CALL FUNCTION 'PFL_CHECK_OS_FILE_EXISTENCE'
          EXPORTING
            long_filename         = CONV pfebackuppro( me->s_fileattr-v_path )
          IMPORTING
            file_exists           = lv_exists
          EXCEPTIONS
            authorization_missing = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

    ENDCASE.

    IF lv_exists = abap_false.
      "Arquivo não existente
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Arquivo não existente'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD download.

    CHECK me->t_solix[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = me->v_binlen
        filename                  = me->s_fileattr-v_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = me->t_solix[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD display.

    CHECK me->o_itab IS BOUND.
    me->o_itab->display( ).

  ENDMETHOD.

  METHOD get_binary.

    CLEAR: et_data[], ev_size.
    et_data[] = me->t_solix[].
    ev_size   = me->v_binlen.

  ENDMETHOD.

  METHOD get_xstring.

    CLEAR rv_xstring.
    CHECK me->t_solix[] IS NOT INITIAL.

    "Transforma binario em xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = me->v_binlen
      IMPORTING
        buffer       = rv_xstring
      TABLES
        binary_tab   = me->t_solix[]
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_fileread_csv IMPLEMENTATION.

  METHOD read_locl.

    CLEAR me->t_csvtab[].

    "Importa arquivo
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = me->s_fileattr-v_path
        filetype                = 'ASC'
      CHANGING
        data_tab                = me->t_csvtab[]
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
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Remove linhas vazias
    DELETE me->t_csvtab[] WHERE table_line IS INITIAL.
    IF me->t_csvtab[] IS NOT INITIAL.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.

  ENDMETHOD.

  METHOD read_serv.

    DATA:
      lv_csvlin TYPE string.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR INPUT IN TEXT MODE
      ENCODING DEFAULT IGNORING CONVERSION ERRORS
      WITH SMART LINEFEED.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao abrir o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Le arquivo
    DO.
      CLEAR lv_csvlin.
      READ DATASET me->s_fileattr-v_path INTO lv_csvlin.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      "Insere na tabela
      APPEND lv_csvlin TO me->t_csvtab[].
    ENDDO.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.

    "Remove linhas vazias
    DELETE me->t_csvtab[] WHERE table_line IS INITIAL.
    IF me->t_csvtab[] IS NOT INITIAL.

      "Move dados para tabela interna estruturada
      me->parse( ).

    ENDIF.

  ENDMETHOD.

  METHOD parse.

    DATA:
      lo_dataline TYPE REF TO data.

    FIELD-SYMBOLS:
      <ft_itb> TYPE ANY TABLE,
      <fs_itb> TYPE any.

    CHECK me->t_csvtab[] IS NOT INITIAL.

    "Recupera dados da tabela
    DATA(lo_data) = me->o_itab->get_data( ).

    "Recupera tabela interna
    ASSIGN lo_data->* TO <ft_itb>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria linha do tipo da tabela interna
    CREATE DATA lo_dataline LIKE LINE OF <ft_itb>.
    IF sy-subrc IS INITIAL.
      ASSIGN lo_dataline->* TO <fs_itb>.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao criar estrutura da tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Move dados
    LOOP AT me->t_csvtab[] ASSIGNING FIELD-SYMBOL(<fs_csv>).
      IF me->s_fileattr-v_hdri = abap_true.
        CHECK sy-tabix <> 1.
      ENDIF.

      "Recupera valores da linha do CSV
      SPLIT <fs_csv> AT me->s_fileattr-v_sepa INTO TABLE DATA(lt_fieldtab).
      CHECK lt_fieldtab[] IS NOT INITIAL.

      CLEAR <fs_itb>.

      "Move dados da linha do CSV para linha da ITAB
      LOOP AT lt_fieldtab[] ASSIGNING FIELD-SYMBOL(<fv_csv>).
        "Recupera campo da tab interna
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_itb> TO FIELD-SYMBOL(<fv_itb>).
        CHECK sy-subrc IS INITIAL.

        "Converte string para tipo interno
        DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data( <fv_itb> ).
        CASE lo_typedescr->type_kind.
          WHEN 'D'.
            "Data
            me->conversion_date( EXPORTING input  = <fv_csv>
                                 IMPORTING output = <fv_itb> ).
          WHEN 'T'.
            "Hora
            me->conversion_time( EXPORTING input  = <fv_csv>
                                 IMPORTING output = <fv_itb> ).
          WHEN 'P'.
            "Valor decimal
            me->conversion_decimal( EXPORTING input  = <fv_csv>
                                    IMPORTING output = <fv_itb> ).
          WHEN OTHERS.
            "Exit de conversao ALPHA
            me->conversion_alpha_exit( EXPORTING input  = <fv_csv>
                                                 outtyp = lo_typedescr
                                       IMPORTING output = <fv_itb> ).
        ENDCASE.

      ENDLOOP.

      IF <fs_itb> IS NOT INITIAL.
        "Insere linha na ITAB
        INSERT <fs_itb> INTO TABLE <ft_itb>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD conversion_alpha_exit.

    DATA:
      lo_outtyp TYPE REF TO cl_abap_elemdescr,
      lv_input  TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Recupera instancia do tipo
    lo_outtyp ?= outtyp.
    IF lo_outtyp IS INITIAL.
      lo_outtyp ?= cl_abap_typedescr=>describe_by_data( output ).
    ENDIF.

    "Recupera exit de conversao
    lo_outtyp->get_ddic_field(
      RECEIVING
        p_flddescr   = DATA(ls_fldattr)
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF ls_fldattr-convexit IS NOT INITIAL.
      "Define nome da funcao da rotina de conversao
      DATA(lv_fmname) = |CONVERSION_EXIT_{ condense( ls_fldattr-convexit ) }_INPUT|.

      "Executa rotina de conversao
      CALL FUNCTION lv_fmname
        EXPORTING
          input         = lv_input
        IMPORTING
          output        = lv_input
        EXCEPTIONS
          length_error  = 1
          error_message = 2
          OTHERS        = 3 ##FM_SUBRC_OK.
    ENDIF.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD conversion_date.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS '/'.
      TRANSLATE lv_input USING '/.'.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    "Converte data (DD.MM.AAAA -> AAAAMMDD)
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_input
        accept_initial_date      = abap_true
      IMPORTING
        date_internal            = lv_input
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD conversion_decimal.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS ','.
      TRANSLATE lv_input USING ',.'.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD conversion_time.

    DATA:
      lv_input TYPE string.

    CLEAR output.
    CHECK input IS NOT INITIAL.

    "Recupera input
    lv_input = condense( input ).

    "Trata separadores
    IF lv_input CS ':'.
      TRANSLATE lv_input USING ': '.
    ENDIF.

    CONDENSE lv_input NO-GAPS.

    TRY.
        "Move valor
        output = lv_input.
      CATCH cx_sy_conversion_error INTO DATA(lx_converror).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_converror.
      CATCH cx_root INTO DATA(lx_root).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_fileread_xls IMPLEMENTATION.

  METHOD read_locl.

    "Importa binario
    super->read_locl( ).

    "Move dados para tabela interna estruturada
    me->parse( ).

  ENDMETHOD.

  METHOD read_serv.

    "Importa binario
    super->read_serv( ).

    "Move dados para tabela interna estruturada
    me->parse( ).

  ENDMETHOD.

  METHOD parse.

    DATA:
      lo_dataline TYPE REF TO data,
      lv_xstring  TYPE xstring.

    FIELD-SYMBOLS:
      <ft_xls> TYPE ANY TABLE,
      <ft_itb> TYPE ANY TABLE,
      <fs_itb> TYPE any.

    CHECK me->t_solix[] IS NOT INITIAL.

    "Transforma binario em xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = me->v_binlen
      IMPORTING
        buffer       = lv_xstring
      TABLES
        binary_tab   = me->t_solix[]
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CHECK lv_xstring IS NOT INITIAL.

    "Recupera dados da tabela
    DATA(lo_data) = me->o_itab->get_data( ).

    "Recupera tabela interna
    ASSIGN lo_data->* TO <ft_itb>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Cria linha do tipo da tabela interna
    CREATE DATA lo_dataline LIKE LINE OF <ft_itb>.
    IF sy-subrc IS INITIAL.
      ASSIGN lo_dataline->* TO <fs_itb>.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao criar estrutura da tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY .
        "Instancia classe de tratamento de excel
        DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
          document_name = 'Excel_Document'
          xdocument     = lv_xstring
        ).

        "Le sheets
        lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
          IMPORTING
            worksheet_names = DATA(lt_worksheets)
        ).

        "Le dados da sheet
        IF lt_worksheets[] IS NOT INITIAL.
          DATA(lo_filelist) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheets[ 1 ] ).
          ASSIGN lo_filelist->* TO <ft_xls>.
        ENDIF.

        IF <ft_xls> IS ASSIGNED.
          "Move dados
          LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<fs_xls>).
            IF me->s_fileattr-v_hdri = abap_true.
              CHECK sy-tabix <> 1.
            ENDIF.

            CLEAR <fs_itb>.

            DO.
              DATA(lv_index) = sy-index.
              ASSIGN COMPONENT lv_index OF STRUCTURE <fs_xls> TO FIELD-SYMBOL(<fv_xls>).
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              ASSIGN COMPONENT lv_index OF STRUCTURE <fs_itb> TO FIELD-SYMBOL(<fv_itb>).
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
              <fv_itb> = <fv_xls>.
            ENDDO.

            IF <fs_itb> IS NOT INITIAL.
              INSERT <fs_itb> INTO TABLE <ft_itb>.
            ENDIF.
          ENDLOOP.
        ENDIF.

      CATCH cx_fdt_excel_core INTO DATA(lx_excel_core).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_excel_core.
      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_fileread_pdf IMPLEMENTATION.

  METHOD display.

    DATA:
      lv_content TYPE fpcontent.

    CHECK me->t_solix[] IS NOT INITIAL.

    LOOP AT me->t_solix[] ASSIGNING FIELD-SYMBOL(<fs_solix>).
      IF lv_content IS INITIAL.
        lv_content = <fs_solix>-line.
      ELSE.
        CONCATENATE lv_content <fs_solix>-line
          INTO lv_content IN BYTE MODE.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'EFG_DISPLAY_PDF'
      EXPORTING
        i_pdf = lv_content.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Definicao das classes para criacao de arquivo
*&---------------------------------------------------------------------*
CLASS lcl_abap_filecreate DEFINITION
  INHERITING FROM lcl_abap_file
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING iv_fileexte        TYPE lif_abap_file=>ty_extension
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_abap_filecreate
      RAISING   lcx_abap_file .

    METHODS set_itabref
      IMPORTING io_itab TYPE REF TO lcl_abap_itab
      RAISING   lcx_abap_file .

    METHODS get_itabref
      RETURNING VALUE(ro_itab) TYPE REF TO lcl_abap_itab
      RAISING   lcx_abap_file .

    METHODS set_binary
      IMPORTING it_solix  TYPE solix_tab
                iv_lenght TYPE i
      RAISING   lcx_abap_file .

    METHODS save
      RAISING lcx_abap_file .

  PROTECTED SECTION.
    DATA:
      t_solix  TYPE solix_tab,
      v_binlen TYPE i,
      o_itab   TYPE REF TO lcl_abap_itab.

    METHODS save_locl
      RAISING lcx_abap_file .

    METHODS save_serv
      RAISING lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_filecreate_csv DEFINITION
  INHERITING FROM lcl_abap_filecreate
  CREATE PUBLIC .

  PROTECTED SECTION.
    METHODS save_locl REDEFINITION .
    METHODS save_serv REDEFINITION .

  PRIVATE SECTION.
    DATA:
      t_csv  TYPE stringtab.

    METHODS conv_to_stringtab
      RAISING lcx_abap_file .

    METHODS get_header
      RETURNING VALUE(rv_header) TYPE string
      RAISING   lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_filecreate_xls DEFINITION
  INHERITING FROM lcl_abap_filecreate
  CREATE PUBLIC .

  PROTECTED SECTION.
    METHODS save_locl REDEFINITION .
    METHODS save_serv REDEFINITION .

  PRIVATE SECTION.
    METHODS conv_to_solix
      RAISING lcx_abap_file .

ENDCLASS.

*&---------------------------------------------------------------------*
* Implementacao das classes para criacao de arquivo
*&---------------------------------------------------------------------*
CLASS lcl_abap_filecreate IMPLEMENTATION.

  METHOD get_instance.

    CLEAR ro_instance.

    "Valida tipo do arquivo
    IF iv_fileexte IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tipo do arquivo é inválido'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    CASE iv_fileexte.
      WHEN lif_abap_file=>c_extension-csv
        OR lif_abap_file=>c_extension-txt.
        "Instancia classe para salvar csv
        ro_instance = NEW lcl_abap_filecreate_csv( ).

      WHEN lif_abap_file=>c_extension-xls
        OR lif_abap_file=>c_extension-xlsx.
        "Instancia classe para salvar excel
        ro_instance = NEW lcl_abap_filecreate_xls( ).

      WHEN lif_abap_file=>c_extension-pdf.
        "Instancia classe para salvar pdf
        ro_instance = NEW lcl_abap_filecreate( ).

      WHEN OTHERS.
        MESSAGE e001(00) INTO lv_dummy
          WITH 'Tipo ' iv_fileexte ' desconhecido'.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).
    ENDCASE.

    ro_instance->s_fileattr-v_exte = iv_fileexte.

  ENDMETHOD.

  METHOD set_binary.
    CLEAR: me->t_solix[], me->v_binlen.
    me->t_solix[] = it_solix[].
    me->v_binlen  = iv_lenght.
*    IF me->v_binlen IS INITIAL.
*      DESCRIBE FIELD me->t_solix LENGTH me->v_binlen IN BYTE MODE.
*    ENDIF.
  ENDMETHOD.

  METHOD set_itabref.

    me->o_itab = io_itab.
    IF me->o_itab IS INITIAL.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Tabela interna inexistente'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD get_itabref.

    ro_itab = me->o_itab.

  ENDMETHOD.

  METHOD save.

    CASE me->s_fileattr-v_syst.
      WHEN lif_abap_file=>c_system-locl.
        "Salva arquivo local
        me->save_locl( ).

      WHEN lif_abap_file=>c_system-serv.
        "Salva arquivo no servidor (AL11)
        me->save_serv( ).

      WHEN OTHERS.
        "Destino do arquivo desconhecido
        MESSAGE e001(00) INTO DATA(lv_dummy)
          WITH 'Destino do arquivo desconhecido'.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            symsg = CORRESPONDING #( sy ).

    ENDCASE.

  ENDMETHOD.

  METHOD save_locl.

    CHECK me->t_solix[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = me->v_binlen
        filename                  = me->s_fileattr-v_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = me->t_solix[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD save_serv.

    CHECK me->t_solix[] IS NOT INITIAL.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Insere dados no arquivo
    LOOP AT me->t_solix[] ASSIGNING FIELD-SYMBOL(<fs_binary>).
      TRANSFER <fs_binary> TO me->s_fileattr-v_path.
    ENDLOOP.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao salvar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_filecreate_csv IMPLEMENTATION.

  METHOD save_locl.

    "Recupera tabela de string
    me->conv_to_stringtab( ).
    CHECK me->t_csv[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = me->s_fileattr-v_path
        filetype                  = 'ASC'
      CHANGING
        data_tab                  = me->t_csv[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD save_serv.

    "Recupera tabela de string
    me->conv_to_stringtab( ).
    CHECK me->t_csv[] IS NOT INITIAL.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR OUTPUT IN TEXT MODE
      ENCODING DEFAULT IGNORING CONVERSION ERRORS
      WITH SMART LINEFEED.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Insere dados no arquivo
    LOOP AT me->t_csv[] ASSIGNING FIELD-SYMBOL(<fs_txt>).
      TRANSFER <fs_txt> TO me->s_fileattr-v_path.
    ENDLOOP.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.
    IF sy-subrc <> 0.
      "Erro ao salvar o arquivo
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao salvar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD conv_to_stringtab.

    DATA:
      lv_txt TYPE string.
    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: me->t_csv[].

    "Recupera dados da tabela
    DATA(lo_data) = me->o_itab->get_data( ).

    "Recupera tabela
    ASSIGN lo_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      "Erro ao criar tabela interna
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    IF me->s_fileattr-v_hdri = abap_true.
      "Insere cabecalho
      APPEND me->get_header( ) TO me->t_csv[].
    ENDIF.

    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
      CLEAR lv_txt.

      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fv_data>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF lv_txt IS INITIAL.
          lv_txt = <fv_data>.
        ELSE.
          lv_txt = |{ lv_txt }{ me->s_fileattr-v_sepa }{ <fv_data> }|.
        ENDIF.
      ENDDO.
      CHECK lv_txt IS NOT INITIAL.

      APPEND lv_txt TO me->t_csv[].
    ENDLOOP.

  ENDMETHOD.

  METHOD get_header.

    DATA:
      lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
      lo_structdescr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: rv_header.

    "Recupera tabela
*    ASSIGN me->o_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      "Erro ao criar tabela interna
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY.
        "Recupera campos
        lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data( <ft_data> ).
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
        DATA(lt_fields) = lo_structdescr->get_ddic_field_list( ).

        "Monta linha de cabecalho
        LOOP AT lt_fields[] ASSIGNING FIELD-SYMBOL(<fs_field>).
          IF rv_header IS INITIAL.
            rv_header = <fs_field>-scrtext_l.
          ELSE.
            rv_header = |{ rv_header }{ me->s_fileattr-v_sepa }{ <fs_field>-scrtext_l }|.
          ENDIF.
        ENDLOOP.

      CATCH cx_root INTO DATA(lx_root) ##CATCH_ALL.
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abap_filecreate_xls IMPLEMENTATION.

  METHOD save_locl.

    "Recupera binario
    me->conv_to_solix( ).
    CHECK me->t_solix[] IS NOT INITIAL.

    "Realiza download
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = me->v_binlen
        filename                  = me->s_fileattr-v_path
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = me->t_solix[]
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD save_serv.

    "Recupera binario
    me->conv_to_solix( ).
    CHECK me->t_solix[] IS NOT INITIAL.

    "Abre arquivo para leitura
    OPEN DATASET me->s_fileattr-v_path FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      "Erro ao criar o arquivo
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    "Insere dados no arquivo
    LOOP AT me->t_solix[] ASSIGNING FIELD-SYMBOL(<fs_solix>).
      TRANSFER <fs_solix> TO me->s_fileattr-v_path.
    ENDLOOP.

    "Fecha arquivo
    CLOSE DATASET me->s_fileattr-v_path.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO lv_dummy
        WITH 'Erro ao salvar o arquivo'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

  ENDMETHOD.

  METHOD conv_to_solix.

    FIELD-SYMBOLS:
      <ft_data> TYPE ANY TABLE.

    CLEAR: me->t_solix[], me->v_binlen.

    "Recupera dados da tabela
    DATA(lo_data) = me->o_itab->get_data( ).

    "Recupera tabela
    ASSIGN lo_data->* TO <ft_data>.
    IF sy-subrc <> 0.
      MESSAGE e001(00) INTO DATA(lv_dummy)
        WITH 'Erro ao criar tabela interna'.
      RAISE EXCEPTION TYPE lcx_abap_file
        EXPORTING
          symsg = CORRESPONDING #( sy ).
    ENDIF.

    TRY .
        "Cria instancia da salv
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = <ft_data>
        ).

        "Recupera XSTRING do excel
        DATA(lv_xstring) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx  ).

        "Converte para binario
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = me->v_binlen
          TABLES
            binary_tab    = me->t_solix[]
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_abap_file
            EXPORTING
              symsg = CORRESPONDING #( sy ).
        ENDIF.

      CATCH cx_salv_msg INTO DATA(lo_except).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lo_except.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Classe factory
*&---------------------------------------------------------------------*
CLASS lcl_abap_file_factory DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS read_file_to_itab
      IMPORTING iv_path TYPE lif_abap_file=>ty_filepath
                iv_syst TYPE lif_abap_file=>ty_system DEFAULT lif_abap_file=>c_system-locl
                iv_hdri TYPE lif_abap_file=>ty_hdrind DEFAULT 'X'
                iv_sepa TYPE lif_abap_file=>ty_separator DEFAULT ';'
      EXPORTING et_data TYPE ANY TABLE
      RAISING   lcx_abap_file .

    CLASS-METHODS read_file_to_binary
      IMPORTING iv_path    TYPE lif_abap_file=>ty_filepath
                iv_syst    TYPE lif_abap_file=>ty_system DEFAULT lif_abap_file=>c_system-locl
      EXPORTING et_binary  TYPE solix_tab
                ev_xstring TYPE xstring
      RAISING   lcx_abap_file .

    CLASS-METHODS create_file_from_itab
      IMPORTING it_data TYPE ANY TABLE
                iv_path TYPE lif_abap_file=>ty_filepath
                iv_syst TYPE lif_abap_file=>ty_system DEFAULT lif_abap_file=>c_system-locl
                iv_hdri TYPE lif_abap_file=>ty_hdrind DEFAULT 'X'
                iv_sepa TYPE lif_abap_file=>ty_separator DEFAULT ';'
      RAISING   lcx_abap_file .

    CLASS-METHODS create_file_from_binary
      IMPORTING it_binary TYPE solix_tab
                iv_binlen TYPE i
                iv_path   TYPE lif_abap_file=>ty_filepath
                iv_syst   TYPE lif_abap_file=>ty_system DEFAULT lif_abap_file=>c_system-locl
      RAISING   lcx_abap_file .

ENDCLASS.

CLASS lcl_abap_file_factory IMPLEMENTATION.

  METHOD read_file_to_itab.

    CLEAR et_data[].

    "Recupera extensao
    DATA(lv_extension) = lcl_abap_file=>get_extension( iv_path ).
    CHECK lv_extension = lif_abap_file=>c_extension-xls
       OR lv_extension = lif_abap_file=>c_extension-xlsx
       OR lv_extension = lif_abap_file=>c_extension-csv
       OR lv_extension = lif_abap_file=>c_extension-txt.

    TRY.
        "Cria instancia
        DATA(lo_fileread) = lcl_abap_fileread=>get_instance( lv_extension ).

        "Seta tabela interna
        lo_fileread->set_itabref(
          io_itab = lcl_abap_itab=>create_from_itab( et_data[] )
        ).

        "Seta atributos do arquivo
        lo_fileread->set_fileattr( iv_path = iv_path     "caminho do arquivo
                                   iv_syst = iv_syst     "origem do arquivo (SERV / LOCL)
                                   iv_hdri = iv_hdri     "indicador de cabecalho
                                   iv_sepa = iv_sepa ).  "separador (para CSV)

        "Le arquivo
        lo_fileread->read( ).

        "Recupera dados
        lo_fileread->get_itabref( )->get_values( IMPORTING et_values = et_data[] ).

      CATCH lcx_abap_itab INTO DATA(lx_itab).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_itab.
    ENDTRY.

  ENDMETHOD.

  METHOD read_file_to_binary.

    CLEAR: et_binary[], ev_xstring.

    "Cria instancia
    DATA(lo_fileread) = NEW lcl_abap_fileread( ).

    "Seta atributos do arquivo
    lo_fileread->set_fileattr( iv_path = iv_path     "caminho do arquivo
                               iv_syst = iv_syst ).  "origem do arquivo (SERV / LOCL)

    "Le arquivo
    lo_fileread->read( ).

    IF et_binary IS REQUESTED.
      "Recupera binario
      lo_fileread->get_binary( IMPORTING et_data = et_binary[] ).
    ENDIF.
    IF ev_xstring IS REQUESTED.
      "Recupera xstring
      ev_xstring = lo_fileread->get_xstring( ).
    ENDIF.

  ENDMETHOD.

  METHOD create_file_from_itab.

    TRY.
        "Recupera extensao
        DATA(lv_extension) = lcl_abap_file=>get_extension( iv_path ).
        CHECK lv_extension = lif_abap_file=>c_extension-xls
           OR lv_extension = lif_abap_file=>c_extension-xlsx
           OR lv_extension = lif_abap_file=>c_extension-csv
           OR lv_extension = lif_abap_file=>c_extension-txt.

        "Cria tabela interna
        DATA(lo_itab) = lcl_abap_itab=>create_from_itab( it_data[] ).

        "Seta dados na tabela interna
        lo_itab->set_values( it_data[]  ).

        "Cria instancia
        DATA(lo_filecreate) = lcl_abap_filecreate=>get_instance( lv_extension ).

        "Seta tabela interna
        lo_filecreate->set_itabref( lo_itab ).

        "Seta atributos do arquivo
        lo_filecreate->set_fileattr( iv_path = iv_path     "caminho do arquivo
                                     iv_syst = iv_syst     "origem do arquivo (SERV / LOCL)
                                     iv_hdri = iv_hdri     "indicador de cabecalho
                                     iv_sepa = iv_sepa ).  "separador (para CSV)

        "Salva dados
        lo_filecreate->save( ).

      CATCH lcx_abap_itab INTO DATA(lx_itab).
        RAISE EXCEPTION TYPE lcx_abap_file
          EXPORTING
            previous = lx_itab.
    ENDTRY.

  ENDMETHOD.

  METHOD create_file_from_binary.

    "Cria instancia
    DATA(lo_filecreate) = NEW lcl_abap_filecreate( ).

    "Seta atributos do arquivo
    lo_filecreate->set_fileattr( iv_path = iv_path     "caminho do arquivo
                                 iv_syst = iv_syst ).  "origem do arquivo (SERV / LOCL)

    "Seta dados
    lo_filecreate->set_binary( it_solix  = it_binary[]
                               iv_lenght = iv_binlen ).

    "Salva dados
    lo_filecreate->save( ).

  ENDMETHOD.

ENDCLASS.