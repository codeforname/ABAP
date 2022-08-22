CLASS zcl_text DEFINITION.

  PUBLIC SECTION.

    CONSTANTS: mc_local_text_delimiter TYPE char1 VALUE '|'.

    TYPES: BEGIN OF ty_s_text,
      repid TYPE progname,
      lang  TYPE spras,
      key TYPE textpoolky,
      entry TYPE textpooltx,
    END OF ty_s_text,
    ty_t_text TYPE STANDARD TABLE OF ty_s_text WITH KEY key.

    TYPES: ty_t_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS:
      text
        IMPORTING iv_repid TYPE progname
                  iv_lang  TYPE spras
                  iv_key TYPE textpoolky
                  iv_msgv1 TYPE string OPTIONAL
                  iv_msgv2 TYPE string OPTIONAL
                  iv_msgv3 TYPE string OPTIONAL
                  iv_msgv4 TYPE string OPTIONAL
        RETURNING value(rv_text) TYPE string,
      set_local_text
        IMPORTING it_text TYPE ty_t_string.

  PROTECTED SECTION.

    DATA: mt_text TYPE ty_t_text,
          mt_local_text TYPE ty_t_text.

  PRIVATE SECTION.

    METHODS:
      read_from_memory
        IMPORTING iv_repid TYPE progname
                  iv_lang  TYPE spras
                  iv_key TYPE textpoolky
        RETURNING value(rs_text) TYPE ty_s_text,
      read_from_pool
        IMPORTING iv_repid TYPE progname
                  iv_lang  TYPE spras
                  iv_key TYPE textpoolky
        RETURNING value(rs_text) TYPE ty_s_text,
      read_from_local_definition
        IMPORTING iv_repid TYPE progname
                  iv_lang  TYPE spras
                  iv_key TYPE textpoolky
        RETURNING value(rs_text) TYPE ty_s_text,
      replace_parameters
        IMPORTING iv_text TYPE textpooltx
                  iv_msgv1 TYPE string OPTIONAL
                  iv_msgv2 TYPE string OPTIONAL
                  iv_msgv3 TYPE string OPTIONAL
                  iv_msgv4 TYPE string OPTIONAL
        RETURNING value(rv_text) TYPE string.

ENDCLASS.


CLASS zcl_text IMPLEMENTATION.

  METHOD read_from_memory.

    DATA: lv_repid TYPE progname,
          lv_lang TYPE spras,
          lv_key TYPE textpoolky.

    lv_repid = iv_repid.
    TRANSLATE lv_repid TO UPPER CASE.

    lv_lang = iv_lang.
    TRANSLATE lv_lang TO UPPER CASE.

    lv_key = iv_key.
    TRANSLATE lv_key TO UPPER CASE.

    READ TABLE mt_text INTO rs_text WITH KEY repid = lv_repid lang = lv_lang key = lv_key.

  ENDMETHOD.

  METHOD read_from_pool.

    DATA: lt_textpool TYPE STANDARD TABLE OF textpool,
          ls_text TYPE ty_s_text.

    FIELD-SYMBOLS: <fs_textpool> TYPE textpool.

    ls_text-repid = iv_repid.
    TRANSLATE ls_text-repid TO UPPER CASE.
    ls_text-lang = iv_lang.
    TRANSLATE ls_text-lang TO UPPER CASE.

    READ TEXTPOOL ls_text-repid LANGUAGE ls_text-lang INTO lt_textpool.

    LOOP AT lt_textpool ASSIGNING <fs_textpool> WHERE key IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_textpool> TO ls_text.
      APPEND ls_text TO mt_text.
    ENDLOOP.
    CHECK sy-subrc = 0.

    rs_text = read_from_memory( iv_repid = iv_repid iv_lang = iv_lang iv_key = iv_key ).

  ENDMETHOD.

  METHOD read_from_local_definition.

    DATA: lv_repid TYPE progname,
          lv_lang TYPE spras,
          lv_key TYPE textpoolky.

    FIELD-SYMBOLS: <fs_text> TYPE ty_s_text.

    lv_repid = iv_repid.
    TRANSLATE lv_repid TO UPPER CASE.

    lv_lang = iv_lang.
    TRANSLATE lv_lang TO UPPER CASE.

    lv_key = iv_key.
    TRANSLATE lv_key TO UPPER CASE.

    LOOP AT mt_local_text ASSIGNING <fs_text> WHERE repid = lv_repid AND lang = lv_lang AND key = lv_key.
      APPEND <fs_text> TO mt_text.
    ENDLOOP.

    rs_text = read_from_memory( iv_repid = iv_repid iv_lang = iv_lang iv_key = iv_key ).

  ENDMETHOD.

  METHOD text.

    DATA: lv_repid TYPE progname,
          lv_lang TYPE spras,
          lv_key TYPE textpoolky,

          ls_text TYPE ty_s_text.

    ls_text = read_from_memory( iv_repid = iv_repid iv_lang = iv_lang iv_key = iv_key ).

    IF ls_text IS INITIAL.
      ls_text = read_from_pool( iv_repid = iv_repid iv_lang = iv_lang iv_key = iv_key ).
    ENDIF.

    IF ls_text IS INITIAL.
      ls_text = read_from_local_definition( iv_repid = iv_repid iv_lang = iv_lang iv_key = iv_key ).
    ENDIF.

    IF ls_text IS INITIAL.
      RETURN.
    ENDIF.

    rv_text = replace_parameters( iv_text = ls_text-entry iv_msgv1 = iv_msgv1 iv_msgv2 = iv_msgv2 iv_msgv3 = iv_msgv3 iv_msgv4 = iv_msgv4 ).

  ENDMETHOD.

  METHOD set_local_text.

    " Single line of it_message consists of:
    " repid | key | entry | language
    " e.g.
    " 'raport_test|001|Brak klasy: &1|L'

    DATA: lv_raw_text TYPE string,

          lv_repid TYPE progname,
          lv_key TYPE textpoolky,
          lv_entry TYPE string,
          lv_lang TYPE spras,

          ls_text TYPE ty_s_text.

    LOOP AT it_text INTO lv_raw_text.

      SPLIT lv_raw_text AT mc_local_text_delimiter INTO: lv_repid lv_key lv_entry lv_lang.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      TRANSLATE lv_repid TO UPPER CASE.
      TRANSLATE lv_key TO UPPER CASE.
      TRANSLATE lv_lang TO UPPER CASE.

      CLEAR ls_text.
      ls_text-repid = lv_repid.
      ls_text-key = lv_key.
      ls_text-entry = lv_entry.
      ls_text-lang = lv_lang.
      APPEND ls_text TO mt_local_text.

    ENDLOOP.

  ENDMETHOD.

  METHOD replace_parameters.

    rv_text = iv_text.

    IF iv_msgv1 IS SUPPLIED.
      REPLACE ALL OCCURRENCES OF '&1' IN rv_text WITH iv_msgv1.
    ELSEIF iv_msgv2 IS SUPPLIED.
      REPLACE ALL OCCURRENCES OF '&2' IN rv_text WITH iv_msgv2.
    ELSEIF iv_msgv3 IS SUPPLIED.
      REPLACE ALL OCCURRENCES OF '&3' IN rv_text WITH iv_msgv3.
    ELSEIF iv_msgv4 IS SUPPLIED.
      REPLACE ALL OCCURRENCES OF '&4' IN rv_text WITH iv_msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
