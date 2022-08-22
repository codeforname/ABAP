*&---------------------------------------------------------------------*
*&  Include           ZCL_MESSAGE
*&---------------------------------------------------------------------*

CLASS zcl_message DEFINITION.

  PUBLIC SECTION.

    CONSTANTS: mc_local_message_delimiter TYPE char1 VALUE '|',
               mc_message_type_information TYPE char1 VALUE 'I',
               mc_message_type_status TYPE char1 VALUE 'S',
               mc_message_type_error TYPE char1 VALUE 'E',
               mc_message_type_warning TYPE char1 VALUE 'W',
               mc_message_type_abort TYPE char1 VALUE 'A',
               mc_message_type_exit TYPE char1 VALUE 'X'.

    TYPES: BEGIN OF ty_s_message,
      class TYPE arbgb,
      no TYPE msgnr,
      text TYPE natxt,
      lang TYPE spras,
    END OF ty_s_message,
    ty_t_message TYPE STANDARD TABLE OF ty_s_message.

    TYPES: ty_t_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS:
      message
        IMPORTING iv_class TYPE arbgb
                  iv_no    TYPE msgnr
                  iv_lang  TYPE spras
                  iv_type  TYPE char2
                  iv_msgv1 TYPE string OPTIONAL
                  iv_msgv2 TYPE string OPTIONAL
                  iv_msgv3 TYPE string OPTIONAL
                  iv_msgv4 TYPE string OPTIONAL,
      set_local_messages
        IMPORTING it_message TYPE ty_t_string.

  PROTECTED SECTION.

    DATA: mt_message TYPE ty_t_message,
          mt_local_message TYPE ty_t_message.

  PRIVATE SECTION.

    METHODS:
      read_from_memory
        IMPORTING iv_class TYPE arbgb
                  iv_no    TYPE msgnr
                  iv_lang  TYPE spras
        RETURNING value(rs_message) TYPE ty_s_message,
      read_from_database
        IMPORTING iv_class TYPE arbgb
                  iv_no    TYPE msgnr
                  iv_lang  TYPE spras
        RETURNING value(rs_message) TYPE ty_s_message,
      read_from_local_definition
        IMPORTING iv_class TYPE arbgb
                  iv_no    TYPE msgnr
                  iv_lang  TYPE spras
        RETURNING value(rs_message) TYPE ty_s_message,
      replace_parameters
        IMPORTING iv_text TYPE natxt
                  iv_msgv1 TYPE string OPTIONAL
                  iv_msgv2 TYPE string OPTIONAL
                  iv_msgv3 TYPE string OPTIONAL
                  iv_msgv4 TYPE string OPTIONAL
        RETURNING value(rv_text) TYPE string.

ENDCLASS.                    "lcl_zslg1_msg DEFINITION

*&---------------------------------------------------------------------*
*&  Include           ZTR_ZSLG1_MSG_IMP
*&---------------------------------------------------------------------*

CLASS zcl_message IMPLEMENTATION.

  METHOD read_from_memory.

    DATA: lv_class TYPE arbgb.

    lv_class = iv_class.
    TRANSLATE lv_class TO UPPER CASE.

    READ TABLE mt_message INTO rs_message WITH KEY class = lv_class no = iv_no lang = iv_lang.

  ENDMETHOD.                    "read_from_memory

  METHOD read_from_database.

    SELECT arbgb AS class
           msgnr AS no
           text
           sprsl AS lang
    FROM t100
    APPENDING CORRESPONDING FIELDS OF TABLE mt_message
    WHERE arbgb = iv_class AND
          sprsl = iv_lang.

    CHECK sy-subrc = 0.

    rs_message = read_from_memory( iv_class = iv_class iv_no = iv_no iv_lang = iv_lang ).

  ENDMETHOD.                    "read_from_database

  METHOD read_from_local_definition.

    DATA: lv_class TYPE arbgb.

    FIELD-SYMBOLS: <fs_message> TYPE ty_s_message.

    lv_class = iv_class.
    TRANSLATE lv_class TO UPPER CASE.

    LOOP AT mt_local_message ASSIGNING <fs_message> WHERE class = lv_class AND lang = iv_lang.
      APPEND <fs_message> TO mt_message.
    ENDLOOP.

    rs_message = read_from_memory( iv_class = iv_class iv_no = iv_no iv_lang = iv_lang ).

  ENDMETHOD.                    "read_from_local_definition

  METHOD message.

    DATA: ls_message TYPE ty_s_message,
          lv_class TYPE arbgb,
          lv_type TYPE char1,
          lv_disp TYPE char1.

    lv_type = zcl_utils=>slice( iv_value = iv_type iv_begin = 0 iv_end = 1 ).
    TRANSLATE lv_type TO UPPER CASE.
    lv_disp = zcl_utils=>slice( iv_value = iv_type iv_begin = 1 ).
    TRANSLATE lv_disp TO UPPER CASE.

    lv_class = iv_class.
    TRANSLATE lv_class TO UPPER CASE.

    ls_message = read_from_memory( iv_class = iv_class iv_no = iv_no iv_lang = iv_lang ).

    IF ls_message IS INITIAL.
      ls_message = read_from_database( iv_class = iv_class iv_no = iv_no iv_lang = iv_lang ).
    ENDIF.

    IF ls_message IS INITIAL.
      ls_message = read_from_local_definition( iv_class = iv_class iv_no = iv_no iv_lang = iv_lang ).
    ENDIF.

    IF ls_message IS INITIAL.
      RETURN.
    ENDIF.

    ls_message-text = replace_parameters( iv_text = ls_message-text iv_msgv1 = iv_msgv1 iv_msgv2 = iv_msgv2 iv_msgv3 = iv_msgv3 iv_msgv4 = iv_msgv4 ).
    MESSAGE ls_message-text TYPE lv_type DISPLAY LIKE lv_disp.

  ENDMETHOD.                    "message

  METHOD set_local_messages.

    " Single line of it_message consists of:
    " message clss | message no | message text | language
    " e.g.
    " 'zslg|001|Brak klasy: &1|L'

    DATA: lv_message TYPE string,

          lv_class TYPE arbgb,
          lv_no TYPE msgnr,
          lv_text TYPE natxt,
          lv_lang TYPE spras,

          ls_message TYPE ty_s_message.

    LOOP AT it_message INTO lv_message.

      SPLIT lv_message AT mc_local_message_delimiter INTO: lv_class lv_no lv_text lv_lang.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR ls_message.
      TRANSLATE lv_class TO UPPER CASE.
      ls_message-class = lv_class.
      ls_message-no = lv_no.
      ls_message-text = lv_text.
      ls_message-lang = lv_lang.

      APPEND ls_message TO mt_local_message.

    ENDLOOP.

  ENDMETHOD.                    "prepare_messages

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

  ENDMETHOD.                    "replace_parameters

ENDCLASS.                    "zcl_message IMPLEMENTATION
