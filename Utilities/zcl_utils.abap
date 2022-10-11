*----------------------------------------------------------------------*
*       CLASS ZCL_UTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES ty_instance TYPE REF TO zcl_utils.
    TYPES ty_t_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CLASS-METHODS:
      obj
        RETURNING value(rr_obj) TYPE ty_instance,
      move_corresponding
        IMPORTING is_from TYPE any
                  iv_skip_empty TYPE abap_bool OPTIONAL
                  iv_except TYPE string OPTIONAL
        CHANGING cs_to TYPE any,
      move_selected
        IMPORTING is_from TYPE any
                  iv_skip_empty TYPE abap_bool OPTIONAL
                  iv_selection TYPE string
        CHANGING cs_to TYPE any,
      read_text
        IMPORTING iv_object TYPE stxl-tdobject
                  iv_id TYPE stxl-tdid
                  iv_name TYPE stxl-tdname
                  iv_language TYPE stxl-tdspras
        RETURNING value(rv_text) TYPE string,
      get_non_empty_components
        IMPORTING is_src TYPE any
        RETURNING value(rt_component) TYPE ty_t_string,
      expected_values
        IMPORTING ix_any TYPE any
                  iv_values TYPE string
        RETURNING value(rv_covered) TYPE abap_bool,
      is_table
        IMPORTING ix_any TYPE any
        RETURNING VALUE(rv_result) TYPE abap_bool,
      is_structure
        IMPORTING ix_any TYPE any
        RETURNING VALUE(rv_result) TYPE abap_bool,
      length
        IMPORTING ix_any TYPE any
        RETURNING VALUE(rv_length) TYPE i,

    " string conversion methods
      slice
        IMPORTING
          iv_value TYPE any
          iv_begin TYPE i
          iv_end TYPE i OPTIONAL
        RETURNING VALUE(rv_value) TYPE string,


    " conversion methods
      to_string
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE string,
      to_int
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE i,
      to_bool
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE abap_bool,
      to_not_bool
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE abap_bool,
      to_date
        IMPORTING iv_date        TYPE any
                  iv_format      TYPE string OPTIONAL
        RETURNING VALUE(rv_date) TYPE dats,
      fragment
        IMPORTING iv_value type STRING
        CHANGING ct_table type STANDARD TABLE,
  
    " other
      calculate_hash
        IMPORTING iv_content     TYPE string
                  iv_key         TYPE string OPTIONAL
        RETURNING VALUE(rv_hash) TYPE string.

  PRIVATE SECTION.

    CLASS-DATA instance TYPE ty_instance.
ENDCLASS.



CLASS ZCL_UTILS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>EXPECTED_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IX_ANY                         TYPE        ANY
* | [--->] IV_VALUES                      TYPE        STRING
* | [<-()] RV_COVERED                     TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD expected_values.
    CLEAR rv_covered.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr,
          lv_kind TYPE typekind,
          lv_values TYPE string,
          lt_split TYPE ty_t_string,
          lv_column TYPE string,
          lv_fname TYPE string,
          lv_value TYPE string,
          lv_str TYPE string.

    FIELD-SYMBOLS: <fs_value> TYPE string,
                   <fs_any> TYPE any,
                   <fs_table> TYPE ANY TABLE.

    lv_values = iv_values.
    TRANSLATE lv_values TO UPPER CASE.
    SPLIT lv_values AT '|' INTO TABLE lt_split.

    lr_typedescr = cl_abap_typedescr=>describe_by_data( ix_any ).

    lv_kind = lr_typedescr->type_kind.
    CASE lv_kind.
      WHEN cl_abap_typedescr=>typekind_table. " INTERNAL TABLE

        ASSIGN ix_any TO <fs_table>.
        LOOP AT lt_split ASSIGNING <fs_value>.
          SPLIT <fs_value> AT '=' INTO lv_column lv_value.
          IF sy-subrc = 0.
            READ TABLE <fs_table> TRANSPORTING NO FIELDS WITH KEY (lv_column) = lv_value.
            IF sy-subrc <> 0.
              rv_covered = abap_false.
            ENDIF.
          ELSE.
            rv_covered = abap_false. " sorry, wrong format
          ENDIF.
        ENDLOOP.
      WHEN cl_abap_typedescr=>typekind_char.
        READ TABLE lt_split TRANSPORTING NO FIELDS WITH KEY table_line = ix_any.
        IF sy-subrc <> 0.
          rv_covered = abap_false.
        ENDIF.
      WHEN cl_abap_typedescr=>kind_struct OR cl_abap_typedescr=>typekind_struct1.
        LOOP AT lt_split ASSIGNING <fs_value>.
          SPLIT <fs_value> AT '=' INTO lv_fname lv_value.
          IF sy-subrc = 0.
            ASSIGN COMPONENT lv_fname OF STRUCTURE ix_any TO <fs_any>.
            IF sy-subrc = 0.
              lv_str = to_string( <fs_any> ).
              IF lv_str <> lv_value.
                rv_covered = abap_false.
              ENDIF.
            ELSE.
              rv_covered = abap_false. " component not found
            ENDIF.
          ELSE.
            rv_covered = abap_false. " sorry, wrong format
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.                    "expected_values


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>GET_NON_EMPTY_COMPONENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SRC                         TYPE        ANY
* | [<-()] RT_COMPONENT                   TYPE        TY_T_STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_non_empty_components.
    CLEAR rt_component[].

    DATA: lr_strdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <fs_component> TYPE abap_compdescr,
                   <fs_field> TYPE any.

    lr_strdescr ?= cl_abap_typedescr=>describe_by_data( is_src ).

    LOOP AT lr_strdescr->components ASSIGNING <fs_component>.

      ASSIGN COMPONENT <fs_component>-name OF STRUCTURE is_src TO <fs_field>.
      IF <fs_field> IS NOT INITIAL.
        APPEND <fs_component>-name TO rt_component.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_non_empty_components


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>IS_STRUCTURE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IX_ANY                         TYPE        ANY
* | [<-()] RV_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_structure.
    CLEAR rv_result.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.

    lr_typedescr = cl_abap_typedescr=>describe_by_data( ix_any ).
    IF  lr_typedescr->type_kind = cl_abap_typedescr=>kind_struct OR
        lr_typedescr->type_kind = cl_abap_typedescr=>typekind_struct1.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>IS_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IX_ANY                         TYPE        ANY
* | [<-()] RV_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_table.
    CLEAR rv_result.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.

    lr_typedescr = cl_abap_typedescr=>describe_by_data( ix_any ).
    IF lr_typedescr->type_kind = cl_abap_typedescr=>typekind_table.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>LENGTH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IX_ANY                         TYPE        ANY
* | [<-()] RV_LENGTH                      TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD length.
    CLEAR rv_length.

    FIELD-SYMBOLS: <fs_table> TYPE TABLE.

    IF is_table( ix_any = ix_any ) = abap_true.
      ASSIGN ix_any TO <fs_table>.
      " DESCRIBE TABLE <fs_table> LINES rv_length.
      rv_length = lines( <fs_table> ).
    ELSEIF is_structure( ix_any = ix_any ) = abap_false.
      DESCRIBE FIELD ix_any LENGTH rv_length IN CHARACTER MODE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>MOVE_CORRESPONDING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FROM                        TYPE        ANY
* | [--->] IV_SKIP_EMPTY                  TYPE        ABAP_BOOL(optional)
* | [--->] IV_EXCEPT                      TYPE        STRING(optional)
* | [<-->] CS_TO                          TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD move_corresponding.

    DATA: lr_strdescr_from TYPE REF TO cl_abap_structdescr,
          lr_strdescr_to TYPE REF TO cl_abap_structdescr,

          lv_except TYPE string,
          lv_skip_empty TYPE abap_bool,
          lt_split TYPE STANDARD TABLE OF string.

    FIELD-SYMBOLS: <fs_components_from> TYPE abap_compdescr,

                   <fs_field_from> TYPE any,
                   <fs_field_to> TYPE any.

    lv_skip_empty = iv_skip_empty.
    IF iv_skip_empty IS NOT SUPPLIED.
      lv_skip_empty = abap_true. " Default: 'X'
    ENDIF.

    lv_except = iv_except.
    TRANSLATE lv_except TO UPPER CASE.
    SPLIT lv_except AT '|' INTO TABLE lt_split.

    lr_strdescr_from ?= cl_abap_typedescr=>describe_by_data( is_from ).
    lr_strdescr_to ?= cl_abap_typedescr=>describe_by_data( cs_to ).
    LOOP AT lr_strdescr_from->components ASSIGNING <fs_components_from>.

      READ TABLE lt_split TRANSPORTING NO FIELDS WITH KEY table_line = <fs_components_from>-name.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <fs_components_from>-name OF STRUCTURE is_from TO <fs_field_from>.
      IF sy-subrc <> 0.
        CONTINUE. " shuld not happen as we loop through is_from components
      ENDIF.

      ASSIGN COMPONENT <fs_components_from>-name OF STRUCTURE cs_to TO <fs_field_to>.
      IF sy-subrc = 0.
        IF ( lv_skip_empty = abap_true AND <fs_field_from> IS NOT INITIAL ) OR <fs_field_from> IS NOT INITIAL OR lv_skip_empty = abap_false.
          <fs_field_to> = <fs_field_from>.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "move_corresponding


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>MOVE_SELECTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FROM                        TYPE        ANY
* | [--->] IV_SKIP_EMPTY                  TYPE        ABAP_BOOL(optional)
* | [--->] IV_SELECTION                   TYPE        STRING
* | [<-->] CS_TO                          TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD move_selected.

    DATA: lv_selection TYPE string,
          lv_skip_empty TYPE abap_bool,
          lt_split TYPE STANDARD TABLE OF string.

    FIELD-SYMBOLS: <fs_split> TYPE string,

                   <fs_field_from> TYPE any,
                   <fs_field_to> TYPE any.

    lv_skip_empty = iv_skip_empty.
    IF iv_skip_empty IS NOT SUPPLIED.
      lv_skip_empty = abap_true. " Default: 'X'
    ENDIF.

    lv_selection = iv_selection.
    TRANSLATE lv_selection TO UPPER CASE.
    SPLIT lv_selection AT '|' INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING <fs_split> WHERE table_line IS NOT INITIAL.

      ASSIGN COMPONENT <fs_split> OF STRUCTURE is_from TO <fs_field_from>.
      IF sy-subrc <> 0.
        CONTINUE. " field not found in source structure
      ENDIF.

      ASSIGN COMPONENT <fs_split> OF STRUCTURE cs_to TO <fs_field_to>.
      IF sy-subrc = 0.
        IF ( lv_skip_empty = abap_true AND <fs_field_from> IS NOT INITIAL ) OR <fs_field_from> IS NOT INITIAL OR lv_skip_empty = abap_false.
          <fs_field_to> = <fs_field_from>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "move_selected


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>OBJ
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RR_OBJ                         TYPE        TY_INSTANCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD obj.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance.
    ENDIF.
    rr_obj = instance.

  ENDMETHOD.                    "obj


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>READ_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        STXL-TDOBJECT
* | [--->] IV_ID                          TYPE        STXL-TDID
* | [--->] IV_NAME                        TYPE        STXL-TDNAME
* | [--->] IV_LANGUAGE                    TYPE        STXL-TDSPRAS
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_text.
    CLEAR rv_text.

    TYPES: BEGIN OF ty_s_stxl,
      clustr TYPE stxl-clustr,
      clustd TYPE stxl-clustd,
    END OF ty_s_stxl.

    DATA: lt_stxl TYPE STANDARD TABLE OF ty_s_stxl,
          lt_tline TYPE STANDARD TABLE OF tline.

    FIELD-SYMBOLS: <fs_stxl> TYPE ty_s_stxl,
                   <fs_tline> TYPE tline.

    SELECT clustr clustd INTO TABLE lt_stxl
      FROM stxl
      WHERE relid    = 'TX' "standard text
        AND tdobject = iv_object
        AND tdid     = iv_id
        AND tdname   = iv_name
        AND tdspras  = iv_language.

    IF lt_stxl IS INITIAL.
      RETURN.
    ENDIF.

    IMPORT tline = lt_tline FROM INTERNAL TABLE lt_stxl.

    LOOP AT lt_tline ASSIGNING <fs_tline>.
      rv_text = |{ rv_text } { <fs_tline>-tdline }|.
    ENDLOOP.

    CONDENSE rv_text.
  ENDMETHOD.                    "read_text


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>SLICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [--->] IV_BEGIN                       TYPE        I
* | [--->] IV_END                         TYPE        I(optional)
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD slice.

    Data: lv_value TYPE string,
          lv_value_len TYPE i,
          lv_begin_normalized TYPE i,
          lv_end_normalized TYPE i,
          lv_output_len TYPE i.


    lv_value = to_string( iv_value ).
    IF lv_value IS INITIAL.
      RETURN.
    ENDIF.

    lv_value_len = strlen( lv_value ).
    lv_begin_normalized = iv_begin MOD lv_value_len.
    IF iv_end IS NOT SUPPLIED.
      lv_end_normalized = lv_value_len.
    ELSE.
      lv_end_normalized = iv_end MOD lv_value_len.
    ENDIF.

    IF lv_begin_normalized >= lv_end_normalized.
      RETURN.
    ENDIF.

    lv_output_len = lv_end_normalized - lv_begin_normalized.
    rv_value = lv_value+lv_begin_normalized(lv_output_len).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>TO_BOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_VALUE                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_bool.
    CLEAR rv_value.

    DATA: lv_value TYPE string.

    lv_value = to_string( iv_value ).
    TRANSLATE lv_value TO UPPER CASE.

    CASE lv_value.
      WHEN '' OR '0' OR 'FALSE' OR 'HIDE' OR 'HIDDEN' OR 'NULL'.
        rv_value = abap_false.
      WHEN OTHERS.
        rv_value = abap_true.
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>TO_INT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_VALUE                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_int.
    CLEAR rv_value.

    DATA: lv_string TYPE string.

    lv_string = to_string( iv_value ).

    CALL FUNCTION 'CONVERT_STRING_TO_INTEGER'
      EXPORTING
        p_string = lv_string
      IMPORTING
        p_int    = rv_value.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "to_int


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>TO_NOT_BOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_VALUE                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_not_bool.
    CLEAR rv_value.

    rv_value = to_bool( iv_value ).

    IF rv_value = abap_false.
      rv_value = abap_true.
    ELSEIF rv_value = abap_true.
      rv_value = abap_false.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_UTILS=>TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD to_string.
    CLEAR rv_value.

    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

    cl_abap_container_utilities=>fill_container_c(
      EXPORTING
        im_value = iv_value
      IMPORTING
        ex_container = rv_value
      EXCEPTIONS
        illegal_parameter_type = 1 ).

    CONDENSE rv_value.
  ENDMETHOD.                    "to_string

  METHOD to_date.
    CLEAR rv_date.

    DATA: lv_date     TYPE tumls_date,
          lv_format   TYPE tumls_date_format,
          lv_date_out TYPE tumls_date.

    lv_format = 'DDMY'.
    IF iv_format IS SUPPLIED.
      lv_format = iv_format.
      TRANSLATE lv_format TO UPPER CASE.
    ENDIF.

    lv_date = iv_date.
    CALL FUNCTION '/SAPDMC/LSM_DATE_CONVERT'
      EXPORTING
        date_in             = lv_date
        date_format_in      = lv_format
        to_output_format    = ' '
        to_internal_format  = 'X'
      IMPORTING
        date_out            = lv_date_out
      EXCEPTIONS
        illegal_date        = 1
        illegal_date_format = 2
        no_user_date_format = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_date = lv_date_out.
  ENDMETHOD.

  METHOD fragment.

    DATA: lr_tabledescr       TYPE REF TO cl_abap_tabledescr,
          lr_structdescr      TYPE REF TO cl_abap_structdescr,
          lr_elemdescr        TYPE REF TO cl_abap_elemdescr,
          ls_component        TYPE abap_componentdescr,
          lt_component        TYPE abap_component_tab,

          lv_components_cnt   TYPE i,

          lt_part             TYPE STANDARD TABLE OF string,
          lv_part             TYPE string,
          lv_available_lenght TYPE i,
          lv_failed           TYPE abap_bool,

          lv_output           TYPE string.

    TRY.

        lr_tabledescr ?= cl_abap_datadescr=>describe_by_data( ct_table ).
        lr_elemdescr ?= lr_tabledescr->get_table_line_type( ).
        lv_available_lenght = lr_elemdescr->output_length.

      CATCH cx_sy_move_cast_error.

        TRY .
            lr_structdescr ?= lr_tabledescr->get_table_line_type( ).
            lt_component = lr_structdescr->get_components( ).
            DESCRIBE TABLE lt_component LINES lv_components_cnt.
            IF lv_components_cnt <> 1.
              RETURN.
            ENDIF.

            READ TABLE lt_component INTO ls_component INDEX 1.
            lr_elemdescr ?= ls_component-type.
            lv_available_lenght = lr_elemdescr->output_length.

          CATCH cx_sy_move_cast_error.
            RETURN.
        ENDTRY.

    ENDTRY.

    CLEAR ct_table.

    IF lv_available_lenght = 0.
      APPEND iv_value TO ct_table.
      RETURN.
    ENDIF.

    SPLIT iv_value AT space INTO TABLE lt_part.
    LOOP AT lt_part INTO lv_part.

      IF strlen( lv_part ) > lv_available_lenght.
        lv_failed = abap_true.
        EXIT.
      ENDIF.

      IF ( strlen( lv_output ) + strlen( lv_part ) + 1 ) > lv_available_lenght.
        APPEND lv_output TO ct_table.
        lv_output = lv_part.
      ELSE.
        IF lv_output IS INITIAL.
          lv_output = lv_part.
        ELSE.
          lv_output = |{ lv_output } { lv_part }|.
        ENDIF.
      ENDIF.

    ENDLOOP.
    IF sy-subrc = 0 AND strlen( lv_output ) <= lv_available_lenght AND lv_failed = abap_false.
      APPEND lv_output TO ct_table.
    ELSE.
      CLEAR ct_table.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_hash.
    CLEAR rv_hash.

    DATA: lv_data TYPE string,
          lv_len  TYPE i,
          lv_key  TYPE xstring.

    IF iv_key IS SUPPLIED.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = iv_key
        IMPORTING
          buffer = lv_key
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      IF sy-subrc <> 0.
        CLEAR lv_key.
      ENDIF.
    ENDIF.

    lv_len = strlen( iv_content ).

    cl_abap_hmac=>calculate_hmac_for_char(
      EXPORTING
        if_algorithm = 'SHA256'
        if_key = lv_key
        if_data = iv_content
        if_length = lv_len
      IMPORTING
        ef_hmacstring = rv_hash
    ).

    TRANSLATE rv_hash TO UPPER CASE.
  ENDMETHOD.
  
ENDCLASS.
