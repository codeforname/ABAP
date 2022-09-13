CLASS zcl_exec_time DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_exec_time,
      tag TYPE string,
      duration TYPE timestampl,
      created_date TYPE datum,
      created_time TYPE uzeit,
      created_order TYPE i,
    END OF ty_s_exec_time,
    ty_t_exec_time TYPE STANDARD TABLE OF ty_s_exec_time.

    TYPES: ty_me TYPE REF TO zcl_exec_time.

    CLASS-METHODS:
      obj
        RETURNING VALUE(rr_me) TYPE ty_me.

    METHODS:
      put
        IMPORTING
          iv_tag       TYPE string
          iv_duration  TYPE ty_s_exec_time-duration OPTIONAL
          iv_timestamp TYPE timestampl OPTIONAL
        RETURNING value(rr_me) TYPE ty_me,
      start
        IMPORTING
          iv_tag TYPE string,
      stop
        IMPORTING
           iv_tag TYPE string.

  PRIVATE SECTION.

    CLASS-DATA: mr_me TYPE ty_me.

    DATA: mt_exec_time TYPE ty_t_exec_time.

    CLASS-METHODS:
      to_string
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE string,
      slice
        IMPORTING
          iv_value TYPE any
          iv_begin TYPE i
          iv_end TYPE i OPTIONAL
        RETURNING value(rv_value) TYPE string,
      get_timestamp
        RETURNING value(rv_timestamp) TYPE timestampl.

    METHODS:
      format_tag
        IMPORTING
          iv_tag TYPE string
        RETURNING value(rv_tag) TYPE string,
      pop
        IMPORTING iv_tag TYPE string
        RETURNING value(rs_exec_time) TYPE ty_s_exec_time.

ENDCLASS.


CLASS zcl_exec_time IMPLEMENTATION.

  METHOD obj.
    IF mr_me IS INITIAL.
      CREATE OBJECT mr_me.
    ENDIF.
    rr_me = mr_me.
  ENDMETHOD.

  METHOD pop.
    READ TABLE mt_exec_time INTO rs_exec_time WITH KEY tag = format_tag( iv_tag ).
    IF sy-subrc = 0.
      DELETE mt_exec_time INDEX sy-tabix.
    ENDIF.
  ENDMETHOD.

  METHOD start.
    pop( iv_tag ).
    put( iv_tag = format_tag( iv_tag ) ).
  ENDMETHOD.

  METHOD stop.

    DATA: ls_exec_time TYPE ty_s_exec_time,
          lv_created_order TYPE N LENGTH 7,
          lv_exec_time_start TYPE timestampl,
          lv_exec_time_end TYPE timestampl,
          lv_exec_time_differ TYPE timestampl.

    ls_exec_time = pop( iv_tag ).
    IF ls_exec_time IS INITIAL.
      RETURN.
    ENDIF.
    
    lv_created_order = 10000000 - ls_exec_time-created_order.
    lv_exec_time_start = |{ ls_exec_time-created_date }{ ls_exec_time-created_time }.{ lv_created_order }|.
    lv_exec_time_end = get_timestamp( ).
    lv_exec_time_differ = cl_abap_tstmp=>subtract( tstmp1 = lv_exec_time_end tstmp2 = lv_exec_time_start ).

    put( iv_tag = iv_tag iv_duration = lv_exec_time_differ iv_timestamp = lv_exec_time_end ).

  ENDMETHOD.

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
  ENDMETHOD.

  METHOD slice.

    DATA: lv_value TYPE string,
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

  METHOD put.

    DATA: ls_exec_time TYPE ty_s_exec_time,
          lv_timestamp TYPE timestampl.

    IF iv_timestamp IS SUPPLIED.
      lv_timestamp = iv_timestamp.
    ELSE.
      lv_timestamp = get_timestamp( ).
    ENDIF.

    ls_exec_time-tag = iv_tag.
    ls_exec_time-duration = iv_duration.
    ls_exec_time-created_date = slice( iv_begin = 0 iv_end = 8 iv_value = lv_timestamp ).
    ls_exec_time-created_time = slice( iv_begin = 8 iv_end = 14 iv_value = lv_timestamp ).
    ls_exec_time-created_order = 10000000 + slice( iv_begin = -7 iv_value = lv_timestamp ).
    
    APPEND ls_exec_time TO mt_exec_time.
    rr_me = me.

  ENDMETHOD.


  METHOD format_tag.

    rv_tag = iv_tag.
    TRANSLATE rv_tag TO UPPER CASE.
    CONDENSE rv_tag.

  ENDMETHOD.

  METHOD get_timestamp.

    DATA: lv_date TYPE datum,
          lv_time TYPE uzeit.

    GET TIME STAMP FIELD rv_timestamp.
    CONVERT TIME STAMP rv_timestamp TIME ZONE sy-zonlo
             INTO DATE lv_date TIME lv_time.

    rv_timestamp = |{ lv_date }{ lv_time }.{ slice( iv_begin = -7 iv_value = rv_timestamp ) }|.

  ENDMETHOD.

ENDCLASS.
