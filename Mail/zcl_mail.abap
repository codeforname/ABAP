CLASS zcl_mail DEFINITION.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF mc_body_type,
                 raw TYPE char03 VALUE 'RAW',
             END OF mc_body_type.

    CONSTANTS: newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

    TYPES: ty_me TYPE REF TO zcl_mail.

    METHODS:
      constructor,
      set_subject
        IMPORTING iv_subject TYPE string
        RETURNING value(rr_me) TYPE ty_me,
      set_sender
        IMPORTING iv_sender TYPE any OPTIONAL
        RETURNING value(rr_me) TYPE ty_me,
      add_recipient
        IMPORTING iv_recipient TYPE any
        RETURNING value(rr_me) TYPE ty_me,
      add_content
        IMPORTING ix_content TYPE any
        RETURNING value(rr_me) TYPE ty_me,
      clear_content
        RETURNING value(rr_me) TYPE ty_me,
      send.

    CLASS-METHODS:
      is_valid_email
        IMPORTING iv_email TYPE any
        RETURNING value(rv_is) TYPE abap_bool,
      is_valid_sap_user
        IMPORTING iv_username TYPE any
        RETURNING value(rv_is) TYPE abap_bool.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_s_sapuser,
      bname TYPE xubname,
    END OF ty_s_sapuser,
    ty_t_sapuser TYPE STANDARD TABLE OF ty_s_sapuser WITH DEFAULT KEY.

    CLASS-DATA:
      mt_sap_user TYPE ty_t_sapuser.

    DATA:
      mr_mail TYPE REF TO cl_bcs,
      mv_subject TYPE so_obj_des,
      mr_sender TYPE REF TO cl_sapuser_bcs,
      mt_recipient TYPE TABLE OF REF TO if_recipient_bcs,
      mt_content TYPE bcsy_text.

    METHODS:
      is_table
        IMPORTING ix_any TYPE any
        RETURNING value(rv_result) TYPE abap_bool,
      to_string
        IMPORTING iv_value TYPE any
        RETURNING value(rv_value) TYPE string,
      fragment
        IMPORTING ix_value TYPE any
        CHANGING ct_table TYPE STANDARD TABLE,
      get_table_elemdescr
        IMPORTING ix_table TYPE ANY TABLE
        RETURNING value(rr_elemdescr) TYPE REF TO cl_abap_elemdescr,
      get_table_key
        IMPORTING ix_table TYPE ANY TABLE
        RETURNING value(rv_key) TYPE string.


ENDCLASS.

CLASS zcl_mail IMPLEMENTATION.

  METHOD clear_content.
    REFRESH mt_content.
    rr_me = me.
  ENDMETHOD.

  METHOD constructor.
    mr_mail = cl_bcs=>create_persistent( ).
  ENDMETHOD.                    "constructor

  METHOD set_subject.
    mv_subject = iv_subject.
    rr_me = me.
  ENDMETHOD.                    "subject

  METHOD set_sender.

    mr_sender = cl_sapuser_bcs=>create( sy-uname ).
    IF iv_sender IS SUPPLIED AND
       is_valid_sap_user( iv_sender ) = abap_true.

      mr_sender = cl_sapuser_bcs=>create( iv_sender ).
    ENDIF.

    rr_me = me.
  ENDMETHOD.                    "sender

  METHOD add_recipient.

    DATA: lr_recipient TYPE REF TO if_recipient_bcs.

    IF is_valid_email( iv_recipient ) = abap_true.
      lr_recipient = cl_cam_address_bcs=>create_internet_address( iv_recipient ).
    ELSEIF is_valid_sap_user( iv_recipient ) = abap_true.
      lr_recipient = cl_sapuser_bcs=>create( iv_recipient ).
    ENDIF.

    CHECK lr_recipient IS BOUND.
    APPEND lr_recipient TO mt_recipient.

    rr_me = me.
  ENDMETHOD.                    "recipient

  METHOD is_valid_email.

    DATA: lr_regex   TYPE REF TO cl_abap_regex,
          lr_matcher TYPE REF TO cl_abap_matcher.

    CREATE OBJECT lr_regex
      EXPORTING
        pattern     = '\w+(\.\w+)*@(\w+\.)+(\w{2,4})'
        ignore_case = abap_true.

    lr_matcher = lr_regex->create_matcher( text = iv_email ).

    IF lr_matcher->match( ) IS NOT INITIAL.
      rv_is = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_valid_email

  METHOD is_valid_sap_user.

    DATA: lv_sapuser TYPE ty_s_sapuser-bname.

    lv_sapuser = iv_username.
    TRANSLATE lv_sapuser TO UPPER CASE.

    IF mt_sap_user IS INITIAL.
      SELECT bname FROM usr01 INTO CORRESPONDING FIELDS OF TABLE mt_sap_user.
    ENDIF.

    READ TABLE mt_sap_user TRANSPORTING NO FIELDS WITH KEY bname = lv_sapuser.
    IF sy-subrc = 0.
      rv_is = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_valid_sap_user

  METHOD send.

    DATA: lr_document TYPE REF TO cl_document_bcs.

    FIELD-SYMBOLS: <fs_recipient> TYPE REF TO if_recipient_bcs.

    CHECK mr_mail IS BOUND.

    IF mr_sender IS NOT BOUND.
      set_sender( ).
    ENDIF.

    CALL METHOD mr_mail->set_sender
      EXPORTING
        i_sender = mr_sender.

    LOOP AT mt_recipient ASSIGNING <fs_recipient>.

      CALL METHOD mr_mail->add_recipient
        EXPORTING
          i_recipient = <fs_recipient>
          i_express   = 'X'.

    ENDLOOP.

    lr_document = cl_document_bcs=>create_document(
                    i_type    = mc_body_type-raw
                    i_text    = mt_content
                    i_subject = mv_subject ).

    CALL METHOD mr_mail->set_document( lr_document ).

    IF mr_mail->send( ) = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.                    "send

  METHOD add_content.

    DATA: ls_content TYPE soli,
          lt_content TYPE bcsy_text,

          lv_key TYPE string.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_line> TYPE any,
                   <fs_value> TYPE any.

    IF is_table( ix_content ) = abap_true.

      lv_key = get_table_key( ix_content ).
      CHECK lv_key IS NOT INITIAL.

      ASSIGN ix_content TO <fs_table>.
      CHECK <fs_table> IS ASSIGNED.

      LOOP AT <fs_table> ASSIGNING <fs_line>.

        IF lv_key = 'TABLE_LINE'.
          ASSIGN <fs_line> TO <fs_value>..
        ELSE.
          ASSIGN COMPONENT lv_key OF STRUCTURE <fs_line> TO <fs_value>.
        ENDIF.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        fragment( EXPORTING ix_value = to_string( <fs_value> ) CHANGING ct_table = lt_content ).
        APPEND LINES OF lt_content TO mt_content.

      ENDLOOP.

    ELSE.
      fragment( EXPORTING ix_value = to_string( ix_content ) CHANGING ct_table = lt_content ).
      APPEND LINES OF lt_content TO mt_content.
    ENDIF.

    rr_me = me.
  ENDMETHOD.                    "add_content

  METHOD is_table.
    CLEAR rv_result.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.

    lr_typedescr = cl_abap_typedescr=>describe_by_data( ix_any ).
    IF lr_typedescr->type_kind = cl_abap_typedescr=>typekind_table.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_table

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

    lr_elemdescr = get_table_elemdescr( ct_table ).
    CHECK lr_elemdescr IS BOUND.
    lv_available_lenght = lr_elemdescr->output_length.

    CLEAR ct_table.

    IF lv_available_lenght = 0.
      APPEND ix_value TO ct_table.
      RETURN.
    ENDIF.

    SPLIT ix_value AT space INTO TABLE lt_part.
    LOOP AT lt_part INTO lv_part.

      IF strlen( lv_part ) > lv_available_lenght.
        lv_failed = abap_true.
        EXIT.
      ENDIF.

      IF ( strlen( lv_output ) + strlen( lv_part ) + 1 ) > lv_available_lenght.
        IF lv_output IS NOT INITIAL.
          APPEND lv_output TO ct_table.
        ENDIF.
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

  ENDMETHOD.                    "fragment

  METHOD get_table_elemdescr.

    DATA: lr_tabledescr       TYPE REF TO cl_abap_tabledescr,
          lr_structdescr      TYPE REF TO cl_abap_structdescr,

          ls_component        TYPE abap_componentdescr,
          lt_component        TYPE abap_component_tab,

          lv_components_cnt   TYPE i.

    TRY.
        lr_tabledescr ?= cl_abap_datadescr=>describe_by_data( ix_table ).
        rr_elemdescr ?= lr_tabledescr->get_table_line_type( ).
      CATCH cx_sy_move_cast_error.
        TRY .
            lr_structdescr ?= lr_tabledescr->get_table_line_type( ).
            lt_component = lr_structdescr->get_components( ).
            DESCRIBE TABLE lt_component LINES lv_components_cnt.
            IF lv_components_cnt <> 1.
              RETURN.
            ENDIF.

            READ TABLE lt_component INTO ls_component INDEX 1.
            rr_elemdescr ?= ls_component-type.

          CATCH cx_sy_move_cast_error.
            RETURN.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.                    "get_table_elemdescr

  METHOD get_table_key.

    DATA: lr_tabledescr TYPE REF TO cl_abap_tabledescr,
          lv_key_cnt TYPE i.

    lr_tabledescr ?= cl_abap_datadescr=>describe_by_data( ix_table ).
    CHECK lr_tabledescr IS BOUND.

    DESCRIBE TABLE lr_tabledescr->key LINES lv_key_cnt.
    IF lv_key_cnt <> 1.
      RETURN.
    ENDIF.

    READ TABLE lr_tabledescr->key INTO rv_key INDEX 1.

  ENDMETHOD.                    "get_table_key

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

ENDCLASS.
