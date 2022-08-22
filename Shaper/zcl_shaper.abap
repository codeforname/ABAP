*&---------------------------------------------------------------------*
*&  Include           ZCL_SHAPER
*&---------------------------------------------------------------------*

CLASS zcl_shaper DEFINITION.

  PUBLIC SECTION.

    TYPES: ty_s_component TYPE abap_componentdescr,
           ty_t_component TYPE abap_component_tab.

    CLASS-METHODS:
      get_structdescr
        IMPORTING ix_any TYPE any
        RETURNING value(rr_structdescr) TYPE REF TO cl_abap_structdescr,
      get_fieldcat
        IMPORTING ix_any TYPE any
        RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat,
      get_components
        IMPORTING ir_structdescr TYPE REF TO cl_abap_structdescr
        RETURNING value(rt_component) TYPE ty_t_component,
      copy_table_content
        IMPORTING ir_from_table TYPE REF TO data
                  ir_to_table TYPE REF TO data,
      is_table_ref
        IMPORTING ir_table TYPE any
        RETURNING value(rv_is) TYPE abap_bool.

    METHODS:
      add_structure
        IMPORTING ix_any TYPE any,
      add_table
        IMPORTING ix_table TYPE any,
      add_table_with_data
        IMPORTING ix_table TYPE any,
      add_data
        IMPORTING ix_table TYPE any,
      add_component
        IMPORTING iv_name TYPE string
                  iv_type TYPE string,
      add_as_table
        IMPORTING iv_name TYPE string
                  ix_any TYPE any,
      get_dynamic_table
        RETURNING value(rr_table) TYPE REF TO data.

  PRIVATE SECTION.

    DATA: mt_component TYPE ty_t_component,
          mr_table TYPE REF TO data.

    CLASS-METHODS:
      get_elemdescr
        IMPORTING is_dfies TYPE dfies
        RETURNING value(rr_elemdescr) TYPE REF TO cl_abap_elemdescr,
      to_component
        IMPORTING iv_name TYPE string
                  iv_type TYPE string
        RETURNING VALUE(rs_component) TYPE ty_s_component,
      to_tableref
        IMPORTING ix_table TYPE any
        RETURNING VALUE(rr_table) TYPE REF TO data.

    METHODS:
      add_any
        IMPORTING ix_any TYPE any.


ENDCLASS.                    "zcl_shaper DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_shaper IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_shaper IMPLEMENTATION.

  METHOD get_structdescr.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr,
          lr_tabledescr TYPE REF TO cl_abap_tabledescr.

    lr_typedescr = cl_abap_datadescr=>describe_by_data( ix_any ).
    IF lr_typedescr->kind = cl_abap_datadescr=>kind_ref.
      lr_typedescr = cl_abap_datadescr=>describe_by_data_ref( ix_any ).
    ENDIF.

    CASE lr_typedescr->kind.
      WHEN cl_abap_datadescr=>kind_elem.

      WHEN cl_abap_datadescr=>kind_struct.
        rr_structdescr ?= lr_typedescr.
      WHEN cl_abap_datadescr=>kind_table.
        lr_tabledescr ?= lr_typedescr.
        rr_structdescr ?= lr_tabledescr->get_table_line_type( ).
    ENDCASE.

  ENDMETHOD.                    "describe

  METHOD get_components.

    DATA: lt_ddfield TYPE ddfields,
          lt_component TYPE ty_t_component,
          ls_component TYPE ty_s_component,

          lr_structdescr TYPE REF TO cl_abap_structdescr,
          lt_subcomponent TYPE ty_t_component.

    FIELD-SYMBOLS: <fs_ddfield> TYPE dfies,
                   <fs_component> TYPE ty_s_component,
                   <fs_subcomponent> TYPE ty_s_component.


    IF ir_structdescr IS NOT BOUND.
      RETURN.
    ENDIF.

    lt_component = ir_structdescr->get_components( ).
    LOOP AT lt_component ASSIGNING <fs_component>.
      IF <fs_component>-as_include = abap_true.

        lr_structdescr ?= <fs_component>-type.
        lt_subcomponent = get_components( lr_structdescr ).
        LOOP AT lt_subcomponent ASSIGNING <fs_subcomponent>.
          APPEND <fs_subcomponent> TO rt_component.
        ENDLOOP.
      ELSE.
        APPEND <fs_component> TO rt_component.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "get_components

  METHOD get_fieldcat.

    DATA: lt_ddfield TYPE ddfields,
          ls_fcat TYPE lvc_s_fcat.

    FIELD-SYMBOLS: <fs_ddfield> TYPE dfies.

    lt_ddfield = cl_salv_data_descr=>read_structdescr( get_structdescr( ix_any ) ).
    LOOP AT lt_ddfield ASSIGNING <fs_ddfield>.
      CLEAR ls_fcat.
      MOVE-CORRESPONDING <fs_ddfield> TO ls_fcat.
      APPEND ls_fcat TO rt_fieldcat.
    ENDLOOP.

  ENDMETHOD.

  METHOD add_component.

    DATA: ls_component TYPE abap_componentdescr.

    ls_component = to_component( iv_name = iv_name iv_type = iv_type ).
    IF ls_component IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE mt_component TRANSPORTING NO FIELDS WITH KEY name = ls_component-name.
    IF sy-subrc = 0.
      DELETE mt_component INDEX sy-tabix.
    ENDIF.

    APPEND ls_component TO mt_component.

  ENDMETHOD.                    "add_component

  METHOD get_elemdescr.

    DATA: lv_intlen TYPE i,
          lv_leng TYPE i,
          lv_decimals TYPE i,
          lv_outputlen TYPE i,

          lv_int1 TYPE int1,
          lv_int2 TYPE int2.

    lv_intlen = is_dfies-intlen.
    lv_leng = is_dfies-leng.
    lv_decimals = is_dfies-decimals.
    lv_outputlen = is_dfies-outputlen.

    CASE is_dfies-inttype.

      WHEN cl_abap_elemdescr=>typekind_int.
        rr_elemdescr = cl_abap_elemdescr=>get_i( ).

      WHEN cl_abap_elemdescr=>typekind_int1.
        rr_elemdescr ?= cl_abap_elemdescr=>describe_by_data( p_data = lv_int1 ).

      WHEN cl_abap_elemdescr=>typekind_int2.
        rr_elemdescr ?= cl_abap_elemdescr=>describe_by_data( p_data = lv_int2 ).

      WHEN cl_abap_elemdescr=>typekind_float.
        rr_elemdescr = cl_abap_elemdescr=>get_f( ).

      WHEN cl_abap_elemdescr=>typekind_date.
        rr_elemdescr = cl_abap_elemdescr=>get_d( ).

      WHEN cl_abap_elemdescr=>typekind_packed.
        rr_elemdescr = cl_abap_elemdescr=>get_p( p_length = lv_leng
                                                 p_decimals = lv_decimals ).

      WHEN cl_abap_elemdescr=>typekind_char.
        rr_elemdescr = cl_abap_elemdescr=>get_c( p_length = lv_outputlen ).

      WHEN cl_abap_elemdescr=>typekind_time.
        rr_elemdescr = cl_abap_elemdescr=>get_t( ).

      WHEN cl_abap_elemdescr=>typekind_num.
        rr_elemdescr = cl_abap_elemdescr=>get_n( p_length = lv_intlen ).

      WHEN cl_abap_elemdescr=>typekind_hex.
        rr_elemdescr = cl_abap_elemdescr=>get_x( p_length = lv_intlen ).

      WHEN cl_abap_elemdescr=>typekind_string.
        rr_elemdescr = cl_abap_elemdescr=>get_string( ).

      WHEN cl_abap_elemdescr=>typekind_xstring.
        rr_elemdescr = cl_abap_elemdescr=>get_xstring( ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.                    "get_elemdescr

  METHOD get_dynamic_table.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr TYPE REF TO cl_abap_tabledescr.

    IF mt_component IS INITIAL.
      RETURN.
    ENDIF.

    lr_structdescr = cl_abap_structdescr=>create( p_components = mt_component ).
    lr_tabledescr = cl_abap_tabledescr=>create( lr_structdescr ).

    CREATE DATA rr_table TYPE HANDLE lr_tabledescr.

    copy_table_content( ir_from_table = mr_table ir_to_table = rr_table ).
    mr_table = rr_table.

  ENDMETHOD.                    "get_dynamic_table

  METHOD add_any.

    DATA: lt_component TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS: <fs_component> TYPE abap_componentdescr.

    lt_component = get_components( get_structdescr( ix_any ) ).

    LOOP AT lt_component ASSIGNING <fs_component>.

      READ TABLE mt_component TRANSPORTING NO FIELDS WITH KEY name = <fs_component>-name.
      IF sy-subrc = 0.
        DELETE mt_component INDEX sy-tabix.
      ENDIF.
      APPEND <fs_component> TO mt_component.

    ENDLOOP.

  ENDMETHOD.                    "add_any

  METHOD add_structure.
    add_any( ix_any = ix_any ).
  ENDMETHOD.                    "add_structure

  METHOD add_table.
    add_any( ix_any = ix_table ).
  ENDMETHOD.                    "add_table

  METHOD add_table_with_data.
    add_any( ix_any = ix_table ).
    add_data( ix_table = ix_table ).
  ENDMETHOD.                    "add_table_with_data

  METHOD add_data.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr,
          lr_tabledescr TYPE REF TO cl_abap_tabledescr,
          is_reference TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS: <fs_source> TYPE STANDARD TABLE,
                   <fs_source_line> TYPE any,
                   <fs_dest> TYPE STANDARD TABLE,
                   <fs_dest_line> TYPE any.

    lr_typedescr = cl_abap_datadescr=>describe_by_data( ix_table ).
    IF lr_typedescr->kind = cl_abap_datadescr=>kind_ref.
      lr_typedescr = cl_abap_datadescr=>describe_by_data_ref( ix_table ).
      is_reference = abap_true.
    ENDIF.

    IF lr_typedescr->kind = cl_abap_datadescr=>kind_table.
      IF is_reference = abap_true.
        ASSIGN ix_table->* TO <fs_source>.
      ELSE.
        ASSIGN ix_table TO <fs_source>.
      ENDIF.
    ENDIF.

    IF <fs_source> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    mr_table = get_dynamic_table( ).
    ASSIGN mr_table->* TO <fs_dest>.

    LOOP AT <fs_source> ASSIGNING <fs_source_line>.
      APPEND INITIAL LINE TO <fs_dest> ASSIGNING <fs_dest_line>.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <fs_source_line> TO <fs_dest_line>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "add_data

  METHOD copy_table_content.

    FIELD-SYMBOLS: <fs_source> TYPE STANDARD TABLE,
                   <fs_source_line> TYPE any,
                   <fs_dest> TYPE STANDARD TABLE,
                   <fs_dest_line> TYPE any.

    IF ir_from_table = ir_to_table OR
       is_table_ref( ir_from_table ) = abap_false OR
       is_table_ref( ir_to_table ) = abap_false.
      RETURN.
    ENDIF.

    ASSIGN ir_from_table->* TO <fs_source>.
    ASSIGN ir_to_table->* TO <fs_dest>.

    LOOP AT <fs_source> ASSIGNING <fs_source_line>.
      APPEND INITIAL LINE TO <fs_dest> ASSIGNING <fs_dest_line>.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <fs_source_line> TO <fs_dest_line>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "copy_table_content

  METHOD is_table_ref.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr,
          is_reference TYPE abap_bool VALUE abap_false.

    IF ir_table IS NOT BOUND.
      RETURN.
    ENDIF.

    lr_typedescr = cl_abap_datadescr=>describe_by_data( ir_table ).
    IF lr_typedescr->kind = cl_abap_datadescr=>kind_ref.
      lr_typedescr = cl_abap_datadescr=>describe_by_data_ref( ir_table ).
      is_reference = abap_true.
    ENDIF.

    IF lr_typedescr->kind = cl_abap_datadescr=>kind_table AND is_reference = abap_true.
      rv_is = abap_true.
    ENDIF.

  ENDMETHOD.                    "is_table_ref

  METHOD add_as_table.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr,
          lr_structdescr TYPE REF TO cl_abap_structdescr,

          ls_component TYPE ty_s_component,
          lt_component TYPE ty_t_component,
          lr_tabledescr TYPE REF TO cl_abap_tabledescr.

    lr_typedescr = cl_abap_datadescr=>describe_by_data( ix_any ).
    IF lr_typedescr->kind = cl_abap_datadescr=>kind_ref.
      lr_typedescr = cl_abap_datadescr=>describe_by_data_ref( ix_any ).
    ENDIF.

    CASE lr_typedescr->kind.
      WHEN cl_abap_datadescr=>kind_elem.

        ls_component = to_component( iv_name = iv_name iv_type = ix_any ).
        IF ls_component IS INITIAL.
          RETURN.
        ENDIF.
        APPEND ls_component TO lt_component.
        lr_structdescr = cl_abap_structdescr=>create( p_components = lt_component ).

      WHEN cl_abap_datadescr=>kind_struct OR cl_abap_datadescr=>kind_table.

        lr_structdescr = get_structdescr( ix_any ).

    ENDCASE.

    IF lr_structdescr IS NOT BOUND.
      RETURN.
    ENDIF.

    lr_tabledescr = cl_abap_tabledescr=>create( lr_structdescr ).
    ls_component-type ?= lr_tabledescr.

    APPEND ls_component TO mt_component.
  ENDMETHOD.                    "add_as_table

  METHOD to_component.

    DATA: lv_name TYPE string,
          lr_typedescr TYPE REF TO cl_abap_typedescr.

    lv_name = iv_name.
    TRANSLATE lv_name TO UPPER CASE.

    cl_abap_elemdescr=>describe_by_name( EXPORTING p_name = iv_type
                                         RECEIVING p_descr_ref = lr_typedescr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rs_component-name = lv_name.
    rs_component-type ?= lr_typedescr.

  ENDMETHOD.


  METHOD to_tableref.

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.

    lr_typedescr = cl_abap_datadescr=>describe_by_data( ix_table ).

    CASE lr_typedescr->kind.

      WHEN cl_abap_datadescr=>kind_table.

        GET REFERENCE OF ix_table INTO rr_table.

      WHEN cl_abap_datadescr=>kind_ref.

        lr_typedescr = cl_abap_datadescr=>describe_by_data_ref( ix_table ).

        IF lr_typedescr->kind = cl_abap_datadescr=>kind_table.
          rr_table = ix_table.
        ENDIF.

    ENDCASE.

  ENDMETHOD.



ENDCLASS.                    "zcl_shaper IMPLEMENTATION
