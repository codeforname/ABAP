CLASS zcl_ioc DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.

    " Name of the method that is used to inject dependencies.
    CONSTANTS: mc_dependency_method_name TYPE string VALUE 'CONSTRUCTOR'.

    TYPES: ty_me TYPE REF TO zcl_ioc.

    CLASS-METHODS:
      obj
        RETURNING
          value(rr_obj) TYPE ty_me.

    METHODS:
      register
        IMPORTING
          iv_dependency TYPE string
          iv_reliant TYPE string OPTIONAL
          iv_class_name TYPE string,
      resolve
        IMPORTING
          iv_dependency TYPE string
          iv_reliant    TYPE string OPTIONAL
        RETURNING
          value(rr_object) TYPE REF TO object.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_s_dependency,
      dependency TYPE string,
      reliant    TYPE string,
      class_name TYPE string,
      END OF ty_s_dependency,
    ty_t_dependency TYPE STANDARD TABLE OF ty_s_dependency.

    CLASS-DATA:
      mr_me TYPE ty_me.

    DATA:
      mt_dependency TYPE ty_t_dependency.

    METHODS:
      get_dependency
        IMPORTING
          iv_dependency TYPE string
          iv_reliant    TYPE string OPTIONAL
        RETURNING value(rv_class_name) TYPE string,
      has_none_ref_params
        IMPORTING ir_classdescr TYPE REF TO cl_abap_classdescr
                  iv_method_name TYPE string
        RETURNING value(rv_has) TYPE abap_bool,
      get_parameter_type_name
        IMPORTING ir_datadesc TYPE REF TO cl_abap_datadescr
        RETURNING value(rv_name) TYPE string.

ENDCLASS.                    "zcl_ioc_container DEFINITION


CLASS zcl_ioc IMPLEMENTATION.

  METHOD obj.

    IF mr_me IS NOT BOUND.
      CREATE OBJECT mr_me.
    ENDIF.
    rr_obj = mr_me.

  ENDMETHOD.                    "obj


  METHOD get_dependency.

    DATA: ls_dependency TYPE ty_s_dependency,
          lv_dependency TYPE string,
          lv_reliant    TYPE string.

    lv_dependency = iv_dependency.
    TRANSLATE lv_dependency TO UPPER CASE.

    lv_reliant = iv_reliant.
    TRANSLATE lv_reliant TO UPPER CASE.

    READ TABLE mt_dependency INTO ls_dependency WITH KEY dependency = lv_dependency reliant = lv_reliant.
    IF sy-subrc <> 0.
      READ TABLE mt_dependency INTO ls_dependency WITH KEY dependency = lv_dependency.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.
    rv_class_name = ls_dependency-class_name.

  ENDMETHOD.                    "get_registered_mapping

  METHOD register.

    DATA: ls_dependency TYPE ty_s_dependency.

    ls_dependency-dependency = iv_dependency.
    TRANSLATE ls_dependency-dependency TO UPPER CASE.

    ls_dependency-reliant = iv_reliant.
    TRANSLATE ls_dependency-reliant TO UPPER CASE.

    ls_dependency-class_name = iv_class_name.
    TRANSLATE ls_dependency-class_name TO UPPER CASE.

    READ TABLE mt_dependency TRANSPORTING NO FIELDS WITH KEY dependency = ls_dependency-dependency reliant = ls_dependency-reliant.
    IF sy-subrc = 0.
      DELETE mt_dependency WHERE dependency = ls_dependency-dependency AND reliant = ls_dependency-reliant.
    ENDIF.

    APPEND ls_dependency TO mt_dependency.

  ENDMETHOD.                    "register

  METHOD has_none_ref_params.

    DATA: ls_method TYPE abap_methdescr,
          lv_method_name TYPE string.

    FIELD-SYMBOLS: <fs_parameter> TYPE abap_parmdescr.

    lv_method_name = iv_method_name.
    TRANSLATE lv_method_name TO UPPER CASE.

    READ TABLE ir_classdescr->methods INTO ls_method WITH KEY name = lv_method_name.
    LOOP AT ls_method-parameters ASSIGNING <fs_parameter>.

      IF <fs_parameter>-type_kind <> cl_abap_typedescr=>typekind_oref.
        rv_has = abap_true.
        EXIT.
      ENDIF.

      CASE <fs_parameter>-parm_kind.
        WHEN cl_abap_typedescr=>kind_intf OR cl_abap_typedescr=>kind_class.
          CONTINUE.
        WHEN OTHERS.
          rv_has = abap_true.
          EXIT.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "has_none_ref_params

  METHOD resolve.

    DATA: lv_resolved_class_name TYPE string,
          lr_resolved_classdescr TYPE REF TO cl_abap_classdescr,
          ls_resolved_method     TYPE abap_methdescr,
          ls_resolved_parameter  TYPE abap_parmdescr,

          lr_resolved_paramdescr TYPE REF TO cl_abap_datadescr,
          lv_resolved_paramtype  TYPE string,
          lr_resolved_parameter  TYPE REF TO data,

          ls_di_parameter TYPE abap_parmbind,
          lt_di_parameter TYPE abap_parmbind_tab.

    FIELD-SYMBOLS: <fs_resolved_parameter> TYPE any.

    CHECK iv_dependency IS NOT INITIAL.

    lv_resolved_class_name = get_dependency( iv_dependency = iv_dependency iv_reliant = iv_reliant ).
    lr_resolved_classdescr ?= cl_abap_classdescr=>describe_by_name( lv_resolved_class_name ).

    CHECK lr_resolved_classdescr IS BOUND.
    IF has_none_ref_params( ir_classdescr = lr_resolved_classdescr iv_method_name = mc_dependency_method_name ) = abap_true.
      RETURN.
    ENDIF.

    READ TABLE lr_resolved_classdescr->methods INTO ls_resolved_method WITH KEY name = mc_dependency_method_name.
    IF sy-subrc = 0.

      LOOP AT ls_resolved_method-parameters INTO ls_resolved_parameter.

        lr_resolved_classdescr->get_method_parameter_type(
              EXPORTING
                p_method_name       = mc_dependency_method_name
                p_parameter_name    = ls_resolved_parameter-name
              RECEIVING
                p_descr_ref         = lr_resolved_paramdescr
              EXCEPTIONS
                OTHERS              = 1
            ).

        lv_resolved_paramtype = get_parameter_type_name( lr_resolved_paramdescr ).
        CREATE DATA lr_resolved_parameter TYPE REF TO (lv_resolved_paramtype).
        ASSIGN lr_resolved_parameter->* TO <fs_resolved_parameter>.

        <fs_resolved_parameter> ?= resolve( iv_dependency = lv_resolved_paramtype iv_reliant = lv_resolved_class_name ).
        IF <fs_resolved_parameter> IS INITIAL.
          RETURN. " creation not possible
        ENDIF.

        ls_di_parameter-name = ls_resolved_parameter-name.
        ls_di_parameter-kind = cl_abap_objectdescr=>exporting.
        ls_di_parameter-value = lr_resolved_parameter.
        INSERT ls_di_parameter INTO TABLE lt_di_parameter.

      ENDLOOP.

      CREATE OBJECT rr_object TYPE (lv_resolved_class_name)
        PARAMETER-TABLE lt_di_parameter.

    ELSE.

      CREATE OBJECT rr_object
        TYPE (lv_resolved_class_name).

    ENDIF.

  ENDMETHOD.                    "resolve

  METHOD get_parameter_type_name.

    DATA: lr_refdescr TYPE REF TO cl_abap_refdescr,
          lr_typedescr TYPE REF TO cl_abap_typedescr,

          lr_objectdescr TYPE REF TO cl_abap_objectdescr,
          lr_intfdescr TYPE REF TO cl_abap_intfdescr.

    IF ir_datadesc->kind <> cl_abap_typedescr=>kind_ref.
      RETURN.
    ENDIF.

    lr_refdescr ?= ir_datadesc.
    lr_typedescr = lr_refdescr->get_referenced_type( ).

    IF lr_typedescr->kind = cl_abap_typedescr=>kind_class.
      lr_objectdescr ?= lr_typedescr.
      rv_name = lr_objectdescr->get_relative_name( ).
    ELSEIF lr_typedescr->kind = cl_abap_typedescr=>kind_intf.
      lr_intfdescr ?= lr_typedescr.
      rv_name = lr_intfdescr->get_relative_name( ).
    ENDIF.

    TRANSLATE rv_name TO UPPER CASE.
  ENDMETHOD.                    "get_parameter_type_name
ENDCLASS.                    "ZCL_IOC_CONTAINER IMPLEMENTATION
