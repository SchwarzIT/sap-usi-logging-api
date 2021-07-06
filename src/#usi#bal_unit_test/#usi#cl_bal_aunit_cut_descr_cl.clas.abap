CLASS /usi/cl_bal_aunit_cut_descr_cl DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  FOR TESTING .

  PUBLIC SECTION.
    CLASS cl_abap_classdescr DEFINITION LOAD .

    DATA classname TYPE classname READ-ONLY .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl .
    METHODS constructor
      IMPORTING
        !i_classname TYPE classname .
    METHODS get_rtti_description
      RETURNING
        VALUE(r_result) TYPE REF TO cl_abap_classdescr .
    METHODS assert_public_attrib_read_only.
    METHODS assert_publ_static_orefs_bound.
    METHODS assert_classname_equals
      IMPORTING
        i_actual_classname TYPE classname.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /usi/cl_bal_aunit_cut_descr_cl IMPLEMENTATION.
  METHOD assert_public_attrib_read_only.
    DATA class_description TYPE REF TO cl_abap_classdescr.
    FIELD-SYMBOLS: <attribute> TYPE abap_attrdescr.

    class_description = get_rtti_description( ).

    READ TABLE  class_description->attributes
      ASSIGNING <attribute>
      WITH KEY  visibility   = cl_abap_classdescr=>public
                is_read_only = abap_false.
    IF sy-subrc EQ 0.
      cl_aunit_assert=>fail(
        msg    = `A public attribute is not READ-ONLY!`
        detail = <attribute>-name
      ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    classname = i_classname.
  ENDMETHOD.


  METHOD get_instance.
    DATA: callstack        TYPE abap_callstack,
          include_resolver TYPE REF TO if_oo_clif_incl_naming.

    FIELD-SYMBOLS: <callstack_line> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 2
      IMPORTING
        callstack = callstack.
    READ TABLE callstack ASSIGNING <callstack_line> INDEX 2.

    cl_oo_include_naming=>get_instance_by_include(
      EXPORTING
        progname       = <callstack_line>-mainprogram
      RECEIVING
        cifref         = include_resolver
      EXCEPTIONS
        no_objecttype  = 1
        internal_error = 2
        OTHERS         = 3
    ).
    IF sy-subrc NE 0.
      cl_aunit_assert=>fail( `Caller is not a class!` ).
    ENDIF.

    CREATE OBJECT r_result
      EXPORTING
        i_classname = include_resolver->cifkey-clsname.
  ENDMETHOD.


  METHOD get_rtti_description.
    DATA: type_description     TYPE REF TO cl_abap_typedescr,
          unexpected_exception TYPE REF TO cx_sy_move_cast_error.

    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = classname
      RECEIVING
        p_descr_ref    = type_description
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc NE 0.
      CLEAR type_description.
    ENDIF.
    cl_aunit_assert=>assert_bound(
      act = type_description
      msg = `Could not get class description!`
    ).

    TRY.
        r_result ?= type_description.
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_classname_equals.
    cl_aunit_assert=>assert_equals(
      exp = classname
      act = i_actual_classname
      msg = `Unexpected classname!`
    ).
  ENDMETHOD.

  METHOD assert_publ_static_orefs_bound.
    DATA class_description TYPE REF TO cl_abap_classdescr.
    FIELD-SYMBOLS: <attribute> TYPE abap_attrdescr,
                   <instance>  TYPE any.

    class_description = get_rtti_description( ).

    LOOP AT class_description->attributes ASSIGNING <attribute> WHERE visibility EQ cl_abap_classdescr=>public
                                                                  AND is_class   EQ abap_true
                                                                  AND type_kind  EQ cl_abap_classdescr=>typekind_oref.
      ASSIGN (classname)=>(<attribute>-name) TO <instance>.
      IF sy-subrc NE 0.
        cl_aunit_assert=>fail(
          msg    = `Could not access public static oref-attribute`
          detail = <attribute>-name
        ).
      ELSE.
      cl_aunit_assert=>assert_bound(
        act = <instance>
        msg = <attribute>-name
      ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
