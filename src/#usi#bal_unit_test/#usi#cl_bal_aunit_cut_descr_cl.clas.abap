CLASS /usi/cl_bal_aunit_cut_descr_cl DEFINITION PUBLIC FINAL CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    DATA classname TYPE classname READ-ONLY.

    "! <h1>Factory</h1>
    "!
    "! @parameter r_result | Helper instance for calling class
    CLASS-METHODS get_instance
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_classname | Class name of calling class
    METHODS constructor
      IMPORTING i_classname TYPE classname.

    "! <h1>Get Class Description</h1>
    "!
    "! @parameter r_result | Class Description (RTTI API)
    METHODS get_rtti_description
      RETURNING VALUE(r_result) TYPE REF TO cl_abap_classdescr.

    "! <h1>Reusable unit test: Public attributes must be READ-ONLY!</h1>
    "!
    "! <p>The method can be called from the test class of a class to consume the check logic.</p>
    "!
    "! <p>Will fail, if non-read-only public attributes exist.</p>
    METHODS assert_public_attrib_read_only.

    "! <h1>Reusable unit test: Public static object references must be bound!</h1>
    "!
    "! <p>The method can be called from the test class of a class to consume the check logic.</p>
    "!
    "! <p>The test is intended for enumeration classes, that will
    "! provide on public static attribute (OREF) per suitable value.</p>
    METHODS assert_publ_static_orefs_bound.

    "! <h1>Reusable unit test for plugin classes</h1>
    "!
    "! <p>Data containers and text containers have a method get_classname that has to return the class name of the
    "! respective class. This is absolutely crucial since their data is converted into an XML-string when saving the
    "! log. When displaying the log, static deserialize-methods of the respective classes need to be called to rebuild
    "! the instances from the XML-data.</p>
    "!
    "! <p>If called from a test class within such a plugin class, this class will know the classname of the calling
    "! class. This allows to write a simple unit test, that compares the return value of get_classname with the real
    "! class name determined by this helper class.</p>
    "!
    "! <p>Will fail, if the passed class name is wrong.</p>
    "!
    "! @parameter i_actual_classname | Actual class name
    METHODS assert_classname_equals
      IMPORTING i_actual_classname TYPE classname.

ENDCLASS.


CLASS /usi/cl_bal_aunit_cut_descr_cl IMPLEMENTATION.
  METHOD assert_classname_equals.
    cl_abap_unit_assert=>assert_equals( exp = classname
                                        act = i_actual_classname
                                        msg = `Unexpected classname!` ).
  ENDMETHOD.

  METHOD assert_public_attrib_read_only.
    DATA(class_description) = get_rtti_description( ).
    TRY.
        DATA(writable_public_attribute) = class_description->attributes[ visibility   = cl_abap_classdescr=>public
                                                                         is_read_only = abap_false ].
        cl_abap_unit_assert=>fail( msg    = `A public attribute is not READ-ONLY!`
                                   detail = writable_public_attribute-name ).
      CATCH cx_sy_itab_line_not_found.
        " Expected / Desired
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD assert_publ_static_orefs_bound.
    FIELD-SYMBOLS: <attribute> TYPE abap_attrdescr,
                   <instance>  TYPE any.

    LOOP AT get_rtti_description( )->attributes
         ASSIGNING <attribute>
         WHERE     visibility = cl_abap_classdescr=>public
               AND is_class   = abap_true
               AND type_kind  = cl_abap_classdescr=>typekind_oref.
      ASSIGN (classname)=>(<attribute>-name) TO <instance>.
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( msg    = `Could not access public static oref-attribute`
                                   detail = <attribute>-name ).
      ELSE.
        cl_abap_unit_assert=>assert_bound( act = <instance>
                                           msg = <attribute>-name ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    classname = i_classname.
  ENDMETHOD.

  METHOD get_instance.
    DATA callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING max_level = 2
      IMPORTING callstack = callstack.

    cl_oo_include_naming=>get_instance_by_include( EXPORTING  progname       = callstack[ 2 ]-mainprogram
                                                   RECEIVING  cifref         = DATA(include_resolver)
                                                   EXCEPTIONS no_objecttype  = 1
                                                              internal_error = 2
                                                              OTHERS         = 3 ).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( `Caller is not a class!` ).
    ENDIF.

    r_result = NEW #( include_resolver->cifkey-clsname ).
  ENDMETHOD.

  METHOD get_rtti_description.
    cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = classname
                                          RECEIVING  p_descr_ref    = DATA(type_description)
                                          EXCEPTIONS type_not_found = 1
                                                     OTHERS         = 2 ).
    IF sy-subrc <> 0.
      CLEAR type_description.
    ENDIF.
    cl_abap_unit_assert=>assert_bound( act = type_description
                                       msg = `Could not get class description!` ).

    r_result ?= type_description.
  ENDMETHOD.
ENDCLASS.
