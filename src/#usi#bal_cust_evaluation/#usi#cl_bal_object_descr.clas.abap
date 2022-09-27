CLASS /usi/cl_bal_object_descr DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.

    "! <h1>Is the object type implementing the interface?</h1>
    "!
    "! @parameter i_interface_name | the interface name
    "! @parameter r_result | abap_bool
    METHODS is_implementing
      IMPORTING
        i_interface_name TYPE seoclsname
      RETURNING
        VALUE(r_result)   TYPE abap_bool.

    "! <h1>Is this class inheriting from i_super_class_name?</h1>
    "!
    "! @parameter i_super_class_name | Super-Class
    "! @parameter r_result | abap_bool
    METHODS is_inheriting_from
      IMPORTING
        i_super_class_name TYPE seoclsname
      RETURNING
        VALUE(r_result)     TYPE abap_bool.

    "! <h1>Is this object type instantiatable (=NOT ABSTRACT)</h1>
    "!
    "! @parameter r_result | abap_bool
    METHODS is_instantiatable
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    "! <h1>Is this object type an interface?</h1>
    "!
    "! @parameter r_result | abap_bool
    METHODS is_interface
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_object_type_name | Class or Interface name
    "! @raising /usi/cx_bal_root | Invalid name
    METHODS constructor
      IMPORTING
        i_object_type_name TYPE seoclsname
      RAISING
        /usi/cx_bal_root.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA object_description TYPE REF TO cl_abap_objectdescr.

ENDCLASS.



CLASS /usi/cl_bal_object_descr IMPLEMENTATION.
  METHOD constructor.
    DATA type_description TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name      = i_object_type_name
      RECEIVING
        p_descr_ref = type_description
      EXCEPTIONS
        OTHERS      = 0 ).

    TRY.
        object_description ?= type_description.
      CATCH cx_sy_move_cast_error.
        CLEAR object_description.
    ENDTRY.

    IF object_description IS NOT BOUND.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.
  ENDMETHOD.


  METHOD is_implementing.
    READ TABLE object_description->interfaces
      TRANSPORTING NO FIELDS
      WITH KEY name = i_interface_name.
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_inheriting_from.
    DATA: super_class_description TYPE REF TO /usi/cl_bal_object_descr,
          own_class_name          TYPE string.

    TRY.
        CREATE OBJECT super_class_description
          EXPORTING
            i_object_type_name = i_super_class_name.
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.

    own_class_name = object_description->get_relative_name( ).
    r_result = super_class_description->object_description->applies_to_class( own_class_name ).
  ENDMETHOD.


  METHOD is_instantiatable.
    r_result  = object_description->is_instantiatable( ).
  ENDMETHOD.


  METHOD is_interface.
    IF object_description->kind EQ cl_abap_typedescr=>kind_intf.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
