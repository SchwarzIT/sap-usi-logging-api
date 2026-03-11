CLASS /usi/cl_bal_enum_sub_log_behav DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: create_new_logger           TYPE REF TO /usi/cl_bal_enum_sub_log_behav READ-ONLY,
                reuse_if_sub_object_matches TYPE REF TO /usi/cl_bal_enum_sub_log_behav READ-ONLY,
                reuse_if_log_object_matches TYPE REF TO /usi/cl_bal_enum_sub_log_behav READ-ONLY,
                reuse_existing_logger       TYPE REF TO /usi/cl_bal_enum_sub_log_behav READ-ONLY.

    DATA value TYPE /usi/bal_sub_log_behavior READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Get enumeration-instance by value</h1>
    "!
    "! <p>In some cases the value of an enum instance is known, but for an API call the instance itself is needed.</p>
    "!
    "! <p>This method provides a backwards search and returns the matching instance for the passed value.</p>
    "!
    "! @parameter i_value          | The value to search for
    "! @parameter r_result         | The corresponding instance
    "! @raising   /usi/cx_bal_root | Unsupported value
    CLASS-METHODS get_by_value
      IMPORTING i_value         TYPE /usi/bal_sub_log_behavior
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_sub_log_behav
      RAISING   /usi/cx_bal_root.

    "! <h1>Create instances</h1>
    "!
    "! @parameter i_value | Technical value
    METHODS constructor
      IMPORTING i_value TYPE /usi/bal_sub_log_behavior.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_buffered_instance,
             value    TYPE /usi/bal_sub_log_behavior,
             instance TYPE REF TO /usi/cl_bal_enum_sub_log_behav,
           END   OF ty_buffered_instance,
           ty_buffered_instances TYPE HASHED TABLE OF ty_buffered_instance WITH UNIQUE KEY value.

    CLASS-DATA buffered_instances TYPE ty_buffered_instances.

ENDCLASS.



CLASS /usi/cl_bal_enum_sub_log_behav IMPLEMENTATION.
  METHOD class_constructor.
    create_new_logger           = NEW #( i_value = 0 ).
    reuse_if_sub_object_matches = NEW #( i_value = 3 ).
    reuse_if_log_object_matches = NEW #( i_value = 6 ).
    reuse_existing_logger       = NEW #( i_value = 9 ).
  ENDMETHOD.

  METHOD constructor.
    value = i_value.

    INSERT VALUE #( value    = value
                    instance = me )
           INTO TABLE buffered_instances.
  ENDMETHOD.

  METHOD get_by_value.
    FIELD-SYMBOLS <buffered_instance> TYPE ty_buffered_instance.

    ASSIGN buffered_instances[ value = i_value ] TO <buffered_instance>.

    IF sy-subrc = 0.
      r_result = <buffered_instance>-instance.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
