CLASS /usi/cl_bal_ce_cx_mapper DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_ce_cx_mapper.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_customizing_dao | DAO-Object
    METHODS constructor
      IMPORTING
        i_customizing_dao TYPE REF TO /usi/if_bal_cd_cx_mapper.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_customizing_entry,
             exception_class_type TYPE seoclstype,
             exception_class_name TYPE /usi/bal_exception_classname,
             mapper_class_name    TYPE /usi/bal_exception_mapper,
           END   OF ty_customizing_entry,
           ty_customizing_entries TYPE HASHED TABLE OF ty_customizing_entry WITH UNIQUE KEY exception_class_type
                                                                                            exception_class_name.

    CONSTANTS: BEGIN OF class_type,
                 class     TYPE seoclstype VALUE 0,
                 interface TYPE seoclstype VALUE 1,
               END   OF class_type.

    DATA: customizing_entries TYPE ty_customizing_entries,
          customizing_dao     TYPE REF TO /usi/if_bal_cd_cx_mapper.

    METHODS get_mapper_class
      IMPORTING
        i_exception_class_description TYPE REF TO cl_abap_classdescr
      RETURNING
        VALUE(r_result)               TYPE /usi/bal_exception_mapper.

    METHODS get_validated_customizing
      RETURNING
        VALUE(r_result) TYPE ty_customizing_entries.

ENDCLASS.



CLASS /usi/cl_bal_ce_cx_mapper IMPLEMENTATION.
  METHOD /usi/if_bal_ce_cx_mapper~get_exception_mapper_classname.
    DATA class_description TYPE REF TO cl_abap_classdescr.
    class_description ?= cl_abap_typedescr=>describe_by_object_ref( i_exception ).
    r_result  = get_mapper_class( class_description ).
  ENDMETHOD.


  METHOD /usi/if_bal_ce_cx_mapper~get_fallback_mapper_classname.
    CONSTANTS fallback_mapper_class TYPE /usi/bal_exception_mapper VALUE '/USI/CL_BAL_EM_BASE'.
    r_result = fallback_mapper_class.
  ENDMETHOD.


  METHOD constructor.
    customizing_dao = i_customizing_dao.
    customizing_entries = get_validated_customizing( ).
  ENDMETHOD.

  METHOD get_mapper_class.
    DATA: exception_classname    TYPE /usi/bal_exception_classname,
          superclass_description TYPE REF TO cl_abap_classdescr.

    FIELD-SYMBOLS: <customizing_entry> TYPE ty_customizing_entry,
                   <interface>         TYPE abap_intfdescr.

    " Check customizing for the class itself
    exception_classname = i_exception_class_description->get_relative_name( ).
    ASSIGN customizing_entries[ exception_class_type = class_type-class
                                exception_class_name = exception_classname ] TO <customizing_entry>.
    IF sy-subrc = 0.
      r_result = <customizing_entry>-mapper_class_name.
      RETURN.
    ENDIF.

    " Check customizing for non-inherited interfaces
    LOOP AT i_exception_class_description->interfaces ASSIGNING <interface> WHERE is_inherited = abap_false.
      ASSIGN customizing_entries[ exception_class_type = class_type-interface
                                  exception_class_name = <interface>-name ] TO <customizing_entry>.
      IF sy-subrc = 0.
        r_result = <customizing_entry>-mapper_class_name.
        EXIT.
      ENDIF.
    ENDLOOP.

    " Check superclass
    IF r_result IS INITIAL.
      i_exception_class_description->get_super_class_type( RECEIVING  p_descr_ref           = superclass_description
                                                           EXCEPTIONS super_class_not_found = 1
                                                                      OTHERS                = 2 ).

      IF sy-subrc = 0.
        r_result = get_mapper_class( superclass_description ).
      ELSE.
        r_result = /usi/if_bal_ce_cx_mapper~get_fallback_mapper_classname( ).
      ENDIF.
    ENDIF.

    " Extend customizing to speed up subsequent calls
    INSERT VALUE #( exception_class_type = class_type-class
                    exception_class_name = i_exception_class_description->get_relative_name( )
                    mapper_class_name    = r_result )
           INTO TABLE customizing_entries.
  ENDMETHOD.

  METHOD get_validated_customizing.
    CONSTANTS: mapper_interface_name TYPE seoclsname VALUE '/USI/IF_BAL_EXCEPTION_MAPPER',
               exception_root_class  TYPE seoclsname VALUE 'CX_ROOT'.

    DATA: raw_customizing_table TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          mapper_description    TYPE REF TO /usi/cl_bal_object_descr,
          exception_description TYPE REF TO /usi/cl_bal_object_descr,
          result_line           TYPE ty_customizing_entry.

    FIELD-SYMBOLS <raw_customizing_entry> TYPE /usi/if_bal_cd_cx_mapper=>ty_record.

    TRY.
        raw_customizing_table = customizing_dao->get_records( ).
      CATCH /usi/cx_bal_root.
        CLEAR raw_customizing_table.
    ENDTRY.

    LOOP AT raw_customizing_table ASSIGNING <raw_customizing_entry>.
      TRY.
          mapper_description = NEW #( i_object_type_name = <raw_customizing_entry>-mapper_class ).

          exception_description = NEW #( i_object_type_name = <raw_customizing_entry>-exception_class ).
        CATCH /usi/cx_bal_root.
          CONTINUE.
      ENDTRY.

      IF NOT (     mapper_description->is_instantiatable( ) = abap_true
               AND mapper_description->is_implementing( mapper_interface_name ) = abap_true ).
        CONTINUE.
      ENDIF.

      IF NOT (    exception_description->is_interface( ) = abap_true
               OR exception_description->is_inheriting_from( exception_root_class ) = abap_true ).
        CONTINUE.
      ENDIF.

      IF exception_description->is_interface( ) = abap_true.
        result_line-exception_class_type = class_type-interface.
      ELSE.
        result_line-exception_class_type = class_type-class.
      ENDIF.
      result_line-exception_class_name = <raw_customizing_entry>-exception_class.
      result_line-mapper_class_name    = <raw_customizing_entry>-mapper_class.

      INSERT result_line INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
