CLASS lcl_plugin_interface IMPLEMENTATION.
  METHOD constructor.
    DATA exception TYPE REF TO cx_root.

    TRY.
        interface ?= cl_oo_object=>get_instance( i_interface_name ).
      CATCH cx_class_not_existent
            cx_sy_move_cast_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING previous = exception
                    textid   = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDTRY.
  ENDMETHOD.

  METHOD get_implementing_classes.
    DATA oo_class TYPE REF TO cl_oo_class.

    LOOP AT interface->get_implementing_classes( ) ASSIGNING FIELD-SYMBOL(<implementing_class>).
      TRY.
          oo_class = CAST #( cl_oo_class=>get_instance( <implementing_class>-clsname ) ).
        CATCH cx_class_not_existent
              cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.

      IF oo_class->is_abstract( ) = abap_false.
        INSERT VALUE #( classname   = oo_class->class-clsname
                        description = oo_class->class-descript ) INTO TABLE r_result.
      ENDIF.

      LOOP AT oo_class->get_subclasses( ) ASSIGNING FIELD-SYMBOL(<subclass>).
        TRY.
            oo_class = CAST #( cl_oo_class=>get_instance( <subclass>-clsname ) ).
          CATCH cx_class_not_existent
                cx_sy_move_cast_error.
            CONTINUE.
        ENDTRY.

        IF oo_class->is_abstract( ) = abap_false.
          INSERT VALUE #( classname   = oo_class->class-clsname
                          description = oo_class->class-descript ) INTO TABLE r_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
