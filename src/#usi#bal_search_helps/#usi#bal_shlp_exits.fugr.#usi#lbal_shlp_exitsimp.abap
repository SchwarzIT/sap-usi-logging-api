*&---------------------------------------------------------------------*
*&  Include           /USI/LBAL_SHLP_EXITSIMP
*&---------------------------------------------------------------------*
CLASS lcl_plugin_interface IMPLEMENTATION.
  METHOD constructor.
    DATA exception TYPE REF TO cx_root.

    TRY.
        interface ?= cl_oo_object=>get_instance( i_interface_name ).
      CATCH cx_class_not_existent
            cx_sy_move_cast_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING
            previous = exception
            textid   = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDTRY.
  ENDMETHOD.

  METHOD get_implementing_classes.
    DATA: implementing_classes TYPE seo_relkeys,
          subclasses           TYPE seo_relkeys,
          oo_object            TYPE REF TO cl_oo_object,
          oo_class             TYPE REF TO cl_oo_class,
          result_line          TYPE class_name_and_description.

    FIELD-SYMBOLS: <implementing_class> TYPE seorelkey,
                   <subclass>           TYPE seorelkey.

    implementing_classes = interface->get_implementing_classes( ).
    LOOP AT implementing_classes ASSIGNING <implementing_class>.
      TRY.
          oo_object = cl_oo_class=>get_instance( <implementing_class>-clsname ).
          oo_class ?= oo_object.
        CATCH cx_class_not_existent
              cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.

      IF oo_class->is_abstract( ) EQ abap_false.
        result_line-classname   = oo_class->class-clsname.
        result_line-description = oo_class->class-descript.
        INSERT result_line INTO TABLE r_result.
      ENDIF.

      subclasses = oo_class->get_subclasses( ).
      LOOP AT subclasses ASSIGNING <subclass>.
        TRY.
            oo_object = cl_oo_class=>get_instance( <subclass>-clsname ).
            oo_class ?= oo_object.
          CATCH cx_class_not_existent
                cx_sy_move_cast_error.
            CONTINUE.
        ENDTRY.

        IF oo_class->is_abstract( ) EQ abap_false.
          result_line-classname   = oo_class->class-clsname.
          result_line-description = oo_class->class-descript.
          INSERT result_line INTO TABLE r_result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
