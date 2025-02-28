CLASS /usi/cl_bal_ce_data_containers DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_ce_data_containers.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_customizing_dao | DAO-Object
    METHODS constructor
      IMPORTING
        i_customizing_dao TYPE REF TO /usi/if_bal_cd_data_containers.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_data_containers.

ENDCLASS.



CLASS /usi/cl_bal_ce_data_containers IMPLEMENTATION.
  METHOD /usi/if_bal_ce_data_containers~get_relevant_plugin_classnames.
    CONSTANTS data_container_interface TYPE seoclsname VALUE '/USI/IF_BAL_DATA_CONTAINER'.

    DATA: required_log_level TYPE REF TO /usi/cl_bal_enum_log_level,
          object_description TYPE REF TO /usi/cl_bal_object_descr.

    " Read database
    TRY.
        DATA(customizing_records) = customizing_dao->get_records( i_log_object_range = VALUE #( sign   = 'I'
                                                                                                option = 'EQ'
                                                                                                ( low = i_log_object )
                                                                                                ( low = space ) )
                                                                  i_sub_object_range = VALUE #( sign   = 'I'
                                                                                                option = 'EQ'
                                                                                                ( low = i_sub_object )
                                                                                                ( low = space ) ) ).
      CATCH /usi/cx_bal_root.
        CLEAR customizing_records.
    ENDTRY.

    " Delete rules of lower priority (Priority: log_object > sub_object)
    SORT customizing_records
         BY classname  ASCENDING
            log_object DESCENDING
            sub_object DESCENDING.

    DELETE ADJACENT DUPLICATES
           FROM customizing_records
           COMPARING classname.

    " Process rules of highest priority
    LOOP AT customizing_records ASSIGNING FIELD-SYMBOL(<customizing_record>).
      TRY.
          required_log_level = /usi/cl_bal_enum_log_level=>get_by_value( <customizing_record>-min_log_level ).

          object_description = NEW #( i_object_type_name = <customizing_record>-classname ).
        CATCH /usi/cx_bal_root.
          CONTINUE.
      ENDTRY.

      IF    required_log_level->is_higher_than( i_log_level )               = abap_true
         OR object_description->is_instantiatable( ) = abap_false
         OR object_description->is_implementing( data_container_interface ) = abap_false.
        CONTINUE.
      ENDIF.

      INSERT <customizing_record>-classname INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    customizing_dao = i_customizing_dao.
  ENDMETHOD.
ENDCLASS.
