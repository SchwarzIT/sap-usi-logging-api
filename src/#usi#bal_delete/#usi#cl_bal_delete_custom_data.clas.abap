CLASS /usi/cl_bal_delete_custom_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_bal_delete_custom_data .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /usi/cl_bal_delete_custom_data IMPLEMENTATION.
  METHOD /usi/if_bal_delete_custom_data~delete_custom_data.
    DATA log_numbers TYPE bal_t_logn.

    LOOP AT i_log_headers ASSIGNING FIELD-SYMBOL(<log_header>).
      INSERT <log_header>-lognumber INTO TABLE log_numbers.
    ENDLOOP.

    DATA(dao_factory) = /usi/cl_bal_logger_dao_factory=>get_instance( ).
    DATA(dao_object)  = dao_factory->get_data_container_collection( ).
    dao_object->delete_collections( log_numbers ).
  ENDMETHOD.
ENDCLASS.
