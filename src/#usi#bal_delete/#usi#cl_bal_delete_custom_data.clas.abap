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
    DATA: dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory,
          dao_object  TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          log_numbers TYPE bal_t_logn.

    FIELD-SYMBOLS <log_header> TYPE balhdr.

    LOOP AT i_log_headers ASSIGNING <log_header>.
      INSERT <log_header>-lognumber INTO TABLE log_numbers.
    ENDLOOP.

    dao_factory = /usi/cl_bal_logger_dao_factory=>get_instance( ).
    dao_object  = dao_factory->get_data_container_collection( ).
    dao_object->delete_collections( log_numbers ).
  ENDMETHOD.
ENDCLASS.
