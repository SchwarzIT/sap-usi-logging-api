INTERFACE /usi/if_bal_data_container
  PUBLIC .


  INTERFACES /usi/if_bal_message_details.

  CLASS-METHODS deserialize
    IMPORTING
      !i_serialized_data_container TYPE /usi/bal_xml_string
    RETURNING
      VALUE(r_result)              TYPE REF TO /usi/if_bal_data_container
    RAISING
      /usi/cx_bal_root .
  CLASS-METHODS is_multiple_use_allowed
    RETURNING
      VALUE(r_result) TYPE abap_bool .
  CLASS-METHODS get_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_classname .
  METHODS get_description
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_description .
  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_xml_string
    RAISING
      /usi/cx_bal_root.
ENDINTERFACE.
