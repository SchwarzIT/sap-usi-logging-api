INTERFACE /usi/if_bal_data_container_col
  PUBLIC .


  INTERFACES /usi/if_bal_message_details.

  CLASS-METHODS deserialize
    IMPORTING
      !i_serialized_data_cont_coll TYPE /usi/bal_xml_string
    RETURNING
      VALUE(r_result)              TYPE REF TO /usi/if_bal_data_container_col
    RAISING
      /usi/cx_bal_root .
  METHODS has_data_containers
    RETURNING
      VALUE(r_result) TYPE abap_bool .
  METHODS insert
    IMPORTING
      !i_data_container TYPE REF TO /usi/if_bal_data_container
    RETURNING
      VALUE(r_result)   TYPE REF TO /usi/if_bal_data_container_col .
  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_containers .
  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_xml_string .
ENDINTERFACE.
