INTERFACE /usi/if_bal_text_container_c40
  PUBLIC .
  TYPES: ty_text TYPE c LENGTH 40.

  CLASS-METHODS deserialize
    IMPORTING
      !i_serialized_text_container TYPE /usi/bal_xml_string
    RETURNING
      VALUE(r_result)              TYPE REF TO /usi/if_bal_text_container_c40
    RAISING
      /usi/cx_bal_root .

  CLASS-METHODS get_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_text_cont_classname.

  METHODS get_text
    RETURNING
      VALUE(r_result) TYPE ty_text.

  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_xml_string.

ENDINTERFACE.
