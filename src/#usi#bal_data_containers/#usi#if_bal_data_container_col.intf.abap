INTERFACE /usi/if_bal_data_container_col PUBLIC.

  INTERFACES /usi/if_bal_message_details.

  "! <h1>Re-Create Data-Container-Collection from XML-String</h1>
  "!
  "! <p>Users of the API will not need this method.
  "! It is used internally when displaying data containers of logs in SLG1.</p>
  "!
  "! @parameter i_serialized_data_cont_coll | Data container collection as XML-String
  "! @parameter r_result | The re-created data container collection
  "! @raising /usi/cx_bal_root | Invalid input
  CLASS-METHODS deserialize
    IMPORTING
      i_serialized_data_cont_coll TYPE /usi/bal_xml_string
    RETURNING
      VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container_col
    RAISING
      /usi/cx_bal_root.

  "! <h1>Contains data containers?</h1>
  "!
  "! @parameter r_result | abap_true, if the collection contains at least one data container
  METHODS has_data_containers
    RETURNING
      VALUE(r_result) TYPE abap_bool.

  "! <h1>Insert container into collection</h1>
  "!
  "! <p><strong>CAUTION:</strong> If the new data container violates the cardinality, the new instance will be ignored.
  "! See /usi/if_bal_data_container~is_multiple_use_allowed for details.</p>
  "!
  "! @parameter i_data_container | to-be-inserted data container
  "! @parameter r_result | This data container collection (to enable chained method calls)
  METHODS insert
    IMPORTING
      i_data_container TYPE REF TO /usi/if_bal_data_container
    RETURNING
      VALUE(r_result)  TYPE REF TO /usi/if_bal_data_container_col.

  "! <h1>Get data containers of this collection</h1>
  "!
  "! @parameter r_result | Table of data container references
  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_containers.

  "! <h1>Convert Data-Container-Collection into XML-String</h1>
  "!
  "! <p>Users of the API will not need this method.
  "! It is used internally when saving a data container collection.</p>
  "!
  "! @parameter r_result | Data-Container-Collection-data as XML-String
  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_xml_string.

ENDINTERFACE.
