INTERFACE /usi/if_bal_data_cont_coll_dao PUBLIC.

  "! <h1>Insert data container collection into buffer</h1>
  "!
  "! @parameter i_log_number | Log number
  "! @parameter i_message_number | Message number
  "! @parameter i_serialized_data_cont_coll | Data container collection as XML-String
  "! @raising /usi/cx_bal_root | Error (invalid input)
  METHODS insert_collection_into_buffer
    IMPORTING
      i_log_number                TYPE balognr
      i_message_number            TYPE /usi/bal_message_number
      i_serialized_data_cont_coll TYPE /usi/bal_xml_string
    RAISING
      /usi/cx_bal_root.

  "! <h1>Write buffer to DB</h1>
  "!
  "! @raising /usi/cx_bal_root | Error
  METHODS save_buffer_to_db
    RAISING
      /usi/cx_bal_root.

  "! <h1>Read data container collection from DB</h1>
  "!
  "! @parameter i_log_number | Log number
  "! @parameter i_message_number | Message number
  "! @parameter r_result | Data container collection as XML-String
  "! @raising /usi/cx_bal_root | Error
  METHODS get_collection
    IMPORTING
      i_log_number     TYPE balognr
      i_message_number TYPE /usi/bal_message_number
    RETURNING
      VALUE(r_result)  TYPE /usi/bal_xml_string
    RAISING
      /usi/cx_bal_root.

  "! <h1>Delete data container collections from DB</h1>
  "!
  "! @parameter i_log_numbers | Log numbers
  METHODS delete_collections
    IMPORTING
      i_log_numbers TYPE bal_t_logn.

ENDINTERFACE.
