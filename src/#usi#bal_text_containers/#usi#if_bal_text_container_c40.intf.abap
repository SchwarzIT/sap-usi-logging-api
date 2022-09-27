INTERFACE /usi/if_bal_text_container_c40 PUBLIC.
  TYPES ty_text TYPE c LENGTH 40.

  "! <h1>Re-Create Text-Container from XML-String</h1>
  "!
  "! @parameter i_serialized_text_container | Container data as XML-String
  "! @parameter r_result | The re-created container
  "! @raising /usi/cx_bal_root | Invalid input
  CLASS-METHODS deserialize
    IMPORTING
      !i_serialized_text_container TYPE /usi/bal_xml_string
    RETURNING
      VALUE(r_result)              TYPE REF TO /usi/if_bal_text_container_c40
    RAISING
      /usi/cx_bal_root.

  "! <h1>Get class name of implementing class</h1>
  "!
  "! <p>Needed to serialize / deserialize the text containers.</p>
  "!
  "! <p>The deserialization of text containers is done by calling the static method deserialize of the respective text
  "! container class. In order to do that, the API will need the class name of the respective class.
  "! It will be saved along with the serialized text container data.</p>
  "!
  "! @parameter r_result | Class name of the implementing text container class
  CLASS-METHODS get_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_text_cont_classname.

  "! <h1>Get the text</h1>
  "!
  "! @parameter r_result | The text
  METHODS get_text
    RETURNING
      VALUE(r_result) TYPE ty_text.

  "! <h1>Convert Text-Container into XML-String</h1>
  "!
  "! <p>Users of the API will not need this method.
  "! It is used internally when saving text containers.</p>
  "!
  "! @parameter r_result | Container data as XML-String
  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_xml_string.

ENDINTERFACE.
