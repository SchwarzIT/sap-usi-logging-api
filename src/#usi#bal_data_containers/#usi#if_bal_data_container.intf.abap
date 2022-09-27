INTERFACE /usi/if_bal_data_container PUBLIC.

  INTERFACES /usi/if_bal_message_details.
  INTERFACES /usi/if_exception_details.

  "! <h1>Deserialize Data-Container (String -> ABAP)</h1>
  "!
  "! <p>Users of the API will not need this method.
  "! It is used internally when displaying data containers of logs in SLG1.</p>
  "!
  "! @parameter i_serialized_data_container | Container data as XML-String
  "! @parameter r_result | The re-created container
  "! @raising /usi/cx_bal_root | Invalid input
  CLASS-METHODS deserialize
    IMPORTING
      i_serialized_data_container TYPE /usi/bal_serialized_data
    RETURNING
      VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container
    RAISING
      /usi/cx_bal_root.

  "! <h1>Multiple use allowed?</h1>
  "!
  "! <p>Currently data containers can have two different cardinalities:
  "!   <ul>
  "!     <li>0-1 (Single-Usage-Container)
  "!       <ul>
  "!         <li>Example: Caller-Source-Code-Position</li>
  "!         <li>More than one instance makes no sense and is therefore forbidden</li>
  "!       </ul>
  "!     </li>
  "!     <li>0-n (Multi-Usage-Container)
  "!       <ul>
  "!         <li>Example: ITAB-Container</li>
  "!         <li>Multiple instances are perfectly fine</li>
  "!       </ul>
  "!     </li>
  "!   </ul>
  "! </p>
  "!
  "! <p>The API will call this method internally while collecting the data containers for a log-message.</p>
  "!
  "! <p>Containers that would violate the cardinality restriction since another instance of the single-usage-container
  "! is already present, will be ignored.</p>
  "!
  "! @parameter r_result | Flag: More than one instance allowed?
  CLASS-METHODS is_multiple_use_allowed
    RETURNING
      VALUE(r_result) TYPE abap_bool.

  "! <h1>Get class name of implementing class</h1>
  "!
  "! <p>Needed to serialize / deserialize the data containers.</p>
  "!
  "! <p>The deserialization of data containers is done by calling the static method deserialize of the respective data
  "! container class. In order to do that, the API will need the class name of the respective data container class.
  "! It will be saved along with the serialized data container data.</p>
  "!
  "! @parameter r_result | Class name of the implementing data container class
  CLASS-METHODS get_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_classname.

  "! <h1>Get Container Content Description (UI-Text)</h1>
  "!
  "! <p>When displaying data container data in SLG1, a title will be needed for each data container.
  "! This method provides that title.</p>
  "!
  "! <p><strong>HINT:</strong> When implementing multi-usage-containers, you might want to add a text container
  "! (Interface /usi/if_bal_text_container_c40) to the data container. It would allow developers to pass an
  "! instance-specific description of the containers content (e.g. "SFLIGHT-Data").</p>
  "!
  "! <p>Since the description has 60 characters, you could use up to 20 characters for a class-specific prefix
  "! (e.g. "Internal Table: ").</p>
  "!
  "! <p>Combining them in method get_description would create the text "Internal Table: SFLIGHT-Data".</p>
  "!
  "! <p>This would make it much easier for users to tell different instances of a data container class apart from each
  "! other.</p>
  "!
  "! @parameter r_result | UI-Text describing the content of the container
  METHODS get_description
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_description.

  "! <h1>Serialize Data-Container (ABAP -> String)</h1>
  "!
  "! <p>Users of the API will not need this method.
  "! It is used internally when saving data containers.</p>
  "!
  "! @parameter r_result | Container data as XML-String
  "! @raising /usi/cx_bal_root | Error during serialization; Container will be skipped
  METHODS serialize
    RETURNING
      VALUE(r_result) TYPE /usi/bal_serialized_data
    RAISING
      /usi/cx_bal_root.

ENDINTERFACE.
