INTERFACE /usi/if_bal_exception_mapper PUBLIC.

  "! <h1>Convert exception text into T100-Message</h1>
  "!
  "! <p>This method exists only because it cannot be deleted due to the open-closed principle.</p>
  "!
  "! <p>Since converting exceptions to different formats was a repetitive task, the logic was moved to a central
  "! implementation in the /USI/EXCEPTION component. However, deleting the method would destroy the custom exception
  "! mappers written by users of the API. Since there is no way to find out if custom implementations exist, this
  "! action is clearly off limits.</p>
  "!
  "! <p>If you ever need to write a custom exception mapper, just copy the implementation for this method from
  "! /usi/cl_bal_em_base.</p>
  "!
  "! <p>If "your" exception is not converted corretly, please enhance component /USI/EXCEPTION instead, since this
  "! will fix both APIs at once.<p/>
  "!
  "! @parameter r_result | Exception text as T100-Message
  METHODS get_t100_message
    RETURNING
      VALUE(r_result) TYPE symsg.

  "! <h1>Get data containers from exception</h1>
  "!
  "! <p>This method might only be needed, if you have an exception, that has some sort of additional data attached to
  "! it, that is <strong>NOT</strong> a data container class already. You could use a custom exception mapper to grab
  "! that data, convert it into a data container and insert it into the data container collection.</p>
  "!
  "! <p>However this is <strong>NOT</strong> the recommended procedure!</p>
  "!
  "! <p>Writing a custom exception mapper is the last resort approach to add data from foreign exceptions
  "! (SAP standard / 3rd party API) to your log message.</p>
  "!
  "! <p>If you have a say on what the exception looks like, please consider adding an instance attribute of
  "! <strong>TYPE REF TO /usi/if_bal_message_details</strong> to the exception instead since the API handles these
  "! automatically. That way you would only have to implement a data container class and attach that to the exception
  "! instead. Less effort, same result.</p>
  "!
  "! @parameter i_target_data_cont_coll | Data container collection of the log message (provided by API)
  METHODS get_data_containers
    IMPORTING
      i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

ENDINTERFACE.
