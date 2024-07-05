CLASS /usi/cl_bal_log_object_range DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA range TYPE /usi/bal_log_object_range READ-ONLY.

    "! Helper-Method to build ranges
    "!
    "! @parameter i_sign | Sign
    "! @parameter i_option | Option
    "! @parameter i_low | Low
    "! @parameter i_high | High
    METHODS insert_line
      IMPORTING
        i_sign   TYPE ddsign DEFAULT 'I'
        i_option TYPE ddoption DEFAULT 'EQ'
        i_low    TYPE balobj_d
        i_high   TYPE balobj_d OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /usi/cl_bal_log_object_range IMPLEMENTATION.
  METHOD insert_line.
    INSERT VALUE #( sign   = i_sign
                    option = i_option
                    low    = i_low
                    high   = i_high ) INTO TABLE range.
  ENDMETHOD.
ENDCLASS.
