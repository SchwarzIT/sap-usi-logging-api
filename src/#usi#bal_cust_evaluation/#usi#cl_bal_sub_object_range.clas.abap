CLASS /usi/cl_bal_sub_object_range DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: range TYPE /usi/bal_sub_object_range READ-ONLY.

    METHODS insert_line
      IMPORTING
        i_sign   TYPE ddsign DEFAULT 'I'
        i_option TYPE ddoption DEFAULT 'EQ'
        i_low    TYPE balobj_d
        i_high   TYPE balobj_d OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /usi/cl_bal_sub_object_range IMPLEMENTATION.
  METHOD insert_line.
    DATA range_line TYPE /usi/bal_sub_object_range_line.

    range_line-sign   = i_sign.
    range_line-option = i_option.
    range_line-low    = i_low.
    range_line-high   = i_high.

    INSERT range_line INTO TABLE range.
  ENDMETHOD.
ENDCLASS.
