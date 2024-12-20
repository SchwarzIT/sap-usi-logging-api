CLASS lcl_plugin_interface DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_class,
             classname   TYPE seoclsname,
             description TYPE seodescr,
           END   OF ty_class,
           ty_classes TYPE STANDARD TABLE OF ty_class WITH EMPTY KEY.

    METHODS constructor
      IMPORTING i_interface_name TYPE seoclsname
      RAISING   /usi/cx_bal_root.

    METHODS get_implementing_classes
      RETURNING VALUE(r_result) TYPE ty_classes.

  PRIVATE SECTION.
    DATA interface TYPE REF TO cl_oo_interface.

ENDCLASS.
