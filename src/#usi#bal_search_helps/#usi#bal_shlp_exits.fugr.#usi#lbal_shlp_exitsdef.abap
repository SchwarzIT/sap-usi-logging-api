*&---------------------------------------------------------------------*
*&  Include           /USI/LBAL_SHLP_EXITSDEF
*&---------------------------------------------------------------------*
CLASS lcl_plugin_interface DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF class_name_and_description,
        classname   TYPE seoclsname,
        description TYPE seodescr,
      END   OF class_name_and_description,
      class_name_and_description_tab TYPE STANDARD TABLE OF class_name_and_description WITH NON-UNIQUE DEFAULT KEY.

    METHODS constructor
      IMPORTING
        i_interface_name TYPE seoclsname
      RAISING
        /usi/cx_bal_root.

    METHODS get_implementing_classes
      RETURNING
        VALUE(r_result) TYPE class_name_and_description_tab.

  PRIVATE SECTION.

    DATA interface TYPE REF TO cl_oo_interface.

ENDCLASS.
