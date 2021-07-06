INTERFACE /usi/if_bal_language_priority
  PUBLIC .

  TYPES: ty_language_priority TYPE n LENGTH 2,
         BEGIN OF ty_language,
           priority TYPE ty_language_priority,
           language TYPE sylangu,
         END   OF ty_language,
         ty_languages TYPE SORTED TABLE OF ty_language WITH UNIQUE KEY priority.

  METHODS get_languages
    RETURNING
      VALUE(r_result) TYPE ty_languages.

ENDINTERFACE.
