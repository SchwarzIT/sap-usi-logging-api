[![SIT](https://img.shields.io/badge/SIT-awesome-blueviolet.svg)](https://it.schwarz)
[![USI](https://img.shields.io/badge/SIT-USI-blue)](https://github.com/SchwarzIT/sap-usi)

# UNDER CONSTRUCTION
A wiki will soon be created. Until then this document will only cover the most vital topics.

# sap-usi-logging
This reusable component allows the creation of complete, comprehensive logs with almost no effort and extends SAP's standard logging API by powerful features.

Logging error situations instead of debugging them can:
* significantly reduce support efforts
* boost developer productivity
* speed up the processing of tickets
* aid in analyzing bugs that are hard to debug

The solution is:
* based on SAPs application log (Transaction SLG1)
* easy to use
* well documentend
* controlled by customizing (Log levels will control the detail level of the logs)
* completely object-oriented
* backwards compatible up to 7.00 and will work on virtually any SAP system

The solution enhances the capabilities of the SAP standard by so-called data containers, that can be used to attach virtually any type of data to log messages making them even more valuable. A variety of data containers for common use cases already exists, but new containers can easily be added whenever needed. The screenshot below shows how seamlessly they are integrated into the SAP standard. Messages with data containers have a detail button that opens a popup and the data can be accessed directly from SLG1.
![alt text](https://github.com/SchwarzIT/sap-usi-logging-api/blob/media/Screenshot_SLG1_Showcase_Data_Containers.png "Showcase Data Containers")

# Installation Guide
## Dependencies
The logging API is using the following components. Make sure, these components are installed in your system before importing the logging API.  
https://github.com/SchwarzIT/sap-usi-authority-check  
https://github.com/SchwarzIT/sap-usi-exception

## Integrate the deletion logic
The API can store so-called "data containers" for each logged message. Data containers can store virtually any kind of data, you might need to analyze an issue (e.g. the callstack, internal tables, etc.). Their data will be saved in the database table /USI/BAL_DATA. This data becomes obsolete when a log is deleted, so it must be deleted as well.

The API comes with a function module, that will get the job done, but you need to integrate this function into the SAP standard deletion logic for application logs.

The custom delete function must be called from the form DB_DELETE_CALLBACK of function group SBAL_DB. There are two possible ways to do this.

### Option #1: Enhancement(s)
You could create an implicit enhancement at the very beginning of the form and copy and paste the coding into that implicit enhancement. Depending on your release, the system might or might nor ask you, which master program should be enhanced. If it does, you need to create one enhancement for each master program.
```ABAP
*----------------------------------------------------------------------*
***INCLUDE LSBAL_DBF07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DB_DELETE_CALLBACK
*&---------------------------------------------------------------------*
FORM db_delete_callback
       USING
        i_t_logs_to_delete       TYPE balhdr_t
        i_in_update_task         TYPE boolean.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Form DB_DELETE_CALLBACK, Start
*$*$-Start: (1)---------------------------------------------------------------------------------$*$*
ENHANCEMENT 1  ZBAL_DELETE_OBSOLETE_DATA_01.    "active version
  IF i_in_update_task EQ abap_true.
    CALL FUNCTION '/USI/BAL_DELETE_CUSTOM_DATA' IN UPDATE TASK
      EXPORTING
        i_log_headers = i_t_logs_to_delete.
  ELSE.
    CALL FUNCTION '/USI/BAL_DELETE_CUSTOM_DATA'
      EXPORTING
        i_log_headers = i_t_logs_to_delete.
  ENDIF.
ENDENHANCEMENT.
*$*$-End:   (1)---------------------------------------------------------------------------------$*$*
 
  TYPES:
    BEGIN OF bal_s_object_lognumber,
```
The advantage of this method is, that you do not need a modification.

The disadvantage ist, that implicit enhancements are bound to exaclty one master program and the include might be used in more than one master program (Depends on your release). Failing to create one enhancement for every master program might result in obsolete additional data not being deleted.

### Option #2: Modification
You can add the same code at the same position using a small modification.

The advantage of this method is, that modifications are bound to the include itself and will therefore become effective in every master program that calls this form routine. This eliminates the risk of missing a master program but comes for the price of a modification.

### Desaster recovery
Forgetting to implement one of the two options before using the logging API might result in ridiculous amounts of obsolete data laying around in table /USI/BAL_DATA. As we are talking about binary data, that might as well contain data of internal tables, we might be talking about hundrets of gigabytes of data.

You can get rid of this obyolete data using the deletion report **/USI/BAL_DELETE_ORPHAN_LOG_DAT**. However, this is not the recommended solution, as deleting data containers using the enhancements or the modification is far more efficient.

Using the report is more like a last resort strategy to handle situations, that should never have occured in the first place.

## Schedule periodic jobs

| Report                        | Priority | Description |
| ----------------------------- | -------- | ----------- |
| SBAL_DELETE                   | **HIGH** | SAP standard deletion report for outdated logs. If you do not have a custom implementation in place, you will have to schedule this report on a regular basis. Failing to do so will result in ridiculous amounts of obsolete data piling up in your system. Issues like e.g. full table spaces will occur over time. Chances are very likely, that this job has already been scheduled. |
| /USI/BAL_DELETE_OUTDATED_CUST | Low      | The report deletes outdated customizing from table /usi/bal_lv_user (User specific log levels). As the customizing entries are created manually during an error analysis, the amount of data should be fairly small. Failing to schedule this report should have no negative impacts on the system. This is more of a convenience report, that will help you to keep your customizing clean and tidy by automatically removing entries, that are no longer needed anyway. |

# How to use it
## Customizing
1. Define log objects and sub objects for your application using transaction SLG0
2. Maintain table **/USI/BAL_LV_LOBJ** via SM30
    * Fields Object and Subobject are optional
    * Empty fields match everything
    * The most specific entry wins at runtime
    * The Object binds stronger, than the subobject

##  Create a new logger
```ABAP
DATA(logger) = /usi/cl_bal_factory=>get_instance( )->create_new_logger( i_log_object  = 'ZMY_REPORT'
                                                                        i_sub_object  = 'DELETE_USER'
                                                                        i_external_id = i_username ).
```

## Get existing logger
```ABAP
DATA(logger) = /usi/cl_bal_factory=>get_instance( )->get_existing_logger( ).
```

## Claim ownership
A log writer is a singleton, that will be stored by the factory. Once created you can access that logger instance literally everywhere in your codebase by just calling the factory.

This is pretty convenient but it also allows e.g. a flawed BAdI implementation to access your logger. Some random flawed BAdI should not be able to accidentally destroy your logger.

In order to avoid such "accidents", the execution of critical actions had to be restricted.

In order to do so, a concept of logical ownership was needed: One log has exactly one owner that has to prove the ownership when executing critical actions.
```ABAP
DATA(token) = logger->claim_ownership( ).
```

|                  | Behavior | Comments |
| ---------------- | -------------------------------------------- | --- |
| First call       | Returns the actual token that will make the caller the owner of the log writer  | Whoever calls the method for the very first time will become the owner of that log writer. You need to store that token, as you will need it to save and destoy the log later. |
| Subsequent calls | Returns a fake token, that is good for nothing | Critical actions like saving or destroying a logger can not be executed with this object, as it is no legit proof of ownership. There is no possibility to distinguish between the real token and a dummy. This might seem odd at first, but the reason is fairly simle: Just imagine, you integrate the API into e.g. a BAdI, that will run in various applications. Some of them might use this API as well while others might not. If the main application uses this API, then your log messages must become a part of the main applications log. This should work automatically - you should not even have to think about this. That's why the developer can't even recognize, if he became the owner of the log or not. It kind of forces him to stick to his own business. Also returning null is a horrible practice ;-) |

## Add messages
The log writer offers three methods, that will append a message to the log:
* add_exception
* add_free_text
* add_message

### Simple calls
The following examples show the simplest possible call to the API.
```ABAP
logger->add_exception( exception ).

logger->add_free_text( `Some free text` ).

logger->add_message( i_message_class      = '38'
                     i_message_number     = '000'
                     i_message_variable_1 = 'A short test message' ).
```

### Complex call
The following call uses all parameters of method add_exception and shows, what the API is capable of.
```ABAP
logger->add_exception( i_problem_class   = /usi/cl_bal_enum_problem_class=>other
                       i_detail_level    = /usi/cl_bal_enum_detail_level=>detail_level_1
                       i_message_type    = /usi/cl_bal_enum_message_type=>error
                       i_exception       = exception
                       i_log_previous    = abap_true
                       i_details         = NEW /usi/cl_bal_dc_collection( )->insert(
                                             NEW /usi/cl_bal_dc_itab(
                                               i_internal_table = table
                                               i_title          = /usi/cl_bal_tc_report_text_c40=>create(
                                                                    i_text_key = 'DYN'
                                                                    i_text     = text-dyn
                                                                  )
                                               i_fieldcatalog   = field_catalog
                                             )
                                           )->insert(
                                             NEW /usi/cl_bal_dc_html(
                                               i_html_document  = `<html><head/><body><p>Test</p></body><//html>`
                                               i_document_title = new /usi/cl_bal_tc_literal_c40( `Document title` )
                                             )
                                           )
                       i_message_context = VALUE #( tabname = 'T000'
                                                    value   = VALUE t000( mandt = 123  ) ) ).
```

### Important parameters

| Parameter       | Description | Recommendation | 
| --------------- | --- | --- |
| I_PROBLEM_CLASS | The parameter I_PROBLEM_CLASS defines the severity of a log message and strongly correlates with the log level. It acutally decides, whether a message will be logged at a certain log level or not. | The more difficult an error is to reproduce, the higher the priority should be. You could use e.g. VERY_IMPORTANT for RFC-errors (as they might be temporary due to a system reboot) and OTHER for data inconsistencies that can be reporduced anytime. |
| I_LOG_PREVIOUS  | Specifically for exceptions. If set, the log writer will log all previous exceptions of an exception. | Use ABAP_TRUE for exceptions from foreign code and ABAP_FALSE for your own exceptions. Log your own exceptions where you raise them (Use a `CLEANUP` block to log the exception you just raised).|
| I_DETAILS       | Can be used to pass either a single data container or a collection of data containers along with the message. | Use, if needed - just do, what you have to. |

## Save the log
The add_*-methods of the log writer will add messages to the buffer.  
Calling the save-method will actually write them to the database.
```ABAP
logger->save( token ).
```
As it should be the decision of the log owner, if and how often the log will be saved, the token is needed to execute that method.

## Destroy the log
The rule of thumb is: If you open a log, you must destroy it, once you are done with it.

This is especially important, if you are logging in e.g. BAdIs, that are called by multiple applications. If the BAdI is called, before the main application gets the chance to open a log and does not destroy the logger, once it is done, the singleton will remain bound, which forces that other application under the BAdIs log. Additionally, the other application would not be able to save the log, as they aren't the owner of the log!

**Failing to destroy a logger once you don't need it anymore might break other peoples logs!**
```ABAP
logger->free( token ).
```
