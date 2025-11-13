CLASS lhc_Event DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Event RESULT result.

    METHODS close_event FOR MODIFY
      IMPORTING keys FOR ACTION Event~close_event RESULT result.

    METHODS open_event FOR MODIFY
      IMPORTING keys FOR ACTION Event~open_event RESULT result.

    METHODS assign_event_id FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Event~assign_event_id.

    METHODS set_default_status FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Event~set_default_status.

    METHODS check_end_date FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~check_end_date.

    METHODS check_start_date FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~check_start_date.

ENDCLASS.

CLASS lhc_Event IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD close_event.
  ENDMETHOD.

  METHOD open_event.
  ENDMETHOD.

  METHOD assign_event_id.
  ENDMETHOD.

  METHOD set_default_status.
  ENDMETHOD.

  METHOD check_end_date.
  ENDMETHOD.

  METHOD check_start_date.
  ENDMETHOD.

ENDCLASS.
