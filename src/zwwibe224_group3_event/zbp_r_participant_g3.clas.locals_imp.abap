CLASS lhc_Participant DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Participant RESULT result.

    METHODS assign_participant_id FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Participant~assign_participant_id.

ENDCLASS.

CLASS lhc_Participant IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD assign_participant_id.
  ENDMETHOD.

ENDCLASS.
