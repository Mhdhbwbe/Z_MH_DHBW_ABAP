CLASS lhc_Participant DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS assign_participant_id FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Participant~assign_participant_id.

ENDCLASS.

CLASS lhc_Participant IMPLEMENTATION.

  METHOD assign_participant_id.
    DATA lv_max_id TYPE n LENGTH 5.

    " Höchste vorhandene ParticipantId finden
    SELECT SINGLE MAX( participant_id )
      FROM zparticipanta
      INTO @lv_max_id.

    IF sy-subrc <> 0 OR lv_max_id IS INITIAL.
      lv_max_id = 0.
    ENDIF.

    " Neue ParticipantIds vergeben
    READ ENTITIES OF ZR_Participant_G3 IN LOCAL MODE
      ENTITY Participant
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(participants).

    LOOP AT participants ASSIGNING FIELD-SYMBOL(<fs_participant>).
      lv_max_id = lv_max_id + 1.
      MODIFY ENTITIES OF ZR_Participant_G3
        IN LOCAL MODE
        ENTITY Participant
        UPDATE
        FIELDS ( ParticipantId )
        WITH VALUE #(
          ( %tky = <fs_participant>-%tky
            ParticipantId = lv_max_id )
        ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
