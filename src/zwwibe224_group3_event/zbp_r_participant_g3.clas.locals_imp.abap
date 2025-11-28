CLASS lhc_Participant DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS assign_participant_id
      FOR MODIFY
      IMPORTING keys FOR ACTION Participant~assign_participant_id.

    METHODS validate_email
      FOR VALIDATE ON SAVE
      IMPORTING keys FOR Participant~validate_email.

ENDCLASS.



CLASS lhc_Participant IMPLEMENTATION.


  "==============================================================
  " ACTION: ASSIGN PARTICIPANT ID (works even if field is readonly)
  "==============================================================
  METHOD assign_participant_id.

    SELECT MAX( participant_id )
      FROM zparticipanta
      INTO @DATA(lv_max).

    IF lv_max IS INITIAL.
      lv_max = '00000'.
    ENDIF.

    DATA counter TYPE n LENGTH 5.
    counter = lv_max.

    DATA lt_update TYPE TABLE FOR UPDATE ZR_Participant_G3.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      counter = counter + 1.

      DATA new_id TYPE n LENGTH 5.
      new_id = counter.

      APPEND VALUE #(
        %tky                     = <key>-%tky
        ParticipantId            = new_id
        %control-ParticipantId   = if_abap_behv=>mk-on   "← wichtig, erlaubt trotz readonly
      ) TO lt_update.

    ENDLOOP.

    MODIFY ENTITIES OF ZR_Participant_G3
      ENTITY Participant
      UPDATE FROM lt_update.

  ENDMETHOD.



  "==============================================================
  " VALIDATION: EMAIL FORMAT CHECK
  "==============================================================
  METHOD validate_email.

    READ ENTITIES OF ZR_Participant_G3
      ENTITY Participant
      FIELDS ( Email ParticipantUuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(parts).

    LOOP AT parts ASSIGNING FIELD-SYMBOL(<p>).

      IF <p>-Email IS INITIAL.
        APPEND VALUE #(
          %tky = <p>-%tky
          %msg = new_message(
                  id       = 'ZMSG_PART'
                  number   = '001'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-participant.
        CONTINUE.
      ENDIF.

      IF <p>-Email NP '*@*'.
        APPEND VALUE #(
          %tky = <p>-%tky
          %msg = new_message(
                  id       = 'ZMSG_PART'
                  number   = '002'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-participant.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
