CLASS lhc_Registration DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Registration RESULT result.

    METHODS approve_registration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~approve_registration RESULT result.

    METHODS reject_registration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~reject_registration RESULT result.

    METHODS assign_registration_id FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Registration~assign_registration_id.

    METHODS set_default_status FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Registration~set_default_status.

    METHODS validate_event FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~validate_event.

    METHODS validate_participant FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~validate_participant.

    METHODS check_max_participants FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~check_max_participants.

ENDCLASS.


CLASS lhc_Registration IMPLEMENTATION.


  METHOD get_instance_authorizations.
  ENDMETHOD.


  "==============================================================
  " ACTION: APPROVE REGISTRATION
  "==============================================================
  METHOD approve_registration.

    READ ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      FIELDS ( RegistrationUuid Status )
      WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs ASSIGNING FIELD-SYMBOL(<reg>).

      IF <reg>-Status = 'Approved'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '004'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      IF <reg>-Status = 'Rejected'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '005'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      MODIFY ENTITIES OF ZR_Registration_G3
        ENTITY Registration
        UPDATE FROM VALUE #(
          (
            %tky            = <reg>-%tky
            Status          = 'Approved'
            %control-Status = if_abap_behv=>mk-on
          )
        )
        REPORTED DATA(update_report).

      APPEND VALUE #(
        %tky = <reg>-%tky
        %msg = new_message(
                id       = 'ZMSG_REG'
                number   = '006'
                severity = if_abap_behv_message=>severity-success )
      ) TO reported-registration.

      INSERT VALUE #( %tky = <reg>-%tky ) INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.



  "==============================================================
  " ACTION: REJECT REGISTRATION
  "==============================================================
  METHOD reject_registration.

    READ ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      FIELDS ( RegistrationUuid Status )
      WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs ASSIGNING FIELD-SYMBOL(<reg>).

      IF <reg>-Status = 'Approved'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '004'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      IF <reg>-Status = 'Rejected'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '005'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      MODIFY ENTITIES OF ZR_Registration_G3
        ENTITY Registration
        UPDATE FROM VALUE #(
          (
            %tky            = <reg>-%tky
            Status          = 'Rejected'
            %control-Status = if_abap_behv=>mk-on
          )
        )
        REPORTED DATA(update_report).

      APPEND VALUE #(
        %tky = <reg>-%tky
        %msg = new_message(
                id       = 'ZMSG_REG'
                number   = '007'
                severity = if_abap_behv_message=>severity-success )
      ) TO reported-registration.

      INSERT VALUE #( %tky = <reg>-%tky ) INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.



  "==============================================================
  " DETERMINE DEFAULT STATUS = 'New'
  "==============================================================
  METHOD set_default_status.

    MODIFY ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      UPDATE FROM VALUE #(
        FOR key IN keys
         (
           %tky             = key-%tky
           Status           = 'New'
           %control-Status  = if_abap_behv=>mk-on
         )
      ).

  ENDMETHOD.



  "==============================================================
  " DETERMINE: ASSIGN REGISTRATION ID
  "==============================================================
  METHOD assign_registration_id.

    SELECT MAX( registration_id )
      FROM zregistrationa
      INTO @DATA(lv_max).

    IF lv_max IS INITIAL.
      lv_max = '00000'.
    ENDIF.

    DATA counter TYPE n LENGTH 5.
    counter = lv_max.

    DATA lt_update TYPE TABLE FOR UPDATE ZR_Registration_G3.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      counter = counter + 1.

      DATA new_id TYPE n LENGTH 5.
      new_id = counter.

      APPEND VALUE #(
        %tky                     = <key>-%tky
        RegistrationId           = new_id
        %control-RegistrationId  = if_abap_behv=>mk-on
      ) TO lt_update.

    ENDLOOP.

    MODIFY ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      UPDATE FROM lt_update.

  ENDMETHOD.



  "==============================================================
  " VALIDATION: EVENT MUST EXIST
  "==============================================================
  METHOD validate_event.

    READ ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      FIELDS ( EventUuid RegistrationUuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs ASSIGNING FIELD-SYMBOL(<reg>).

      SELECT SINGLE event_uuid
        FROM zeventa
        WHERE event_uuid = @<reg>-EventUuid
        INTO @DATA(lv_exists).

      IF sy-subrc <> 0.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '003'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.



  "==============================================================
  " VALIDATION: PARTICIPANT MUST EXIST
  "==============================================================
  METHOD validate_participant.

    READ ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      FIELDS ( ParticipantUuid RegistrationUuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs ASSIGNING FIELD-SYMBOL(<reg>).

      SELECT SINGLE participant_uuid
        FROM zparticipanta
        WHERE participant_uuid = @<reg>-ParticipantUuid
        INTO @DATA(lv_exists).

      IF sy-subrc <> 0.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '002'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.



  "==============================================================
  " VALIDATION: CHECK MAX PARTICIPANTS
  "==============================================================
  METHOD check_max_participants.

    READ ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      FIELDS ( EventUuid RegistrationUuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs ASSIGNING FIELD-SYMBOL(<reg>).

      SELECT SINGLE max_participants
        FROM zeventa
        WHERE event_uuid = @<reg>-EventUuid
        INTO @DATA(maxp).

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      SELECT COUNT(*)
        FROM zregistrationa
        WHERE event_uuid = @<reg>-EventUuid
        INTO @DATA(count).

      IF count >= maxp.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '001'
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
