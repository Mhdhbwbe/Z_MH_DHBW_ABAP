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

    METHODS check_max_participants FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~check_max_participants.

    METHODS validate_event FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~validate_event.

    METHODS validate_participant FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~validate_participant.

ENDCLASS.

CLASS lhc_Registration IMPLEMENTATION.

  METHOD get_instance_authorizations.
    " Keine speziellen Berechtigungen notwendig
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

      " Already approved
      IF <reg>-Status = 'Approved'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '004'       " Already approved
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      " Already rejected
      IF <reg>-Status = 'Rejected'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '005'       " Already rejected
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      " Update status
      MODIFY ENTITIES OF ZR_Registration_G3
        ENTITY Registration
        UPDATE FIELDS ( Status )
        WITH VALUE #(
          ( RegistrationUuid = <reg>-RegistrationUuid
            Status           = 'Approved' ) )
        REPORTED DATA(update_report).

      " Success message
      APPEND VALUE #(
        %tky = <reg>-%tky
        %msg = new_message(
                id       = 'ZMSG_REG'
                number   = '006'     " Registration approved successfully
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

      " Already approved
      IF <reg>-Status = 'Approved'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '004'     " Already approved
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      " Already rejected
      IF <reg>-Status = 'Rejected'.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                  id       = 'ZMSG_REG'
                  number   = '005'     " Already rejected
                  severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
        CONTINUE.
      ENDIF.

      " Update status
      MODIFY ENTITIES OF ZR_Registration_G3
        ENTITY Registration
        UPDATE FIELDS ( Status )
        WITH VALUE #(
          ( RegistrationUuid = <reg>-RegistrationUuid
            Status           = 'Rejected' ) )
        REPORTED DATA(update_report).

      " Success message
      APPEND VALUE #(
        %tky = <reg>-%tky
        %msg = new_message(
                id       = 'ZMSG_REG'
                number   = '007'     " Registration rejected successfully
                severity = if_abap_behv_message=>severity-success )
      ) TO reported-registration.

      INSERT VALUE #( %tky = <reg>-%tky ) INTO TABLE result.

    ENDLOOP.

  ENDMETHOD.

  "==============================================================
  " DETERMINE: SET DEFAULT STATUS = 'New'
  "==============================================================
  METHOD set_default_status.
    MODIFY ENTITIES OF ZR_Registration_G3
      ENTITY Registration
      UPDATE FIELDS ( Status )
      WITH VALUE #(
        FOR key IN keys
        (
          RegistrationUuid        = key-RegistrationUuid
          Status                  = 'New'
          %control-Status         = if_abap_behv=>mk-on
        )
      ).
  ENDMETHOD.

  "==============================================================
  " DETERMINE: ASSIGN REGISTRATION ID (READONLY-FIELD!)
  "==============================================================
 METHOD assign_registration_id.

   SELECT MAX( registration_id )
     FROM zregistrationa
     INTO @DATA(lv_max_id_db).

   DATA(lv_max_int) = 0.
   IF sy-subrc = 0 AND lv_max_id_db IS NOT INITIAL.
     lv_max_int = lv_max_id_db.
   ENDIF.

   DATA lt_update TYPE TABLE FOR UPDATE ZR_Registration_G3\\Registration.

   LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

     lv_max_int += 1.

     DATA(lv_new_id) = |{ lv_max_int WIDTH = 5 ALIGN = RIGHT PAD = '0' }|.

     APPEND VALUE #(
       %tky            = <ls_key>-%tky
       RegistrationId  = lv_new_id
     ) TO lt_update.

   ENDLOOP.

   MODIFY ENTITIES OF ZR_Registration_G3 IN LOCAL MODE
     ENTITY Registration
     UPDATE FIELDS ( RegistrationId )
     WITH lt_update.

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
        INTO @DATA(lv_event_exists).

      IF sy-subrc <> 0.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                    id       = 'ZMSG_REG'
                    number   = '003'     " Invalid event
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
        INTO @DATA(lv_part_exists).

      IF sy-subrc <> 0.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                    id       = 'ZMSG_REG'
                    number   = '002'   " Invalid participant
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

      " Read event max participants
      SELECT SINGLE max_participants
        FROM zeventa
        WHERE event_uuid = @<reg>-EventUuid
        INTO @DATA(maxp).

      IF sy-subrc <> 0.
        CONTINUE.  " Event validation handles this
      ENDIF.

      " Count existing registrations
      SELECT COUNT(*)
        FROM zregistrationa
        WHERE event_uuid = @<reg>-EventUuid
        INTO @DATA(current_count).

      IF current_count >= maxp.
        APPEND VALUE #(
          %tky = <reg>-%tky
          %msg = new_message(
                    id       = 'ZMSG_REG'
                    number   = '001'   " Max participants reached
                    severity = if_abap_behv_message=>severity-error )
        ) TO reported-registration.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
