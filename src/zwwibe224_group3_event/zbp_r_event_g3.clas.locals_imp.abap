CLASS lhc_event DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    " --- EVENT: DETERMINATIONS (ON MODIFY) ---
    METHODS assign_event_id FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Event~assign_event_id.

    METHODS setDefaultStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Event~setDefaultStatus.

    " --- EVENT: VALIDATIONS (ON SAVE) ---
    METHODS checkStartDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkStartDate.

    METHODS checkEndDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkEndDate.

    " --- EVENT: ACTIONS ---
    METHODS openEvent FOR MODIFY
      IMPORTING keys FOR ACTION Event~openEvent.

    METHODS closeEvent FOR MODIFY
      IMPORTING keys FOR ACTION Event~closeEvent.

    " --- REGISTRATION: DETERMINATIONS (ON MODIFY) ---
    METHODS assignRegistrationId FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Registration~assignRegistrationId.

    METHODS setDefaultRegStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Registration~setDefaultRegStatus.

    " --- REGISTRATION: VALIDATIONS (ON SAVE) ---
    METHODS checkMaxParticipants FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~checkMaxParticipants.

    " --- REGISTRATION: ACTIONS ---
    METHODS approveRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~approveRegistration.

    METHODS rejectRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~rejectRegistration.

ENDCLASS.

CLASS lhc_event IMPLEMENTATION.

  METHOD assign_event_id.
    " Automatische Nummervergabe für Event
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE
      ENTITY Event
      FIELDS ( EventId ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    DATA: lv_max_id TYPE zeventa-event_id.
    SELECT SINGLE MAX( event_id ) FROM zeventa INTO @lv_max_id.

    DATA(lv_max_int) = 0.
    IF lv_max_id IS NOT INITIAL.
      lv_max_int = lv_max_id.
    ENDIF.

    LOOP AT events INTO DATA(event).
      IF event-EventId IS INITIAL.
        lv_max_int = lv_max_int + 1.
        lv_max_id = lv_max_int. " Automatische Konvertierung nach NUMC

        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE
          ENTITY Event
          UPDATE FROM VALUE #( (
            EventUuid = event-EventUuid
            EventId = lv_max_id
            %control-EventId = 01
          ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setDefaultStatus.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE
      ENTITY Event
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    LOOP AT events INTO DATA(event).
      IF event-Status IS INITIAL.
        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE
          ENTITY Event
          UPDATE FROM VALUE #( (
            EventUuid = event-EventUuid
            Status = 'P'
            %control-Status = 01
          ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkStartDate.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      FIELDS ( StartDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    LOOP AT events INTO DATA(event).
      IF event-StartDate < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky = event-%tky ) TO failed-event.
        APPEND VALUE #( %tky = event-%tky
                        %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Startdatum muss in der Zukunft liegen.' )
                      ) TO reported-event.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkEndDate.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      FIELDS ( StartDate EndDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    LOOP AT events INTO DATA(event).
      IF event-EndDate < event-StartDate.
        APPEND VALUE #( %tky = event-%tky ) TO failed-event.
        APPEND VALUE #( %tky = event-%tky
                        %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Enddatum darf nicht vor Startdatum liegen.' )
                      ) TO reported-event.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD openEvent.
    MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      UPDATE FIELDS ( Status ) WITH VALUE #( FOR key IN keys ( %tky = key-%tky Status = 'O' ) ).
  ENDMETHOD.

  METHOD closeEvent.
    MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      UPDATE FIELDS ( Status ) WITH VALUE #( FOR key IN keys ( %tky = key-%tky Status = 'C' ) ).
  ENDMETHOD.

  METHOD assignRegistrationId.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( RegistrationId ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    DATA: lv_maxid TYPE zregistrationa-registration_id.
    SELECT SINGLE MAX( registration_id ) FROM zregistrationa INTO @lv_maxid.

    DATA(lv_max_int_reg) = 0.
    IF lv_maxid IS NOT INITIAL.
      lv_max_int_reg = lv_maxid.
    ENDIF.

    LOOP AT regs INTO DATA(reg).
      IF reg-RegistrationId IS INITIAL.
        lv_max_int_reg = lv_max_int_reg + 1.
        lv_maxid = lv_max_int_reg.

        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
          UPDATE FROM VALUE #( (
            RegistrationUuid = reg-RegistrationUuid
            RegistrationId = lv_maxid
            %control-RegistrationId = 01
          ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setDefaultRegStatus.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs INTO DATA(reg).
      IF reg-Status IS INITIAL.
        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
          UPDATE FROM VALUE #( (
            RegistrationUuid = reg-RegistrationUuid
            Status = 'New'
            %control-Status = 01
          ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkMaxParticipants.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( EventUuid ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs INTO DATA(reg).
      SELECT SINGLE max_participants FROM zeventa WHERE event_uuid = @reg-EventUuid INTO @DATA(lv_max).
      SELECT COUNT( * ) FROM zregistrationa WHERE event_uuid = @reg-EventUuid AND status <> 'Rejected' INTO @DATA(lv_current).

      IF lv_current >= lv_max.
         APPEND VALUE #( %tky = reg-%tky ) TO failed-registration.
         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Maximale Teilnehmerzahl überschritten.' )
                       ) TO reported-registration.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD approveRegistration.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs INTO DATA(reg).
      IF reg-Status = 'Approved' OR reg-Status = 'Rejected'.
         APPEND VALUE #( %tky = reg-%tky ) TO failed-registration.
         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Bereits bearbeitet.' )
                       ) TO reported-registration.
      ELSE.
         MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
           UPDATE FIELDS ( Status ) WITH VALUE #( ( %tky = reg-%tky Status = 'Approved' ) ).

         " Optional: Erfolgsmeldung in reported schreiben
         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Genehmigt.' )
                       ) TO reported-registration.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD rejectRegistration.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs INTO DATA(reg).
      IF reg-Status = 'Approved' OR reg-Status = 'Rejected'.
         APPEND VALUE #( %tky = reg-%tky ) TO failed-registration.
         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Bereits bearbeitet.' )
                       ) TO reported-registration.
      ELSE.
         MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
           UPDATE FIELDS ( Status ) WITH VALUE #( ( %tky = reg-%tky Status = 'Rejected' ) ).

         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Abgelehnt.' )
                       ) TO reported-registration.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
