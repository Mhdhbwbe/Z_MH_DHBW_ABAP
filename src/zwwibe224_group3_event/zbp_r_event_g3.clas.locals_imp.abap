CLASS lhc_event DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    " --- EVENT METHODEN ---
    METHODS setDefaultStatus FOR DETERMINE ON SAVE
      IMPORTING keys FOR Event~setDefaultStatus.

    METHODS checkStartDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkStartDate.

    METHODS checkEndDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkEndDate.

    METHODS openEvent FOR MODIFY
      IMPORTING keys FOR ACTION Event~openEvent.

    METHODS closeEvent FOR MODIFY
      IMPORTING keys FOR ACTION Event~closeEvent.

    " --- REGISTRATION METHODEN ---
    METHODS assignRegistrationId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Registration~assignRegistrationId.

    METHODS setDefaultRegStatus FOR DETERMINE ON SAVE
      IMPORTING keys FOR Registration~setDefaultRegStatus.

    METHODS checkMaxParticipants FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~checkMaxParticipants.

    METHODS checkStatusOnAction FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~checkStatusOnAction.

    METHODS approveRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~approveRegistration.

    METHODS rejectRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~rejectRegistration.

ENDCLASS.

CLASS lhc_event IMPLEMENTATION.

  " ============================================================================
  " EVENT LOGIK
  " ============================================================================
  METHOD setDefaultStatus.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    LOOP AT events REFERENCE INTO DATA(event).
      IF event->Status IS INITIAL.
        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
          UPDATE FIELDS ( Status ) WITH VALUE #( ( %tky = event->%tky Status = 'P' ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkStartDate.
    DATA: lv_today TYPE d.
    lv_today = sy-datum.

    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Event
      FIELDS ( StartDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    LOOP AT events INTO DATA(event).
      IF event-StartDate < lv_today.
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

  " ============================================================================
  " REGISTRATION LOGIK
  " ============================================================================
  METHOD assignRegistrationId.
    DATA: lv_maxid TYPE zregistrationa-registration_id.
    SELECT SINGLE MAX( registration_id ) FROM zregistrationa INTO @lv_maxid.

    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( RegistrationId ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs REFERENCE INTO DATA(reg).
      IF reg->RegistrationId IS INITIAL.
        lv_maxid = lv_maxid + 1.
        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
          UPDATE FIELDS ( RegistrationId ) WITH VALUE #( ( %tky = reg->%tky RegistrationId = lv_maxid ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setDefaultRegStatus.
    READ ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
      FIELDS ( Status ) WITH CORRESPONDING #( keys )
      RESULT DATA(regs).

    LOOP AT regs REFERENCE INTO DATA(reg).
      IF reg->Status IS INITIAL.
        MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE ENTITY Registration
          UPDATE FIELDS ( Status ) WITH VALUE #( ( %tky = reg->%tky Status = 'New' ) ).
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

      IF lv_current > lv_max.
         APPEND VALUE #( %tky = reg-%tky ) TO failed-registration.
         APPEND VALUE #( %tky = reg-%tky
                         %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Maximale Teilnehmerzahl überschritten.' )
                       ) TO reported-registration.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkStatusOnAction.
    " Leer, da Prüfung in Action erfolgt
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
