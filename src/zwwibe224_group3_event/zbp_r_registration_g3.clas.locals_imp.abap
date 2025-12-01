CLASS lhc_Registration DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS setDefaults FOR DETERMINE ON SAVE
      IMPORTING keys FOR Registration~setDefaults.

    METHODS checkMaxParticipants FOR VALIDATE ON SAVE
      IMPORTING keys FOR Registration~checkMaxParticipants.

    METHODS approveRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~approveRegistration.

    METHODS rejectRegistration FOR MODIFY
      IMPORTING keys FOR ACTION Registration~rejectRegistration.

ENDCLASS.

CLASS lhc_Registration IMPLEMENTATION.

  METHOD setDefaults.
    DATA lv_max_id TYPE zregistrationa-registration_id.
    DATA lt_update TYPE TABLE FOR UPDATE ZR_Registration_G3.

    " Höchste ID aus der DB lesen
    SELECT SINGLE MAX( registration_id )
      FROM zregistrationa
      INTO @lv_max_id.

    IF lv_max_id IS INITIAL.
      lv_max_id = '00000'.
    ENDIF.

    LOOP AT keys INTO DATA(key).
      lv_max_id = lv_max_id + 1.

      " Update vorbereiten: Neue ID und Status 'New'
      APPEND VALUE #(
        RegistrationUuid = key-RegistrationUuid
        RegistrationId   = lv_max_id
        Status           = 'New' " N für New (Anforderung App 1)
        %control-RegistrationId = if_abap_behv=>mk-on
        %control-Status         = if_abap_behv=>mk-on
      ) TO lt_update.
    ENDLOOP.

    " Update ausführen
    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF ZR_Registration_G3 IN LOCAL MODE
        ENTITY Registration
        UPDATE FROM lt_update.
    ENDIF.
  ENDMETHOD.

  METHOD checkMaxParticipants.
    " 1. Lese die Event-UUID der zu prüfenden Registrierungen
    READ ENTITIES OF ZR_Registration_G3 IN LOCAL MODE
      ENTITY Registration
      FIELDS ( EventUuid )
      WITH VALUE #( FOR key IN keys ( RegistrationUuid = key-RegistrationUuid ) )
      RESULT DATA(lt_regs).

    LOOP AT lt_regs INTO DATA(ls_reg).
      " Event-UUID holen
      DATA(lv_event_uuid) = ls_reg-EventUuid.

      IF lv_event_uuid IS NOT INITIAL.
        " 2. Maximale Teilnehmerzahl des Events lesen
        SELECT SINGLE max_participants
          FROM zeventa
          WHERE event_uuid = @lv_event_uuid
          INTO @DATA(lv_max).

        " 3. Aktuelle genehmigte/neue Teilnehmer zählen
        SELECT COUNT( * )
          FROM zregistrationa
          WHERE event_uuid = @lv_event_uuid
            AND status <> 'Rejected' " Rejected zählt nicht mit
          INTO @DATA(lv_current).

        " 4. Prüfung (Anforderung App 1: Fehlermeldung bei Überschreitung)
        IF lv_current >= lv_max.

          INSERT VALUE #( RegistrationUuid = ls_reg-RegistrationUuid )
            INTO TABLE failed-registration.

          INSERT VALUE #(
            RegistrationUuid = ls_reg-RegistrationUuid
            %msg             = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = 'Maximale Teilnehmerzahl für dieses Event überschritten.'
                               )
          ) INTO TABLE reported-registration.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD approveRegistration.
    " Status auf 'Approved' setzen (Anforderung App 2)
    MODIFY ENTITIES OF ZR_Registration_G3 IN LOCAL MODE
      ENTITY Registration
      UPDATE FIELDS ( Status )
      WITH VALUE #( FOR key IN keys
                    ( RegistrationUuid = key-RegistrationUuid
                      Status           = 'Approved' ) ).
                      " Hinweis: Prüfen Sie, ob in Ihrer DB 'Approved' oder 'A' gespeichert wird.
                      " Ggf. auf 'A' ändern, falls das Feld nur 1 Zeichen lang ist.
  ENDMETHOD.

  METHOD rejectRegistration.
    " Status auf 'Rejected' setzen (Anforderung App 2)
    MODIFY ENTITIES OF ZR_Registration_G3 IN LOCAL MODE
      ENTITY Registration
      UPDATE FIELDS ( Status )
      WITH VALUE #( FOR key IN keys
                    ( RegistrationUuid = key-RegistrationUuid
                      Status           = 'Rejected' ) ).
  ENDMETHOD.

ENDCLASS.
