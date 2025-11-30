CLASS zbp_r_registration_g3 DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zr_registration_g3.
  PUBLIC SECTION.
    TYPES: zregistrata_tab TYPE STANDARD TABLE OF zregistrationa WITH EMPTY KEY.
    TYPES: zeventa_tab TYPE STANDARD TABLE OF zeventa WITH EMPTY KEY.
    TYPES: BEGIN OF ts_result,
             registrationuuid TYPE zregistrationa-registration_uuid,
           END OF ts_result,
           zr_reg_g3_result_tab TYPE STANDARD TABLE OF ts_result WITH EMPTY KEY.

    CLASS-METHODS assign_registration_id
      IMPORTING entities TYPE zregistrata_tab.
    CLASS-METHODS set_default_status
      IMPORTING entities TYPE zregistrata_tab.

    CLASS-METHODS check_max_participants
      IMPORTING keys TYPE zregistrata_tab.

    CLASS-METHODS check_status_for_action
      IMPORTING keys TYPE zregistrata_tab.

    CLASS-METHODS approve_registration
      IMPORTING keys TYPE zregistrata_tab
      EXPORTING result TYPE zr_reg_g3_result_tab.
    CLASS-METHODS reject_registration
      IMPORTING keys TYPE zregistrata_tab
      EXPORTING result TYPE zr_reg_g3_result_tab.

ENDCLASS.

CLASS zbp_r_registration_g3 IMPLEMENTATION.

  METHOD assign_registration_id.
    DATA: lv_max_id TYPE zregistrationa-registration_id VALUE '00000'.
    SELECT SINGLE MAX( registration_id ) FROM zregistrationa INTO @lv_max_id.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<fs_create>).
      IF <fs_create>-registration_id IS INITIAL.
        lv_max_id = lv_max_id + 1.
        <fs_create>-registration_id = lv_max_id.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_default_status.
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<fs_create>).
      IF <fs_create>-status IS INITIAL.
        <fs_create>-status = 'N'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_max_participants.
    " Lokale Struktur für Event-Info
    TYPES: BEGIN OF ts_event_limits,
             event_uuid       TYPE zeventa-event_uuid,
             max_participants TYPE zeventa-max_participants,
           END OF ts_event_limits.
    DATA: lt_event_limits TYPE STANDARD TABLE OF ts_event_limits WITH EMPTY KEY.
    DATA: ls_event_limit  TYPE ts_event_limits.

    " 1. Hole alle Event-UUIDs aus den eingehenden Keys
    DATA(lt_event_uuids) = VALUE zeventa_tab( FOR key IN keys ( event_uuid = key-event_uuid ) ).

    " 2. Hole Max-Teilnehmer pro Event (INTO am Ende!)
    IF lt_event_uuids IS NOT INITIAL.
      SELECT event_uuid, max_participants FROM zeventa
        FOR ALL ENTRIES IN @lt_event_uuids WHERE event_uuid = @lt_event_uuids-event_uuid
        INTO TABLE @lt_event_limits.
    ENDIF.

    " 3. Hole existierende Registrierungen (nur UUIDs) (Kein GROUP BY, um Syntaxfehler zu vermeiden)
    TYPES: BEGIN OF ts_reg_simple,
             event_uuid TYPE zeventa-event_uuid,
           END OF ts_reg_simple.
    DATA: lt_existing_regs TYPE STANDARD TABLE OF ts_reg_simple WITH EMPTY KEY.

    IF lt_event_uuids IS NOT INITIAL.
      SELECT event_uuid FROM zregistrationa
        FOR ALL ENTRIES IN @lt_event_uuids WHERE event_uuid = @lt_event_uuids-event_uuid AND status <> 'R'
        INTO TABLE @lt_existing_regs. " ✅ KORREKTUR: INTO am Ende
    ENDIF.

    " 4. Validierungsschleife
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_key>).
      " Limit finden
      READ TABLE lt_event_limits INTO ls_event_limit WITH KEY event_uuid = <fs_key>-event_uuid.
      IF sy-subrc = 0.
        " Manuelles Zählen in ABAP (sicherer als SQL GROUP BY in strict mode)
        DATA(lv_current_count) = 0.
        LOOP AT lt_existing_regs TRANSPORTING NO FIELDS WHERE event_uuid = <fs_key>-event_uuid.
          lv_current_count = lv_current_count + 1.
        ENDLOOP.

        " Prüfung
        IF lv_current_count >= ls_event_limit-max_participants.
          " Fehler: Max Teilnehmer erreicht
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_status_for_action.
    DATA: lt_registrations TYPE zregistrata_tab.

    SELECT status FROM zregistrationa
      FOR ALL ENTRIES IN @keys WHERE registration_uuid = @keys-registration_uuid
      INTO TABLE @lt_registrations. " ✅ KORREKTUR: INTO am Ende

    LOOP AT lt_registrations ASSIGNING FIELD-SYMBOL(<fs_reg>).
      IF <fs_reg>-status = 'A' OR <fs_reg>-status = 'R'.
         " Fehler
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD approve_registration.
    DATA(lt_updates) = VALUE zregistrata_tab( FOR key IN keys ( registration_uuid = key-registration_uuid status = 'A' ) ).
    UPDATE zregistrationa FROM TABLE @lt_updates.
  ENDMETHOD.

  METHOD reject_registration.
    DATA(lt_updates) = VALUE zregistrata_tab( FOR key IN keys ( registration_uuid = key-registration_uuid status = 'R' ) ).
    UPDATE zregistrationa FROM TABLE @lt_updates.
  ENDMETHOD.

ENDCLASS.
