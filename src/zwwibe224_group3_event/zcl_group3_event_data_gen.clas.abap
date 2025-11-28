CLASS zcl_group3_event_data_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.



CLASS zcl_group3_event_data_gen IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    "---------------------------------------------------------
    " 1) Bestehende Daten löschen
    "---------------------------------------------------------
    DELETE FROM zregistrationa.
    DELETE FROM zeventa.
    DELETE FROM zparticipanta.

    "---------------------------------------------------------
    " 2) Tabellen - Deklaration
    "---------------------------------------------------------
    DATA: lt_events       TYPE TABLE OF zeventa,
          ls_event        TYPE zeventa,
          lt_participants TYPE TABLE OF zparticipanta,
          ls_part         TYPE zparticipanta,
          lt_regs         TYPE TABLE OF zregistrationa,
          ls_reg          TYPE zregistrationa.

    "---------------------------------------------------------
    " 3) Events (5 Stück)
    "   Admin-Felder werden NICHT gesetzt.
    "---------------------------------------------------------
    DO 5 TIMES.
      CLEAR ls_event.

      ls_event-event_uuid   = cl_system_uuid=>create_uuid_x16_static( ).
      ls_event-event_id     = |{ sy-index WIDTH = 5 PAD = '0' ALIGN = RIGHT }|.
      ls_event-title        = |Event { sy-index }|.
      ls_event-location     = SWITCH #( sy-index
                                          WHEN 1 THEN 'Berlin'
                                          WHEN 2 THEN 'Hamburg'
                                          WHEN 3 THEN 'München'
                                          WHEN 4 THEN 'Köln'
                                          ELSE 'Frankfurt' ).
      ls_event-start_date       = |2025120{ sy-index }|.
      ls_event-end_date         = |2025121{ sy-index }|.
      ls_event-max_participants = 50.
      ls_event-status           = 'O'.
      ls_event-description      = |Description for Event { sy-index }|.

      " Adminfelder NICHT setzen!
      CLEAR: ls_event-created_at, ls_event-created_by,
             ls_event-last_changed_at, ls_event-last_changed_by.

      APPEND ls_event TO lt_events.
    ENDDO.

    INSERT zeventa FROM TABLE @lt_events.


    "---------------------------------------------------------
    " 4) Participants (5 Stück)
    "---------------------------------------------------------
    DO 5 TIMES.
      CLEAR ls_part.

      ls_part-participant_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      ls_part-participant_id   = |{ sy-index WIDTH = 5 PAD = '0' ALIGN = RIGHT }|.
      ls_part-first_name       = SWITCH #( sy-index
                                            WHEN 1 THEN 'Anna'
                                            WHEN 2 THEN 'Ben'
                                            WHEN 3 THEN 'Carla'
                                            WHEN 4 THEN 'David'
                                            ELSE 'Elena' ).
      ls_part-last_name        = SWITCH #( sy-index
                                            WHEN 1 THEN 'Meyer'
                                            WHEN 2 THEN 'Schulz'
                                            WHEN 3 THEN 'Bauer'
                                            WHEN 4 THEN 'Hoffmann'
                                            ELSE 'Klein' ).
      ls_part-email            = |user{ sy-index }@example.com|.
      ls_part-phone            = |+49151123456{ sy-index }|.

      CLEAR: ls_part-created_at, ls_part-created_by,
             ls_part-last_changed_at, ls_part-last_changed_by.

      APPEND ls_part TO lt_participants.
    ENDDO.

    INSERT zparticipanta FROM TABLE @lt_participants.


    "---------------------------------------------------------
    " 5) Registrations (5 Stück)
    "---------------------------------------------------------
    DO 5 TIMES.
      CLEAR ls_reg.

      ls_reg-registration_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      ls_reg-registration_id   = |{ sy-index WIDTH = 5 PAD = '0' ALIGN = RIGHT }|.
      ls_reg-event_uuid        = lt_events[ sy-index ]-event_uuid.
      ls_reg-participant_uuid  = lt_participants[ sy-index ]-participant_uuid.
      ls_reg-status            = SWITCH #( sy-index
                                            WHEN 1 THEN 'New'
                                            WHEN 2 THEN 'Approved'
                                            WHEN 3 THEN 'Approved'
                                            WHEN 4 THEN 'Rejected'
                                            ELSE 'New' ).
      ls_reg-remarks = |Registration { sy-index } comment|.

      CLEAR: ls_reg-created_at, ls_reg-created_by,
             ls_reg-last_changed_at, ls_reg-last_changed_by.

      APPEND ls_reg TO lt_regs.
    ENDDO.

    INSERT zregistrationa FROM TABLE @lt_regs.


    "---------------------------------------------------------
    " 6) Ausgabe
    "---------------------------------------------------------
    out->write(
      |Testdaten erfolgreich generiert: { lines( lt_events ) } Events, { lines( lt_participants ) } Participants, { lines( lt_regs ) } Registrations.|
    ).

  ENDMETHOD.

ENDCLASS.

