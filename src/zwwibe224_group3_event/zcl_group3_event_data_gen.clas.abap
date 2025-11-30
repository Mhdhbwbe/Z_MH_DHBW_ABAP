CLASS zcl_group3_event_data_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    METHODS generate_data
      IMPORTING
        io_out TYPE REF TO if_oo_adt_classrun_out.

ENDCLASS.

CLASS zcl_group3_event_data_gen IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    me->generate_data( out ).
  ENDMETHOD.

  METHOD generate_data.
    DATA: lt_events      TYPE TABLE OF zeventa,
          lt_participants TYPE TABLE OF zparticipanta,
          lt_registrations TYPE TABLE OF zregistrationa.

    DATA: lv_timestamp TYPE p LENGTH 11 DECIMALS 7.
    GET TIME STAMP FIELD lv_timestamp.
    DATA(lv_date) = sy-datum.

    " 1. ZPARTICIPANTA (Teilnehmer) - Mindestens 5 Datensätze
    lt_participants = VALUE #(
      ( participant_uuid = cl_system_uuid=>create_uuid_x16_static( )
        Participant_Id = '00001' first_name = 'Max' last_name = 'Mustermann'
        email = 'max.muster@mail.de' phone = '+49123456789' created_at = lv_timestamp )
      ( participant_uuid = cl_system_uuid=>create_uuid_x16_static( )
        Participant_Id = '00002' first_name = 'Anna' last_name = 'Musterfrau'
        email = 'anna.m@mail.de' phone = '+49987654321' created_at = lv_timestamp )
      ( participant_uuid = cl_system_uuid=>create_uuid_x16_static( )
        Participant_Id = '00003' first_name = 'Chris' last_name = 'Schmidt'
        email = 'c.schmidt@mail.de' phone = '+49112233445' created_at = lv_timestamp )
      ( participant_uuid = cl_system_uuid=>create_uuid_x16_static( )
        Participant_Id = '00004' first_name = 'Lena' last_name = 'Wagner'
        email = 'lena.w@mail.de' phone = '+49667788990' created_at = lv_timestamp )
      ( participant_uuid = cl_system_uuid=>create_uuid_x16_static( )
        Participant_Id = '00005' first_name = 'Tom' last_name = 'Kaiser'
        email = 'tom.k@mail.de' phone = '+49135791357' created_at = lv_timestamp )
    ).
    INSERT zparticipanta FROM TABLE @lt_participants.
    io_out->write( |{ sy-dbcnt } participants created.| ).

    " 2. ZEVENTA (Veranstaltung) - Mindestens 5 Datensätze
    lt_events = VALUE #(
    ( event_uuid = cl_system_uuid=>create_uuid_x16_static( ) event_id = '00001'
      title = 'RAP Workshop 2026' location = 'Raum A101' start_date = lv_date
      end_date = lv_date max_participants = 10 status = 'O'
      description = 'Einführung in RAP und Fiori Elements.' created_at = lv_timestamp )
    ( event_uuid = cl_system_uuid=>create_uuid_x16_static( ) event_id = '00002'
      title = 'SAP S/4HANA Keynote' location = 'Halle 5' start_date = lv_date + 7
      end_date = lv_date + 7 max_participants = 50 status = 'P'
      description = 'Die neuesten S/4HANA Features.' created_at = lv_timestamp )
    ( event_uuid = cl_system_uuid=>create_uuid_x16_static( ) event_id = '00003'
      title = 'ABAP Cloud Summit' location = 'Online' start_date = lv_date + 14
      end_date = lv_date + 15 max_participants = 200 status = 'P'
      description = 'Entwicklung in der ABAP Cloud.' created_at = lv_timestamp )
    ( event_uuid = cl_system_uuid=>create_uuid_x16_static( ) event_id = '00004'
      title = 'Fiori UX Session' location = 'Raum C303' start_date = lv_date + 21
      end_date = lv_date + 21 max_participants = 15 status = 'P'
      description = 'Verbesserung der User Experience.' created_at = lv_timestamp )
    ( event_uuid = cl_system_uuid=>create_uuid_x16_static( ) event_id = '00005'
      title = 'Abschluss-Party' location = 'Mensa' start_date = lv_date + 30
      end_date = lv_date + 30 max_participants = 100 status = 'C'
      description = 'Feier zum Abschluss des Semesters.' created_at = lv_timestamp )
    ).
    INSERT zeventa FROM TABLE @lt_events.
    io_out->write( |{ sy-dbcnt } events created.| ).

    " 3. ZREGISTRATIONA (Teilnahme) - Mindestens 5 Datensätze
    READ TABLE lt_events INDEX 1 INTO DATA(ls_event1).
    READ TABLE lt_participants INDEX 1 INTO DATA(ls_part1).
    READ TABLE lt_participants INDEX 2 INTO DATA(ls_part2).
    READ TABLE lt_participants INDEX 3 INTO DATA(ls_part3).
    READ TABLE lt_participants INDEX 4 INTO DATA(ls_part4).
    READ TABLE lt_events INDEX 2 INTO DATA(ls_event2).
    READ TABLE lt_participants INDEX 5 INTO DATA(ls_part5).

    lt_registrations = VALUE #(
      ( registration_uuid = cl_system_uuid=>create_uuid_x16_static( ) registration_id = '00001'
        event_uuid = ls_event1-event_uuid participant_uuid = ls_part1-participant_uuid status = 'A' created_at = lv_timestamp )
      ( registration_uuid = cl_system_uuid=>create_uuid_x16_static( ) registration_id = '00002'
        event_uuid = ls_event1-event_uuid participant_uuid = ls_part2-participant_uuid status = 'N' created_at = lv_timestamp )
      ( registration_uuid = cl_system_uuid=>create_uuid_x16_static( ) registration_id = '00003'
        event_uuid = ls_event1-event_uuid participant_uuid = ls_part3-participant_uuid status = 'R' created_at = lv_timestamp )
      ( registration_uuid = cl_system_uuid=>create_uuid_x16_static( ) registration_id = '00004'
        event_uuid = ls_event2-event_uuid participant_uuid = ls_part4-participant_uuid status = 'N' created_at = lv_timestamp )
      ( registration_uuid = cl_system_uuid=>create_uuid_x16_static( ) registration_id = '00005'
        event_uuid = ls_event2-event_uuid participant_uuid = ls_part5-participant_uuid status = 'N' created_at = lv_timestamp )
    ).
    INSERT zregistrationa FROM TABLE @lt_registrations.
    io_out->write( |{ sy-dbcnt } registrations created.| ).

    COMMIT WORK.
    io_out->write( |Test data generation completed and committed.| ).

  ENDMETHOD.

ENDCLASS.
