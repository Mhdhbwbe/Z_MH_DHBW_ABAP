CLASS lhc_Event DEFINITION
  INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS setDefaultStatus
      FOR DETERMINE ON SAVE
      IMPORTING keys FOR Event~setDefaultStatus.

    METHODS checkStartDate
      FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkStartDate.

    METHODS checkEndDate
      FOR VALIDATE ON SAVE
      IMPORTING keys FOR Event~checkEndDate.

    METHODS openEvent
      FOR MODIFY
      IMPORTING keys FOR ACTION Event~openEvent.

    METHODS closeEvent
      FOR MODIFY
      IMPORTING keys FOR ACTION Event~closeEvent.

ENDCLASS.


CLASS lhc_Event IMPLEMENTATION.

  METHOD setDefaultStatus.
    DATA lv_max_id TYPE zeventa-event_id.
    DATA lt_update TYPE TABLE FOR UPDATE ZR_Event_G3.

    SELECT MAX( event_id )
      FROM zeventa
      INTO @lv_max_id.

    IF lv_max_id IS INITIAL.
      lv_max_id = '00000'.
    ENDIF.

    LOOP AT keys INTO DATA(key).
      lv_max_id = lv_max_id + 1.
      APPEND VALUE #(
        EventUuid = key-EventUuid
        EventId   = lv_max_id
        Status    = 'P'
      ) TO lt_update.
    ENDLOOP.

    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF ZR_Event_G3 IN LOCAL MODE
        ENTITY Event
        UPDATE FIELDS ( EventId Status )
        WITH lt_update.
    ENDIF.
  ENDMETHOD.

  METHOD checkStartDate.
    DATA lt_events TYPE TABLE FOR READ RESULT ZR_Event_G3.

    READ ENTITIES OF ZR_Event_G3 IN LOCAL MODE
      ENTITY Event
      FIELDS ( StartDate )
      WITH VALUE #(
        FOR key IN keys
          ( EventUuid = key-EventUuid )
      )
      RESULT lt_events.

    LOOP AT lt_events INTO DATA(ls_event).
      IF ls_event-%data-StartDate IS NOT INITIAL
         AND ls_event-%data-StartDate < sy-datum.

        INSERT VALUE #( EventUuid = ls_event-%key-EventUuid )
          INTO TABLE failed-Event.

        INSERT VALUE #(
          EventUuid          = ls_event-%key-EventUuid
          %msg               = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = 'Start date must not be in the past' )
          %element-StartDate = if_abap_behv=>mk-on
        ) INTO TABLE reported-Event.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD checkEndDate.
    DATA lt_events TYPE TABLE FOR READ RESULT ZR_Event_G3.

    READ ENTITIES OF ZR_Event_G3 IN LOCAL MODE
      ENTITY Event
      FIELDS ( StartDate EndDate )
      WITH VALUE #(
        FOR key IN keys
          ( EventUuid = key-EventUuid )
      )
      RESULT lt_events.

    LOOP AT lt_events INTO DATA(ls_event).
      IF ls_event-%data-EndDate IS NOT INITIAL
         AND ls_event-%data-EndDate < ls_event-%data-StartDate.

        INSERT VALUE #( EventUuid = ls_event-%key-EventUuid )
          INTO TABLE failed-Event.

        INSERT VALUE #(
          EventUuid        = ls_event-%key-EventUuid
          %msg             = new_message_with_text(
                               severity = if_abap_behv_message=>severity-error
                               text     = 'End date must not be before start date' )
          %element-EndDate = if_abap_behv=>mk-on
        ) INTO TABLE reported-Event.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD openEvent.
    MODIFY ENTITIES OF ZR_Event_G3 IN LOCAL MODE
      ENTITY Event
      UPDATE FIELDS ( Status )
      WITH VALUE #(
        FOR key IN keys
          ( EventUuid = key-EventUuid
            Status    = 'O' )
      ).
  ENDMETHOD.

  METHOD closeEvent.
    MODIFY ENTITIES OF ZR_Event_G3 IN LOCAL MODE
      ENTITY Event
      UPDATE FIELDS ( Status )
      WITH VALUE #(
        FOR key IN keys
          ( EventUuid = key-EventUuid
            Status    = 'C' )
      ).
  ENDMETHOD.

ENDCLASS.
