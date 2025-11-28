CLASS lhc_Event DEFINITION INHERITING FROM cl_abap_behavior_handler.

PRIVATE SECTION.



METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION

IMPORTING keys REQUEST requested_authorizations FOR Event RESULT result.



METHODS close_event FOR MODIFY

IMPORTING keys FOR ACTION Event~close_event RESULT result.



METHODS open_event FOR MODIFY

IMPORTING keys FOR ACTION Event~open_event RESULT result.



METHODS assign_event_id FOR DETERMINE ON MODIFY

IMPORTING keys FOR Event~assign_event_id.



METHODS set_default_status FOR DETERMINE ON MODIFY

IMPORTING keys FOR Event~set_default_status.



METHODS check_end_date FOR VALIDATE ON SAVE

IMPORTING keys FOR Event~check_end_date.



METHODS check_start_date FOR VALIDATE ON SAVE

IMPORTING keys FOR Event~check_start_date.



ENDCLASS.



CLASS lhc_Event IMPLEMENTATION.



METHOD get_instance_authorizations.

" Für die Prüfungsaufgabe nichts nötig

ENDMETHOD.



"*-------------------------------------------------------------*

"* Determination: assign_event_id

"* - beim CREATE aufgerufen

"*-------------------------------------------------------------*

METHOD assign_event_id.



" Max EventId aus DB

SELECT MAX( event_id )

FROM zeventa

INTO @DATA(lv_max_id_db).



DATA(lv_max_int) = 0.

IF sy-subrc = 0 AND lv_max_id_db IS NOT INITIAL.

lv_max_int = lv_max_id_db.

ENDIF.



DATA lt_update TYPE TABLE FOR UPDATE zr_event_g3\\Event.



LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).



lv_max_int = lv_max_int + 1.



DATA(lv_new_id) = |{ lv_max_int WIDTH = 5 ALIGN = RIGHT PAD = '0' }|.



APPEND VALUE #(

%tky = <ls_key>-%tky

EventId = lv_new_id

) TO lt_update.



ENDLOOP.



MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

UPDATE

FIELDS ( EventId )

WITH lt_update.



ENDMETHOD.







"*-------------------------------------------------------------*

"* Determination: set_default_status

"* - beim CREATE, Status = 'P'

"*-------------------------------------------------------------*

METHOD set_default_status.



READ ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

FIELDS ( status )

WITH CORRESPONDING #( keys )

RESULT DATA(lt_events).



LOOP AT lt_events ASSIGNING FIELD-SYMBOL(<ls_event>).



" Nur wenn Status noch leer ist

IF <ls_event>-status IS INITIAL.



MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

UPDATE

FIELDS ( status )

WITH VALUE #( ( %tky = <ls_event>-%tky

status = 'P' ) ). " Planned



ENDIF.



ENDLOOP.



ENDMETHOD.



"*-------------------------------------------------------------*

"* Validation: check_start_date

"* - StartDate darf nicht in der Vergangenheit liegen

"* - Message 001 aus ZSMG_EVENT

"*-------------------------------------------------------------*

METHOD check_start_date.



DATA(lv_today) = cl_abap_context_info=>get_system_date( ).



LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).



SELECT SINGLE start_date

FROM zeventa

WHERE event_uuid = @<ls_key>-EventUuid

INTO @DATA(lv_start_date).



IF sy-subrc = 0 AND lv_start_date < lv_today.



APPEND VALUE #(

%tky = <ls_key>-%tky

%msg = new_message(

id = 'ZSMG_EVENT'

number = '001'

severity = if_abap_behv_message=>severity-error ) )

TO reported-event.



APPEND VALUE #( %tky = <ls_key>-%tky )

TO failed-event.



ENDIF.



ENDLOOP.



ENDMETHOD.







"*-------------------------------------------------------------*

"* Validation: check_end_date

"* - EndDate darf nicht vor StartDate liegen

"* - Message 002 aus ZSMG_EVENT

"*-------------------------------------------------------------*

METHOD check_end_date.



LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).



SELECT SINGLE start_date, end_date

FROM zeventa

WHERE event_uuid = @<ls_key>-EventUuid

INTO @DATA(ls_event_db).



IF sy-subrc = 0

AND ls_event_db-end_date IS NOT INITIAL

AND ls_event_db-start_date IS NOT INITIAL

AND ls_event_db-end_date < ls_event_db-start_date.



APPEND VALUE #(

%tky = <ls_key>-%tky

%msg = new_message(

id = 'ZSMG_EVENT'

number = '002'

severity = if_abap_behv_message=>severity-error ) )

TO reported-event.



APPEND VALUE #( %tky = <ls_key>-%tky )

TO failed-event.



ENDIF.



ENDLOOP.



ENDMETHOD.







"*-------------------------------------------------------------*

"* Action: open_event

"* - setzt Status = 'O'

"* - Message 004 aus ZSMG_EVENT

"*-------------------------------------------------------------*

METHOD open_event.



READ ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

FIELDS ( status )

WITH CORRESPONDING #( keys )

RESULT DATA(lt_events).



LOOP AT lt_events ASSIGNING FIELD-SYMBOL(<ls_event>).



" Status auf 'O' setzen

MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

UPDATE

FIELDS ( status )

WITH VALUE #( ( %tky = <ls_event>-%tky

status = 'O' ) ).



" Erfolgsmeldung

APPEND VALUE #(

%tky = <ls_event>-%tky

%msg = new_message(

id = 'ZSMG_EVENT'

number = '004'

severity = if_abap_behv_message=>severity-success ) )

TO reported-event.



" Action-Result (SELF)

APPEND VALUE #( %tky = <ls_event>-%tky ) TO result.



ENDLOOP.



ENDMETHOD.



"*-------------------------------------------------------------*

"* Action: close_event

"* - setzt Status = 'C'

"* - Message 005 aus ZSMG_EVENT

"*-------------------------------------------------------------*

METHOD close_event.



READ ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

FIELDS ( status )

WITH CORRESPONDING #( keys )

RESULT DATA(lt_events).



LOOP AT lt_events ASSIGNING FIELD-SYMBOL(<ls_event>).



" Status auf 'C' setzen

MODIFY ENTITIES OF zr_event_g3 IN LOCAL MODE

ENTITY Event

UPDATE

FIELDS ( status )

WITH VALUE #( ( %tky = <ls_event>-%tky

status = 'C' ) ).



" Erfolgsmeldung

APPEND VALUE #(

%tky = <ls_event>-%tky

%msg = new_message(

id = 'ZSMG_EVENT'

number = '005'

severity = if_abap_behv_message=>severity-success ) )

TO reported-event.



" Action-Result (SELF)

APPEND VALUE #( %tky = <ls_event>-%tky ) TO result.



ENDLOOP.



ENDMETHOD.



ENDCLASS.
