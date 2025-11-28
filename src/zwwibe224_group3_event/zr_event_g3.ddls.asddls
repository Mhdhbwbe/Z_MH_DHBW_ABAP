@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_Event_G3'
//@Metadata.ignorePropagatedAnnotations: true

@Search.searchable: true

@UI.headerInfo: {
typeName: 'Event',
typeNamePlural: 'Events',
title: { value: 'Title' },
description: { value: 'Location' }
}

define root view entity ZR_Event_G3
as select from zeventa
association [0..*] to ZR_Registration_G3 as _Registrations
on _Registrations.EventUuid = zeventa.event_uuid
{

/* KEY */
key event_uuid as EventUuid,

/* LIST REPORT FIELDS */
@UI.lineItem: [ { position: 10, importance: #HIGH } ]
@UI.identification: [ { position: 10 } ]
@UI.selectionField: [ { position: 10 } ]
event_id as EventId,

@UI.lineItem: [ { position: 20, importance: #HIGH } ]
@UI.identification: [ { position: 20 } ]
@Search.defaultSearchElement: true
title as Title,

@UI.lineItem: [ { position: 30, importance: #HIGH } ]
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.7
location as Location,

@UI.lineItem: [ { position: 40, importance: #HIGH } ]
@UI.identification: [ { position: 40 } ]
@UI.selectionField: [ { position: 20 } ]
start_date as StartDate,

@UI.lineItem: [ { position: 50, importance: #HIGH } ]
@UI.identification: [ { position: 50 } ]
end_date as EndDate,

@UI.identification: [ { position: 60 } ]
max_participants as MaxParticipants,

@UI.identification: [ { position: 70 } ]
description as Description,

/* STATUS TEXT */
@UI.lineItem: [ { position: 55, importance: #HIGH } ]
@UI.identification: [ { position: 55 } ]
case
when status = 'P' then 'Planned'
when status = 'O' then 'Open'
when status = 'C' then 'Closed'
else 'Unknown'
end as StatusText,
status as Status,

/* ADMIN FIELDS */
@UI.hidden: true
created_by as CreatedBy,

@UI.hidden: true
created_at as CreatedAt,

@UI.hidden: true
last_changed_by as LastChangedBy,

@UI.hidden: true
last_changed_at as LastChangedAt,

/* ASSOCIATION – OBJECT PAGE TAB */
@UI.facet: [
{
id: 'REGISTRATIONS',
type: #LINEITEM_REFERENCE,
label: 'Registrations',
targetElement: '_Registrations'
}
]

_Registrations

}

