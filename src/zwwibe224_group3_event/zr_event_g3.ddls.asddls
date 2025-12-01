@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_Event_G3'
@Search.searchable: true

@UI.headerInfo: {
  typeName: 'Event',
  typeNamePlural: 'Events',
  title: { value: 'Title' },
  description: { value: 'Location' }
}

define root view entity ZR_Event_G3
  as select from zeventa
  
  composition [0..*] of ZR_Registration_G3 as _Registrations
{

  key event_uuid as EventUuid,

  // ANFORDERUNG: Filter nach interner Nummer (2 Punkte)
  @UI.lineItem:       [ { position: 10, importance: #HIGH, label: 'Event-Nummer' } ]
  @UI.identification: [ { position: 10, label: 'Event-Nummer' } ]
  @UI.selectionField: [ { position: 10 } ] 
  event_id as EventId,

  @UI.lineItem:       [ { position: 20, importance: #HIGH } ]
  @UI.identification: [ { position: 20, label: 'Titel' } ]
  @Search.defaultSearchElement: true
  title as Title,

  // ANFORDERUNG: Suche nach Ort mit Unschärfe 0.7 (1 Punkt)
  @UI.lineItem:       [ { position: 30, importance: #HIGH } ]
  @UI.identification: [ { position: 30, label: 'Ort' } ]
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.7
  location as Location,

  // ANFORDERUNG: Filter nach Startdatum (2 Punkte)
  @UI.lineItem:       [ { position: 40, importance: #HIGH } ]
  @UI.identification: [ { position: 40, label: 'Startdatum' } ]
  @UI.selectionField: [ { position: 20 } ]
  start_date as StartDate,

  @UI.lineItem:       [ { position: 50, importance: #HIGH } ]
  @UI.identification: [ { position: 50, label: 'Enddatum' } ]
  end_date as EndDate,

  @UI.identification: [ { position: 60, label: 'Max. Teilnehmer' } ]
  max_participants as MaxParticipants,

  @UI.identification: [ { position: 70, label: 'Beschreibung' } ]
  description as Description,

  /* STATUS & BUTTONS */
  @UI.lineItem: [ 
      { position: 55, importance: #HIGH },
      { type: #FOR_ACTION, dataAction: 'openEvent', label: 'Open Event' },
      { type: #FOR_ACTION, dataAction: 'closeEvent', label: 'Close Event' }
  ]
  @UI.identification: [ 
      { position: 55, label: 'Status' },
      { type: #FOR_ACTION, dataAction: 'openEvent', label: 'Open Event' },
      { type: #FOR_ACTION, dataAction: 'closeEvent', label: 'Close Event' }
  ]
  case
    when status = 'P' then 'Planned'
    when status = 'O' then 'Open'
    when status = 'C' then 'Closed'
    else 'Unknown'
  end as StatusText,

  status as Status,

  /* ADMIN FIELDS */
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.lastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt,

  /* FACETS */
  @UI.facet: [
    {
      id: 'General',
      type: #IDENTIFICATION_REFERENCE,
      label: 'General Information',
      position: 10
    },
    {
      id: 'REGISTRATIONS',
      type: #LINEITEM_REFERENCE,
      label: 'Registrations',
      targetElement: '_Registrations',
      position: 20
    }
  ]
  
  _Registrations

}
