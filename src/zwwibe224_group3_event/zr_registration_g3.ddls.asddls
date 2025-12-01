@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_Registration_G3'

@UI.headerInfo: {
  typeName: 'Registration',
  typeNamePlural: 'Registrations',
  title: { value: 'RegistrationId' },
  description: { value: 'Status' }
}

define view entity ZR_Registration_G3
  as select from zregistrationa
  
  association to parent ZR_Event_G3       as _Event
    on $projection.EventUuid = _Event.EventUuid
    
  association [1..1] to ZR_PARTICIPANT_G3 as _Participant
    on $projection.ParticipantUuid = _Participant.ParticipantUuid
{
  key registration_uuid as RegistrationUuid,

  event_uuid as EventUuid,

  @UI.lineItem:       [ { position: 10, importance: #HIGH, label: 'Reg. ID' } ]
  @UI.identification: [ { position: 10, label: 'Registration ID' } ]
  registration_id as RegistrationId,

  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZR_Participant_G3', element: 'ParticipantUuid' }
  }]
  @ObjectModel.text.association: '_Participant'
  @UI.lineItem:       [ { position: 20, importance: #HIGH, label: 'Teilnehmer' } ]
  @UI.identification: [ { position: 20, label: 'Teilnehmer' } ]
  participant_uuid as ParticipantUuid,

  /* STATUS & BUTTONS */
  @UI.selectionField: [ { position: 10 } ]
  @UI.lineItem:       [ 
    { position: 30, importance: #HIGH, label: 'Status' },
    { type: #FOR_ACTION, dataAction: 'approveRegistration', label: 'Approve' },
    { type: #FOR_ACTION, dataAction: 'rejectRegistration', label: 'Reject' }
  ]
  @UI.identification: [ 
    { position: 30, label: 'Status' },
    { type: #FOR_ACTION, dataAction: 'approveRegistration', label: 'Approve' },
    { type: #FOR_ACTION, dataAction: 'rejectRegistration', label: 'Reject' }
  ]
  status as Status,

  @UI.lineItem:       [ { position: 40, importance: #MEDIUM, label: 'Bemerkung' } ]
  @UI.identification: [ { position: 40, label: 'Bemerkung' } ]
  remarks as Remarks,

  /* ADMIN FIELDS */
  @Semantics.user.createdBy: true
  created_by      as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at      as CreatedAt,
  @Semantics.user.lastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt,

  /* FACET */
  @UI.facet: [
    {
      id: 'General',
      type: #IDENTIFICATION_REFERENCE,
      label: 'Registration Details',
      position: 10
    }
  ]
  
  _Participant,
  _Event
}
