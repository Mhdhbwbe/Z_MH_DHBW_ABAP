@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_Registration_G3'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true

define root view entity ZR_Registration_G3
  as select from zregistrationa

    /* Association to Event */
    association [1..1] to ZR_Event_G3 as _Event
      on _Event.EventUuid = zregistrationa.event_uuid

    /* Association to Participant (for VH + UI) */
    association [1..1] to ZR_PARTICIPANT_G3 as _Participant
      on _Participant.ParticipantUuid = zregistrationa.participant_uuid

{
    /* Keys */
    key registration_uuid as RegistrationUuid,

    /* Basic Fields */
    @UI.lineItem: [ { position: 10 } ]
    @Search.defaultSearchElement: true
    registration_id as RegistrationId,

    @UI.selectionField: [ { position: 10 } ]
    @UI.lineItem:       [ { position: 20 } ]
    status as Status,

    @UI.lineItem: [ { position: 30 } ]
    remarks as Remarks,

    /* ============================== */
    /* Event Information              */
    /* ============================== */
    @UI.lineItem: [ { position: 40, importance: #HIGH } ]
    _Event.Title,

    /* ============================== */
    /* Participant Information        */
    /* ============================== */
    @UI.lineItem: [ { position: 50 } ]
    _Participant.FirstName,

    @UI.lineItem: [ { position: 60 } ]
    _Participant.LastName,

    /* ============================== */
    /*  PARTICIPANT VALUE HELP        */
    /* ============================== */
    @EndUserText.label: 'Participant (UUID)'
    @Consumption.valueHelpDefinition: [
      {
        entity: {
          name: 'ZR_PARTICIPANT_G3',
          element: 'ParticipantUuid'
        }
      }
    ]
    participant_uuid as ParticipantUuid,

    /* Event FK */
    event_uuid as EventUuid,

    /* ============================== */
    /* Admin / Technical Fields       */
    /* ============================== */
    @UI.hidden: true
    created_by as CreatedBy,

    @UI.hidden: true
    created_at as CreatedAt,

    @UI.hidden: true
    last_changed_by as LastChangedBy,

    @UI.hidden: true
    last_changed_at as LastChangedAt,

    /* Associations must be exposed */
    _Event,
    _Participant
}
