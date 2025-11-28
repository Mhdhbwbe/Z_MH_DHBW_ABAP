@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_PARTICIPANT_G3'
@Metadata.ignorePropagatedAnnotations: true
@Search.searchable: true

define root view entity ZR_PARTICIPANT_G3
  as select from zparticipanta
{
    /* Key */
    key participant_uuid as ParticipantUuid,

    /* Main Data */
    @UI.lineItem:       [ { position: 10, importance: #HIGH } ]
    @Search.defaultSearchElement: true
    participant_id      as ParticipantId,

    @UI.lineItem:       [ { position: 20, importance: #HIGH } ]
    first_name          as FirstName,

    @UI.lineItem:       [ { position: 30, importance: #HIGH } ]
    last_name           as LastName,

    @UI.lineItem: [ { position: 40 } ]
    email               as Email,

    @UI.lineItem: [ { position: 50 } ]
    phone               as Phone,

    /* Admin fields – hidden */
    @UI.hidden: true
    created_by          as CreatedBy,

    @UI.hidden: true
    created_at          as CreatedAt,

    @UI.hidden: true
    last_changed_by     as LastChangedBy,

    @UI.hidden: true
    last_changed_at     as LastChangedAt
}

