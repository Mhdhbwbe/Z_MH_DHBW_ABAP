@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_PARTICIPANT_G3'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZR_PARTICIPANT_G3 as select from zparticipanta
{
    key participant_uuid as ParticipantUuid,
    participant_id as ParticipantId,
    first_name as FirstName,
    last_name as LastName,
    email as Email,
    phone as Phone,
    created_by as CreatedBy,
    created_at as CreatedAt,
    last_changed_by as LastChangedBy,
    last_changed_at as LastChangedAt
}
