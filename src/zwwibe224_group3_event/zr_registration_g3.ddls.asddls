@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_REGISTRATION_G3'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZR_REGISTRATION_G3 as select from zregistrationa
{
    key registration_uuid as RegistrationUuid,
    registration_id as RegistrationId,
    event_uuid as EventUuid,
    participant_uuid as ParticipantUuid,
    status as Status,
    remarks as Remarks,
    created_by as CreatedBy,
    created_at as CreatedAt,
    last_changed_by as LastChangedBy,
    last_changed_at as LastChangedAt
}
