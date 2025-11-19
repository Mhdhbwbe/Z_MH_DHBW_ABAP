@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZR_Event_G3'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZR_Event_G3
  as select from zeventa
{
    key event_uuid        as EventUuid,
        event_id          as EventId,
        title             as Title,
        location          as Location,
        start_date        as StartDate,
        end_date          as EndDate,
        max_participants  as MaxParticipants,
        status            as Status,
        description       as Description,
        created_by        as CreatedBy,
        created_at        as CreatedAt,
        last_changed_by   as LastChangedBy,
        last_changed_at   as LastChangedAt,

    case status
      when 'P' then 'Planned'
      when 'O' then 'Open'
      when 'C' then 'Closed'
      else 'Unknown'
    end as StatusText
}

