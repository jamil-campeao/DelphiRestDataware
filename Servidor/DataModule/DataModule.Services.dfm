object DMServices: TDMServices
  OnCreate = ServerMethodDataModuleCreate
  Encoding = esUtf8
  QueuedRequest = False
  Height = 292
  Width = 483
  object ServerEvents: TRESTDWServerEvents
    IgnoreInvalidParams = False
    Events = <
      item
        Routes = [crAll]
        NeedAuthorization = False
        Params = <>
        DataMode = dmRAW
        Name = 'usuarios'
        EventName = 'usuarios'
        BaseURL = '/'
        DefaultContentType = 'application/json'
        CallbackEvent = False
        OnlyPreDefinedParams = False
        OnReplyEventByType = ServerEventsEventsusuariosReplyEventByType
      end>
    Left = 40
    Top = 48
  end
end
