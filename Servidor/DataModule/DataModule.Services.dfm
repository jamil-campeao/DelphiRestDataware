object DMServices: TDMServices
  OnCreate = ServerMethodDataModuleCreate
  Encoding = esUtf8
  OnUserTokenAuth = ServerMethodDataModuleUserTokenAuth
  QueuedRequest = False
  Height = 292
  Width = 220
  object ServerEvents: TRESTDWServerEvents
    IgnoreInvalidParams = False
    Events = <
      item
        Routes = [crAll]
        NeedAuthorization = False
        Params = <>
        DataMode = dmRAW
        Name = 'login'
        EventName = 'login'
        BaseURL = '/usuarios/'
        DefaultContentType = 'application/json'
        CallbackEvent = False
        OnlyPreDefinedParams = False
        OnReplyEventByType = ServerEventsEventsloginReplyEventByType
      end
      item
        Routes = [crAll]
        NeedAuthorization = True
        Params = <>
        DataMode = dmRAW
        Name = 'push'
        EventName = 'push'
        BaseURL = '/usuarios/'
        DefaultContentType = 'application/json'
        CallbackEvent = False
        OnlyPreDefinedParams = False
        OnReplyEventByType = ServerEventsEventspushReplyEventByType
      end
      item
        Routes = [crAll]
        NeedAuthorization = True
        Params = <>
        DataMode = dmRAW
        Name = 'perfil'
        EventName = 'perfil'
        BaseURL = '/usuarios/'
        DefaultContentType = 'application/json'
        CallbackEvent = False
        OnlyPreDefinedParams = False
        OnReplyEventByType = ServerEventsEventsperfilReplyEventByType
      end
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
      end
      item
        Routes = [crAll]
        NeedAuthorization = True
        Params = <>
        DataMode = dmRAW
        Name = 'senha'
        EventName = 'senha'
        BaseURL = '/usuarios/'
        DefaultContentType = 'application/json'
        CallbackEvent = False
        OnlyPreDefinedParams = False
        OnReplyEventByType = ServerEventsEventssenhaReplyEventByType
      end>
    Left = 40
    Top = 48
  end
end
