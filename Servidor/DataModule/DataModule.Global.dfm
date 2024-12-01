object DmGlobal: TDmGlobal
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object Conn: TFDConnection
    Params.Strings = (
      'Database=C:\Projetos\Delphi_Rest\DB\PEDIDOS.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Server=localhost'
      'Protocol=TCPIP'
      'Port=3050'
      'DriverID=FB')
    ConnectedStoredUsage = []
    LoginPrompt = False
    BeforeConnect = ConnBeforeConnect
    Left = 232
    Top = 112
  end
  object DriverLink: TFDPhysFBDriverLink
    VendorLib = 'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll'
    Left = 344
    Top = 112
  end
end
