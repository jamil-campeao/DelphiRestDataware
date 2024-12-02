object DmGlobal: TDmGlobal
  OnCreate = DataModuleCreate
  Height = 244
  Width = 421
  object Conn: TFDConnection
    Params.Strings = (
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Port=3050'
      
        'Database=C:\Temp\ws-Delphi\ProjetoRest\DelphiRestDataware\DB\PED' +
        'IDOS.FDB'
      'DriverID=FB')
    ConnectedStoredUsage = []
    LoginPrompt = False
    BeforeConnect = ConnBeforeConnect
    Left = 96
    Top = 48
  end
  object DriverLink: TFDPhysFBDriverLink
    VendorLib = 'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll'
    Left = 192
    Top = 56
  end
end
