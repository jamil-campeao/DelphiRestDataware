unit DataModule.Global;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase, System.IniFiles, FireDAC.VCLUI.Wait;

type
  TDmGlobal = class(TDataModule)
    Conn: TFDConnection;
    DriverLink: TFDPhysFBDriverLink;
    procedure ConnBeforeConnect(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure CarregarConfigDB(Connection: TFDConnection);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DmGlobal: TDmGlobal;

implementation

uses
  FMX.Dialogs;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDmGlobal.CarregarConfigDB(Connection: TFDConnection);
var
    vIni : TIniFile;
    vArq: string;
begin
  try
    vArq := ExtractFilePath(ParamStr(0)) + 'config.ini';

    // Validar arquivo vIni...
    if NOT FileExists(vArq) then
      raise Exception.Create('Arquivo INI não encontrado: ' + vArq);

    // Instanciar arquivo vIni...
    vIni := TIniFile.Create(vArq);
    Connection.DriverName := vIni.ReadString('Banco de Dados', 'DriverID', '');

    // Buscar dados do arquivo fisico...
    with Connection.Params do
    begin
      Clear;
      Add('DriverID=' + vIni.ReadString('Banco de Dados', 'DriverID', ''));
      Add('Database=' + vIni.ReadString('Banco de Dados', 'Database', ''));
      Add('User_Name=' + vIni.ReadString('Banco de Dados', 'User_name', ''));
      Add('Password=' + vIni.ReadString('Banco de Dados', 'Password', ''));

      if vIni.ReadString('Banco de Dados', 'Port', '') <> '' then
        Add('Port=' + vIni.ReadString('Banco de Dados', 'Port', ''));

      if vIni.ReadString('Banco de Dados', 'Server', '') <> '' then
        Add('Server=' + vIni.ReadString('Banco de Dados', 'Server', ''));

      if vIni.ReadString('Banco de Dados', 'Protocol', '') <> '' then
        Add('Protocol=' + vIni.ReadString('Banco de Dados', 'Protocol', ''));

      if vIni.ReadString('Banco de Dados', 'VendorLib', '') <> '' then
        DriverLink.VendorLib := vIni.ReadString('Banco de Dados', 'VendorLib', '');
    end;
  finally
    if Assigned(vIni) then
      vIni.DisposeOf;
  end;
end;

procedure TDmGlobal.ConnBeforeConnect(Sender: TObject);
begin
  CarregarConfigDB(Conn);
end;

procedure TDmGlobal.DataModuleCreate(Sender: TObject);
begin
  try
    Conn.Connected := True;
  except
    ShowMessage('Erro ao se conectar com o Banco de Dados');
  end;
end;

end.
