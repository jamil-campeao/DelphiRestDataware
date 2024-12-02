unit DataModule.Global;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase, System.IniFiles, FireDAC.VCLUI.Wait, System.JSON,
  DataSet.Serialize, FireDac.DApt;

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
    function fInserirUsuario(pNome, pEmail, pSenha: String): TJSonObject;
    function fLogin(pEmail, pSenha: String): TJSonObject;
    procedure fPush(pCodUsuario:Integer; pTokenPush: String);

    { Public declarations }
  end;

var
  DmGlobal: TDmGlobal;

implementation

uses
  FMX.Dialogs, UnitPrincipal;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDmGlobal.CarregarConfigDB(Connection: TFDConnection);
var
  Config: TIniFile;
  vDriverID, vDatabase, vUserName, vPassword, vServer, vVendorLib, vProtocol: string;
  vPort: Integer;
begin
  Config := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    vDriverID  := Config.ReadString('Banco de Dados', 'DriverID', '');
    vDatabase  := Config.ReadString('Banco de Dados', 'Database', '');
    vUserName  := Config.ReadString('Banco de Dados', 'User_name', '');
    vPassword  := Config.ReadString('Banco de Dados', 'Password', '');
    vServer    := Config.ReadString('Banco de Dados', 'Server', '');
    vPort      := Config.ReadInteger('Banco de Dados', 'Port', 3050);
    vVendorLib := Config.ReadString('Banco de Dados', 'VendorLib', '');
    vProtocol  := Config.ReadString('Banco de Dados', 'Protocol', '');

    // Configura a conexão no FireDAC
    Conn.Params.Clear;
    Conn.Params.DriverID := vDriverID;
    Conn.Params.Database := vDatabase;
    Conn.Params.UserName := vUserName;
    Conn.Params.Password := vPassword;
    if vServer <> '' then
      Conn.Params.Add('Server=' + vServer);
    Conn.Params.Add('Port=' + IntToStr(vPort));
    if vProtocol <> '' then
      Conn.Params.Add('Protocol=' + vProtocol);
    Conn.Params.Add('VendorLib=' + vVendorLib);
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao conectar ao banco: ' + E.Message);
      Halt; // Finaliza a aplicação se não conseguir conectar
    end;
  end;
  Config.Free;
end;

procedure TDmGlobal.ConnBeforeConnect(Sender: TObject);

begin
  if Assigned(frmPrincipal) and (frmPrincipal.aLerIni) then
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

function TDmGlobal.fInserirUsuario(pNome, pEmail, pSenha: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' INSERT INTO TAB_USUARIO ' +
                          ' (NOME, EMAIL, SENHA)    ' +
                          '  VALUES                 ' +
                          ' (:NOME, :EMAIL, :SENHA) ' +
                          ' RETURNING COD_USUARIO   ';

    vSQLQuery.ParamByName('NOME').AsString  := pNome;
    vSQLQuery.ParamByName('EMAIL').AsString := pEmail;
    vSQLQuery.ParamByName('SENHA').AsString := pSenha;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJsonObject;


  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fLogin(pEmail, pSenha: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT                 ' +
                          ' COD_USUARIO,           ' +
                          ' NOME,                  ' +
                          ' EMAIL                  ' +
                          ' FROM TAB_USUARIO       ' +
                          ' WHERE                  ' +
                          ' EMAIL = :EMAIL         ' +
                          ' AND SENHA = :SENHA     ';

    vSQLQuery.ParamByName('EMAIL').AsString := pEmail;
    vSQLQuery.ParamByName('SENHA').AsString := pSenha;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJsonObject;


  finally
    FreeAndNil(vSQLQuery);

  end;

end;

procedure TDmGlobal.fPush(pCodUsuario:Integer; pTokenPush: String);
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' UPDATE TAB_USUARIO               ' +
                          ' SET TOKEN_PUSH = :TOKEN_PUSH     ' +
                          ' WHERE COD_USUARIO = :COD_USUARIO ';

    vSQLQuery.ParamByName('TOKEN_PUSH').AsString   := pTokenPush;
    vSQLQuery.ParamByName('COD_USUARIO').asInteger := pCodUsuario;

    vSQLQuery.ExecSQL;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;


end.
