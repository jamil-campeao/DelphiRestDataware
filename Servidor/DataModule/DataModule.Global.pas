unit DataModule.Global;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase, System.IniFiles, FireDAC.VCLUI.Wait, System.JSON,
  DataSet.Serialize, FireDac.DApt, uMD5, FMX.Graphics;

type
  TDmGlobal = class(TDataModule)
    Conn: TFDConnection;
    DriverLink: TFDPhysFBDriverLink;
    procedure ConnBeforeConnect(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure CarregarConfigDB(Connection: TFDConnection);
    function fListarItensPedidos(pSQLQuery: TFDQuery; pCodPedido: Integer): TJSONArray;
    { Private declarations }
  public
    function fInserirUsuario(pNome, pEmail, pSenha: String): TJSonObject;
    function fLogin(pEmail, pSenha: String): TJSonObject;
    procedure fPush(pCodUsuario:Integer; pTokenPush: String);
    function fEditarSenha(pCodUsuario: Integer; pSenha: String): TJSonObject;
    function fListarNotificacoes(pCodUsuario: Integer): TJSonArray;
    function fListarCondPagto: TJSonArray;
    function fListarClientes(pDtUltSinc: String; vPagina: Integer): TJSonArray;
    function fInserirEditarCliente(pCodUsuario, pCodClienteLocal: Integer;
      pCnpjCpf, pNome, pFone, pEmail, pEndereco, pNumero, pComplemento, pBairro,
      pCidade, pUF, pCEP: String; pLatitude, pLongitude,
      pLimiteDisponivel: Double; pCodClienteOficial: Integer;
      pDtUltSincronizacao: String): TJSonObject;
    function fListarProdutos(pDtUltSinc: String; vPagina: Integer): TJSonArray;
    function fEditarUsuario(pCodUsuario: Integer; pNome,
      pEmail: String): TJSonObject;
    function fInserirEditarProduto(pCodUsuario, pCodProdutoLocal: Integer;
      pDescricao: String; pValor, pQtdEstoque: Double;
      pCodProdutoOficial: Integer; pDtUltSincronizacao: String): TJSonObject;
    procedure EditarFoto(pCodProduto: Integer; pFoto: TBitMap);
    function fListarFoto(pCodProduto: Integer): TMemoryStream;
    function fListarPedidos(pDtUltSinc: String; pPagina, pCodUsuario: Integer): TJSonArray;
    function fInserirEditarPedido(pCodUsuario, pCodPedidoLocal, pCodCliente: Integer;
                                        pTipoPedido, pDataPedido, pContato, pOBS: String;
                                        pValorTotal: Double; pCodCondPgto: Integer;
                                        pPrazoEntrega, pDataEntrega: String;
                                        pCodPedidoOficial: Integer; pDtUltSincronizacao: String;
                                        pItens: TJSONArray): TJSonObject;
    function fExcluirUsuario(pCodUsuario:Integer): TJSONObject;

    { Public declarations }
  end;

var
  DmGlobal: TDmGlobal;

const
  cQTD_REG_PAG_CLIENTE = 5;
  cQTD_REG_PAG_PRODUTO = 5;
  cQTD_REG_PAG_PEDIDO = 5;

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

    // Configura a conex�o no FireDAC
    Conn.Params.Clear;
    Conn.Params.DriverID := vDriverID;
    Conn.Params.Database := vDatabase;
    Conn.Params.UserName := vUserName;
    Conn.Params.Password := vPassword;
    Conn.Params.Add('Server=' + vServer);
    Conn.Params.Add('Port=' + IntToStr(vPort));
    Conn.Params.Add('Protocol=' + vProtocol);
    Conn.Params.Add('VendorLib=' + vVendorLib);
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao conectar ao banco: ' + E.Message);
      Halt; // Finaliza a aplica��o se n�o conseguir conectar
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
  if (pEmail = '') or (pSenha = '') or (pNome = '') then
    raise Exception.Create('Informe o nome, e-mail e a senha do usu�rio');

  if pSenha.Length < 5 then
    raise Exception.Create('A senha deve conter pelo menos 5 caracteres!');

  vSQLQuery := TFDQuery.Create(nil);
  try
    //Valida��o E-mail
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT COD_USUARIO   ' +
                          ' FROM TAB_USUARIO     ' +
                          ' WHERE EMAIL = :EMAIL ';


    vSQLQuery.ParamByName('EMAIL').AsString  := pEmail;
    vSQLQuery.Active := True;

    if vSQLQuery.RecordCount > 0 then
      raise Exception.Create('Email informado j� esta em uso por outro usu�rio!');

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' INSERT INTO TAB_USUARIO                ' +
                          ' (NOME, EMAIL, SENHA, IND_EXCLUIDO)     ' +
                          '  VALUES                                ' +
                          ' (:NOME, :EMAIL, :SENHA, :IND_EXCLUIDO) ' +
                          ' RETURNING COD_USUARIO                  ';

    vSQLQuery.ParamByName('NOME').AsString         := pNome;
    vSQLQuery.ParamByName('EMAIL').AsString        := pEmail;
    vSQLQuery.ParamByName('IND_EXCLUIDO').AsString := 'N';
    vSQLQuery.ParamByName('SENHA').AsString        := fSaltPassword(pSenha);

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
  if (pEmail = '') or (pSenha = '') then
    raise Exception.Create('Informe o e-mail e a senha');

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
    vSQLQuery.ParamByName('SENHA').AsString := fSaltPassword(pSenha);

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
  if pTokenPush = '' then
    raise Exception.Create('Informe o token push do usu�rio');

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

function TDmGlobal.fEditarUsuario(pCodUsuario:Integer; pNome, pEmail: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  if (pNome = '') or (pEmail = '') then
    raise Exception.Create('Informe o nome e e-mail do usu�rio');

  vSQLQuery := TFDQuery.Create(nil);
  try
    //Valida��o Email
    vSQLQuery.Connection := Conn;

        vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT COD_USUARIO               ' +
                          ' FROM TAB_USUARIO                 ' +
                          ' WHERE EMAIL = :EMAIL             ' +
                          ' AND COD_USUARIO <> :COD_USUARIO  ';

    vSQLQuery.ParamByName('EMAIL').AsString         := pEmail;
    vSQLQuery.ParamByName('COD_USUARIO').AsInteger  := pCodUsuario;
    vSQLQuery.Active := True;

    if vSQLQuery.RecordCount > 0 then
      raise Exception.Create('Email informado j� esta em uso em outra conta!');



    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' UPDATE TAB_USUARIO               ' +
                          ' SET NOME = :NOME,                ' +
                          ' EMAIL = :EMAIL                   ' +
                          ' WHERE COD_USUARIO = :COD_USUARIO ' +
                          ' RETURNING COD_USUARIO            ';

    vSQLQuery.ParamByName('NOME').AsString         := pNome;
    vSQLQuery.ParamByName('EMAIL').AsString        := pEmail;
    vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONObject;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fEditarSenha(pCodUsuario:Integer; pSenha: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  if pSenha = '' then
    raise Exception.Create('Informe a nova senha do usu�rio');

  if pSenha.Length < 5 then
    raise Exception.Create('A senha deve conter pelo menos 5 caracteres!');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' UPDATE TAB_USUARIO               ' +
                          ' SET SENHA = :SENHA               ' +
                          ' WHERE COD_USUARIO = :COD_USUARIO ' +
                          ' RETURNING COD_USUARIO            ';

    vSQLQuery.ParamByName('SENHA').AsString        := fSaltPassword(pSenha);
    vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONObject;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarNotificacoes(pCodUsuario: Integer): TJSonArray;
var
  vSQLQuery: TFDQuery;
begin
  {$REGION 'SELECT NOTIFICACOES'}
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT                     ' +
                          ' COD_NOTIFICACAO,           ' +
                          ' DATA_NOTIFICACAO,          ' +
                          ' TITULO,                    ' +
                          ' TEXTO                      ' +
                          ' FROM TAB_NOTIFICACAO       ' +
                          ' WHERE                      ' +
                          ' COD_USUARIO = :COD_USUARIO ' +
                          ' AND IND_LIDO = :IND_LIDO   ';

    vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;
    vSQLQuery.ParamByName('IND_LIDO').AsString     := 'N';

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONArray;
  {$ENDREGION}


  {$REGION 'UPDATE NOTIFICACOES'}
    //Marco as mensagens como Lidas
    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' UPDATE TAB_NOTIFICACAO     ' +
                          ' SET IND_LIDO = ''S''       ' +
                          ' WHERE                      ' +
                          ' COD_USUARIO = :COD_USUARIO ' +
                          ' AND IND_LIDO = :IND_LIDO   ';

    vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;
    vSQLQuery.ParamByName('IND_LIDO').AsString     := 'N';

    vSQLQuery.ExecSQL;
  {$ENDREGION}

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarCondPagto: TJSonArray;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT                     ' +
                          ' COD_COND_PAGTO,            ' +
                          ' COND_PAGTO                 ' +
                          ' FROM TAB_COND_PAGTO        ' +
                          ' ORDER BY COD_COND_PAGTO    ';
    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONArray;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarClientes(pDtUltSinc: String; vPagina: Integer): TJSonArray;
var
  vSQLQuery: TFDQuery;
begin
  if pDtUltSinc = '' then
    raise Exception.Create('Par�metro dt_ult_sincronizao n�o informado');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT FIRST :FIRST SKIP :SKIP *               ' +
                          ' FROM TAB_CLIENTE                               ' +
                          ' WHERE DATA_ULT_ALTERACAO > :DATA_ULT_ALTERACAO ' +
                          ' ORDER BY COD_CLIENTE                           ';

    vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').AsString  := pDtUltSinc;
    vSQLQuery.ParamByName('FIRST').AsInteger              := cQTD_REG_PAG_CLIENTE;
    vSQLQuery.ParamByName('SKIP').AsInteger               := (vPagina * cQTD_REG_PAG_CLIENTE) - cQTD_REG_PAG_CLIENTE;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONArray;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fInserirEditarCliente(pCodUsuario, pCodClienteLocal: Integer;
                                        pCnpjCpf, pNome, pFone, pEmail, pEndereco,
                                        pNumero, pComplemento, pBairro, pCidade, pUF, pCEP: String;
                                        pLatitude, pLongitude, pLimiteDisponivel: Double;
                                        pCodClienteOficial: Integer; pDtUltSincronizacao: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    if pCodClienteOficial = 0 then
    begin
      vSQLQuery.SQL.Text := ' INSERT INTO TAB_CLIENTE                                                                      '+
                            ' (COD_USUARIO, CNPJ_CPF, NOME, FONE, EMAIL, ENDERECO, NUMERO, COMPLEMENTO,                    '+
                            ' BAIRRO, CIDADE, UF, CEP, LATITUDE, LONGITUDE, LIMITE_DISPONIVEL, DATA_ULT_ALTERACAO)         '+
                            ' VALUES (:COD_USUARIO, :CNPJ_CPF, :NOME, :FONE, :EMAIL, :ENDERECO, :NUMERO, :COMPLEMENTO,     '+
                            ' :BAIRRO, :CIDADE, :UF, :CEP, :LATITUDE, :LONGITUDE, :LIMITE_DISPONIVEL, :DATA_ULT_ALTERACAO) '+
                            ' RETURNING COD_CLIENTE AS COD_CLIENTE_OFICIAL                                                 ';

      vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;
    end
    else
    begin
      vSQLQuery.SQL.Text := ' UPDATE TAB_CLIENTE                                                               '+
                            ' SET CNPJ_CPF = :CNPJ_CPF, NOME = :NOME, FONE = :FONE,                            '+
                            ' EMAIL = :EMAIL, ENDERECO = :ENDERECO, NUMERO = :NUMERO,                          '+
                            ' COMPLEMENTO = :COMPLEMENTO, BAIRRO = :BAIRRO, CIDADE = :CIDADE,                  '+
                            ' UF = :UF, CEP = :CEP, LATITUDE = :LATITUDE, LONGITUDE = :LONGITUDE,              '+
                            ' LIMITE_DISPONIVEL = :LIMITE_DISPONIVEL, DATA_ULT_ALTERACAO = :DATA_ULT_ALTERACAO '+
                            ' WHERE COD_CLIENTE = :COD_CLIENTE                                                 '+
                            ' RETURNING COD_CLIENTE AS COD_CLIENTE_OFICIAL                                     ';

      vSQLQuery.ParamByName('COD_CLIENTE').AsInteger := pCodClienteOficial;
    end;

    vSQLQuery.ParamByName('CNPJ_CPF').AsString            := pCnpjCpf;
    vSQLQuery.ParamByName('NOME').AsString                := pNome;
    vSQLQuery.ParamByName('FONE').AsString                := pFone;
    vSQLQuery.ParamByName('EMAIL').AsString               := pEmail;
    vSQLQuery.ParamByName('ENDERECO').AsString            := pEndereco;
    vSQLQuery.ParamByName('NUMERO').AsString              := pNumero;
    vSQLQuery.ParamByName('COMPLEMENTO').AsString         := pComplemento;
    vSQLQuery.ParamByName('BAIRRO').AsString              := pBairro;
    vSQLQuery.ParamByName('CIDADE').AsString              := pCidade;
    vSQLQuery.ParamByName('UF').AsString                  := pUF;
    vSQLQuery.ParamByName('CEP').AsString                 := pCEP;
    vSQLQuery.ParamByName('LATITUDE').AsFloat             := pLatitude;
    vSQLQuery.ParamByName('LONGITUDE').AsFloat            := pLongitude;
    vSQLQuery.ParamByName('LIMITE_DISPONIVEL').AsFloat    := pLimiteDisponivel;
    vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').AsString  := pDtUltSincronizacao;

    vSQLQuery.Active := True;
    Result           := vSQLQuery.ToJSONObject;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarProdutos(pDtUltSinc: String; vPagina: Integer): TJSonArray;
var
  vSQLQuery: TFDQuery;
begin
  if pDtUltSinc = '' then
    raise Exception.Create('Par�metro dt_ult_sincronizao n�o informado');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT FIRST :FIRST SKIP :SKIP                 '+
                          ' COD_PRODUTO,                                   '+
                          ' DESCRICAO,                                     '+
                          ' VALOR,                                         '+
                          ' QTD_ESTOQUE,                                   '+
                          ' COD_USUARIO,                                   '+
                          ' DATA_ULT_ALTERACAO                             '+
                          ' FROM TAB_PRODUTO                               '+
                          ' WHERE DATA_ULT_ALTERACAO > :DATA_ULT_ALTERACAO '+
                          ' ORDER BY COD_PRODUTO                           ';

    vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').AsString  := pDtUltSinc;
    vSQLQuery.ParamByName('FIRST').AsInteger              := cQTD_REG_PAG_PRODUTO;
    vSQLQuery.ParamByName('SKIP').AsInteger               := (vPagina * cQTD_REG_PAG_PRODUTO) - cQTD_REG_PAG_PRODUTO;

    vSQLQuery.Active := True;

    Result := vSQLQuery.ToJSONArray;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fInserirEditarProduto(pCodUsuario, pCodProdutoLocal: Integer;
                                        pDescricao: String; pValor, pQtdEstoque: Double;
                                        pCodProdutoOficial: Integer; pDtUltSincronizacao: String): TJSonObject;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    if pCodProdutoOficial = 0 then
    begin
      vSQLQuery.SQL.Text := ' INSERT INTO TAB_PRODUTO                                                       '+
                            ' (DESCRICAO, VALOR, QTD_ESTOQUE, COD_USUARIO, DATA_ULT_ALTERACAO)              '+
                            ' VALUES (:DESCRICAO, :VALOR, :QTD_ESTOQUE, :COD_USUARIO, :DATA_ULT_ALTERACAO)  '+
                            ' RETURNING COD_PRODUTO AS COD_PRODUTO_OFICIAL                                  ';

      vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;
    end
    else
    begin
      vSQLQuery.SQL.Text := ' UPDATE TAB_PRODUTO                                                       '+
                            ' SET DESCRICAO = :DESCRICAO, VALOR = :VALOR, QTD_ESTOQUE = :QTD_ESTOQUE,  '+
                            ' DATA_ULT_ALTERACAO = :DATA_ULT_ALTERACAO                                 '+
                            ' WHERE COD_PRODUTO = :COD_PRODUTO                                         '+
                            ' RETURNING COD_PRODUTO AS COD_PRODUTO_OFICIAL                             ';

      vSQLQuery.ParamByName('COD_PRODUTO').AsInteger := pCodProdutoOficial;
    end;

    vSQLQuery.ParamByName('DESCRICAO').AsString            := pDescricao;
    vSQLQuery.ParamByName('VALOR').AsFloat                 := pValor;
    vSQLQuery.ParamByName('QTD_ESTOQUE').AsFloat           := pQtdEstoque;
    vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').AsString   := pDtUltSincronizacao;

    vSQLQuery.Active := True;
    Result           := vSQLQuery.ToJSONObject;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

procedure TDmGlobal.EditarFoto(pCodProduto:Integer; pFoto: TBitMap);
var
  vSQLQuery: TFDQuery;
begin
  if pCodProduto <= 0 then
    raise Exception.Create('O par�metro cod_produto n�o foi informado');

  if pFoto = nil then
    raise Exception.Create('O par�metro foto n�o foi informado');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' UPDATE TAB_PRODUTO               ' +
                          ' SET FOTO = :FOTO                 ' +
                          ' WHERE COD_PRODUTO = :COD_PRODUTO ';

    vSQLQuery.ParamByName('FOTO').Assign(pFoto);
    vSQLQuery.ParamByName('COD_PRODUTO').AsInteger := pCodProduto;

    vSQLQuery.ExecSQL;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarFoto(pCodProduto: Integer): TMemoryStream;
var
  vSQLQuery: TFDQuery;
  vStream  : TStream;
begin
  if pCodProduto <=0 then
    raise Exception.Create('Par�metro cod_produto n�o informado');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT FOTO FROM TAB_PRODUTO     '+
                          ' WHERE COD_PRODUTO = :COD_PRODUTO ';

    vSQLQuery.ParamByName('COD_PRODUTO').AsInteger  := pCodProduto;

    vSQLQuery.Active := True;

    if vSQLQuery.FieldByName('FOTO').AsString = '' then
      raise Exception.Create('O produto n�o possui uma foto cadastrada');

    vStream := vSQLQuery.CreateBlobStream(vSQLQuery.FieldByName('FOTO'), TBlobStreamMode.bmRead);
    Result := TMemoryStream.Create;
    Result.LoadFromStream(vStream);

  finally
    FreeAndNil(vSQLQuery);
    FreeAndNil(vStream);
  end;

end;

function TDmGlobal.fListarPedidos(pDtUltSinc: String; pPagina, pCodUsuario: Integer): TJSonArray;
var
  vSQLQuery: TFDQuery;
  vPedidos : TJsonArray;
  i        : Integer;
begin
  if pDtUltSinc = '' then
    raise Exception.Create('Par�metro dt_ult_sincronizacao n�o informado');

  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;

    vSQLQuery.Active := False;
    vSQLQuery.SQL.Clear;

    vSQLQuery.SQL.Text := ' SELECT FIRST :FIRST SKIP :SKIP *               ' +
                          ' FROM TAB_PEDIDO                                ' +
                          ' WHERE DATA_ULT_ALTERACAO > :DATA_ULT_ALTERACAO ' +
                          ' AND COD_USUARIO = :COD_USUARIO                 ' +
                          ' ORDER BY COD_PEDIDO                            ';

    vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').AsString  := pDtUltSinc;
    vSQLQuery.ParamByName('FIRST').AsInteger              := cQTD_REG_PAG_PEDIDO;
    vSQLQuery.ParamByName('SKIP').AsInteger               := (pPagina * cQTD_REG_PAG_PEDIDO) - cQTD_REG_PAG_PEDIDO;
    vSQLQuery.ParamByName('COD_USUARIO').AsInteger        := pCodUsuario;

    vSQLQuery.Active := True;

    vPedidos := vSQLQuery.ToJSONArray;

    for I := 0 to vPedidos.Size - 1 do
      (vPedidos[i] as TJSONObject).AddPair('itens', fListarItensPedidos(vSQLQuery, vPedidos[i].GetValue<integer>('cod_pedido',0)));

    Result := vPedidos;

  finally
    FreeAndNil(vSQLQuery);

  end;

end;

function TDmGlobal.fListarItensPedidos(pSQLQuery: TFDQuery; pCodPedido: Integer): TJSONArray;
begin
  pSQLQuery.Active := False;
  pSQLQuery.SQL.Clear;

  pSQLQuery.SQL.Text := ' SELECT                          ' +
                        ' COD_ITEM,                       ' +
                        ' COD_PRODUTO,                    ' +
                        ' QTD,                            ' +
                        ' VALOR_UNITARIO,                 ' +
                        ' VALOR_TOTAL                     ' +
                        ' FROM TAB_PEDIDO_ITEM            ' +
                        ' WHERE COD_PEDIDO = :COD_PEDIDO  ' +
                        ' ORDER BY COD_ITEM               ';

  pSQLQuery.ParamByName('COD_PEDIDO').AsInteger  := pCodPedido;
  pSQLQuery.Active := True;

  Result := pSQLQuery.ToJSONArray;
end;

function TDmGlobal.fInserirEditarPedido(pCodUsuario, pCodPedidoLocal, pCodCliente: Integer;
                                        pTipoPedido, pDataPedido, pContato, pOBS: String;
                                        pValorTotal: Double; pCodCondPgto: Integer;
                                        pPrazoEntrega, pDataEntrega: String;
                                        pCodPedidoOficial: Integer; pDtUltSincronizacao: String;
                                        pItens: TJSONArray): TJSonObject;
var
  vSQLQuery: TFDQuery;
  I: Integer;
begin
  {$REGION 'CABECALHO DO PEDIDO'}
  vSQLQuery := TFDQuery.Create(nil);
  try
    vSQLQuery.Connection := Conn;
    try
      Conn.StartTransaction;

      vSQLQuery.Active := False;
      vSQLQuery.SQL.Clear;

      if pCodPedidoOficial = 0 then
      begin
        vSQLQuery.SQL.Text := ' INSERT INTO TAB_PEDIDO                                                     '+
                              ' (COD_CLIENTE, COD_USUARIO, TIPO_PEDIDO, DATA_PEDIDO, CONTATO,              '+
                              ' OBS, VALOR_TOTAL, COD_COND_PAGTO, PRAZO_ENTREGA, DATA_ENTREGA,             '+
                              ' COD_PEDIDO_LOCAL, DATA_ULT_ALTERACAO)                                      '+
                              ' VALUES (:COD_CLIENTE, :COD_USUARIO, :TIPO_PEDIDO, :DATA_PEDIDO, :CONTATO,  '+
                              ' :OBS, :VALOR_TOTAL, :COD_COND_PAGTO, :PRAZO_ENTREGA, :DATA_ENTREGA,        '+
                              ' :COD_PEDIDO_LOCAL, :DATA_ULT_ALTERACAO)                                    '+
                              ' RETURNING COD_PEDIDO AS COD_PEDIDO_OFICIAL                                 ';

        vSQLQuery.ParamByName('COD_USUARIO').AsInteger       := pCodUsuario;
      end
      else
      begin
        vSQLQuery.SQL.Text := ' UPDATE TAB_PEDIDO                                                                                   '+
                              ' SET COD_CLIENTE = :COD_CLIENTE, TIPO_PEDIDO = :TIPO_PEDIDO, DATA_PEDIDO = :DATA_PEDIDO,             '+
                              ' CONTATO = :CONTATO, OBS = :OBS, VALOR_TOTAL = :VALOR_TOTAL, COD_COND_PAGTO = :COD_COND_PAGTO,       '+
                              ' PRAZO_ENTREGA = :PRAZO_ENTREGA, DATA_ENTREGA = :DATA_ENTREGA, COD_PEDIDO_LOCAL = :COD_PEDIDO_LOCAL, '+
                              ' DATA_ULT_ALTERACAO = :DATA_ULT_ALTERACAO                                                            '+
                              ' WHERE COD_PEDIDO = :COD_PEDIDO                                                                      '+
                              ' RETURNING COD_PEDIDO AS COD_PEDIDO_OFICIAL                                                          ';

        vSQLQuery.ParamByName('COD_PEDIDO').AsInteger := pCodPedidoOficial;
      end;

      vSQLQuery.ParamByName('COD_CLIENTE').AsInteger      := pCodCliente;
      vSQLQuery.ParamByName('TIPO_PEDIDO').AsString       := pTipoPedido;
      vSQLQuery.ParamByName('DATA_PEDIDO').Value          := pDataPedido;
      vSQLQuery.ParamByName('CONTATO').AsString           := pContato;
      vSQLQuery.ParamByName('OBS').AsString               := pOBS;
      vSQLQuery.ParamByName('VALOR_TOTAL').AsFloat        := pValorTotal;
      vSQLQuery.ParamByName('COD_COND_PAGTO').AsInteger   := pCodCondPgto;
      vSQLQuery.ParamByName('PRAZO_ENTREGA').AsString     := pPrazoEntrega;
      vSQLQuery.ParamByName('COD_PEDIDO_LOCAL').AsInteger := pCodPedidoLocal;

      if pDataEntrega <> '' then
        vSQLQuery.ParamByName('DATA_ENTREGA').Value := pDataEntrega
      else
      begin
        vSQLQuery.ParamByName('DATA_ENTREGA').DataType := ftString;
        vSQLQuery.ParamByName('DATA_ENTREGA').Clear;
      end;

      vSQLQuery.ParamByName('DATA_ULT_ALTERACAO').Value := pDtUltSincronizacao;


      vSQLQuery.Active  := True;
      Result            := vSQLQuery.ToJSONObject;
      pCodPedidoOficial := vSQLQuery.FieldByName('COD_PEDIDO_OFICIAL').AsInteger;
      {$ENDREGION}

      {$REGION 'ITENS DO PEDIDO'}
      vSQLQuery.Active := False;
      vSQLQuery.SQL.Clear;
      vSQLQuery.SQL.Text := ' DELETE FROM TAB_PEDIDO_ITEM    '+
                            ' WHERE COD_PEDIDO = :COD_PEDIDO ';

      vSQLQuery.ParamByName('COD_PEDIDO').AsInteger := pCodPedidoOficial;
      vSQLQuery.ExecSQL;

      for I := 0 to pItens.Size - 1 do
      begin
        vSQLQuery.Active := False;
        vSQLQuery.SQL.Clear;
        vSQLQuery.SQL.Text := ' INSERT INTO TAB_PEDIDO_ITEM       '+
                              ' (COD_PEDIDO, COD_PRODUTO, QTD,    '+
                              ' VALOR_UNITARIO, VALOR_TOTAL   )   '+
                              ' VALUES                            '+
                              ' (:COD_PEDIDO, :COD_PRODUTO, :QTD, '+
                              ' :VALOR_UNITARIO, :VALOR_TOTAL)    ';

        vSQLQuery.ParamByName('COD_PEDIDO').AsInteger   := pCodPedidoOficial;
        vSQLQuery.ParamByName('COD_PRODUTO').AsInteger  := pItens[i].GetValue<integer>('cod_produto',0);
        vSQLQuery.ParamByName('QTD').AsFloat            := pItens[i].GetValue<double>('qtd',0);
        vSQLQuery.ParamByName('VALOR_UNITARIO').AsFloat := pItens[i].GetValue<double>('valor_unitario',0);
        vSQLQuery.ParamByName('VALOR_TOTAL').AsFloat    := pItens[i].GetValue<double>('valor_total',0);
        vSQLQuery.ExecSQL;
      end;
      {$ENDREGION}

      Conn.Commit;

    except on e:Exception do
      begin
        Conn.Rollback;
        raise Exception.Create('Erro ao atualizar ou inserir pedido:' + e.Message);
      end;
    end;

  finally
    FreeAndNil(vSQLQuery);
  end;

end;


function TDmGlobal.fExcluirUsuario(pCodUsuario:Integer): TJSONObject;
var
  vSQLQuery: TFDQuery;
begin
  vSQLQuery := TFDQuery.Create(nil);
  try
    Conn.StartTransaction;
    vSQLQuery.Connection := Conn;
    try

      vSQLQuery.Active := False;
      vSQLQuery.SQL.Clear;

      vSQLQuery.SQL.Text := ' UPDATE TAB_USUARIO                   ' +
                            ' SET IND_EXCLUIDO = :IND_EXCLUIDO,    ' +
                            ' EMAIL = :EMAIL, NOME = :NOME,        ' +
                            ' TOKEN_PUSH = :TOKEN_PUSH,            ' +
                            ' PLATAFORMA = :PLATAFORMA             ' +
                            ' WHERE COD_USUARIO = :COD_USUARIO     ' +
                            ' RETURNING COD_USUARIO AS COD_USUARIO ';

      vSQLQuery.ParamByName('IND_EXCLUIDO').AsString := 'S';
      vSQLQuery.ParamByName('EMAIL').AsString        := 'USU�RIO EXCLU�DO';
      vSQLQuery.ParamByName('NOME').AsString         := 'USU�RIO EXCLU�DO';
      vSQLQuery.ParamByName('TOKEN_PUSH').AsString   := '';
      vSQLQuery.ParamByName('PLATAFORMA').AsString   := '';
      vSQLQuery.ParamByName('COD_USUARIO').AsInteger := pCodUsuario;
      vSQLQuery.Active := True;

      Result := vSQLQuery.ToJSonObject;

    except on e:Exception do
      begin
        Conn.Rollback;
        raise Exception.Create('Erro ao excluir usu�rio: ' + e.Message);
      end;

    end;

    Conn.Commit;
  finally
    FreeAndNil(vSQLQuery);

  end;

end;

end.
