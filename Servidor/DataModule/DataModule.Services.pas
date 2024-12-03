unit DataModule.Services;

interface

uses
  System.SysUtils, System.Classes, uRESTDWDatamodule, uRESTDWComponentBase,
  uRESTDWServerEvents, uRESTDWParams, uRESTDWConsts, DataSet.Serialize.Config,
  uRESTDWDataUtils, uRESTDWServerContext;

type
  TDMServices = class(TServerMethodDataModule)
    ServerEvents: TRESTDWServerEvents;
    ServerContext: TRESTDWServerContext;
    procedure ServerEventsEventsusuariosReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerMethodDataModuleCreate(Sender: TObject);
    procedure ServerEventsEventsloginReplyEventByType(var Params: TRESTDWParams;
      var Result: string; const RequestType: TRequestType;
      var StatusCode: Integer; RequestHeader: TStringList);
    procedure ServerMethodDataModuleUserTokenAuth(Welcomemsg, AccessTag: string;
      Params: TRESTDWParams; AuthOptions: TRESTDWAuthTokenParam;
      var ErrorCode: Integer; var ErrorMessage, TokenID: string;
      var Accept: Boolean);
    procedure ServerEventsEventspushReplyEventByType(var Params: TRESTDWParams;
      var Result: string; const RequestType: TRequestType;
      var StatusCode: Integer; RequestHeader: TStringList);
    procedure ServerEventsEventsperfilReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventssenhaReplyEventByType(var Params: TRESTDWParams;
      var Result: string; const RequestType: TRequestType;
      var StatusCode: Integer; RequestHeader: TStringList);
    procedure ServerEventsEventshorarioReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventsnotificacoesReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventscond_pagtoReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventssincronizacaoReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventsproduto_sincronizacaoReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerEventsEventseditar_fotoReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerContextContextListlistar_fotoReplyRequestStream(
      const Params: TRESTDWParams; var ContentType: string;
      const Result: TMemoryStream; const RequestType: TRequestType;
      var StatusCode: Integer);
    procedure ServerEventsEventspedido_sincronizacaoReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMServices: TDMServices;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses Controllers.Usuario, Controllers.Auth, Controllers.Notificacao,
Controllers.CondPagto, Controllers.Cliente, Controllers.Produto, Controllers.Pedido;

{$R *.dfm}

procedure TDMServices.ServerContextContextListlistar_fotoReplyRequestStream(
  const Params: TRESTDWParams; var ContentType: string;
  const Result: TMemoryStream; const RequestType: TRequestType;
  var StatusCode: Integer);
begin
  //GET: /PRODUTOS/FOTO/123
  Controllers.Produto.RegistrarRotasFotoStream(Params, ContentType, Result, RequestType, StatusCode);

end;

procedure TDMServices.ServerEventsEventscond_pagtoReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET: /COND-PAGTO
  Controllers.CondPagto.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventseditar_fotoReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //PUT: /PRODUTOS/FOTO/123
  Controllers.Produto.RegistrarRotasFoto(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventshorarioReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET: /USUARIOS/HORARIO
  Controllers.Usuario.RegistrarRotasHorario(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventsloginReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //POST: /USUARIOS/LOGIN
  Controllers.Usuario.RegistrarRotasLogin(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure TDMServices.ServerEventsEventsnotificacoesReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET /NOTIFICACOES
  Controllers.Notificacao.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventspedido_sincronizacaoReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET, POST /PEDIDOS/SINCRONIZACAO
  Controllers.Pedido.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventsperfilReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //PUT /USUARIOS/PERFIL
  Controllers.Usuario.RegistrarRotasPerfil(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventsproduto_sincronizacaoReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET, POST /PRODUTOS/SINCRONIZACAO
  Controllers.Produto.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure TDMServices.ServerEventsEventspushReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //POST /USUARIOS/PUSH
  Controllers.Usuario.RegistrarRotasPush(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventssenhaReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //PUT /USUARIOS/SENHA
  Controllers.Usuario.RegistrarRotasSenha(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventssincronizacaoReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //GET, POST /CLIENTES/SINCRONIZACAO
  Controllers.Cliente.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure TDMServices.ServerEventsEventsusuariosReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //POST, PUT /USUARIOS
  Controllers.Usuario.RegistrarRotas(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure TDMServices.ServerMethodDataModuleCreate(Sender: TObject);
begin
  TDataSetSerializeConfig.GetInstance.CaseNameDefinition := cndLower;
  TDataSetSerializeConfig.GetInstance.Import.DecimalSeparator := '.';

end;

procedure TDMServices.ServerMethodDataModuleUserTokenAuth(Welcomemsg,
  AccessTag: string; Params: TRESTDWParams; AuthOptions: TRESTDWAuthTokenParam;
  var ErrorCode: Integer; var ErrorMessage, TokenID: string;
  var Accept: Boolean);
begin
  try
    Controllers.Auth.vID_USUARIO := AuthOptions.Secrets.ToInteger;
    Accept := True;
  except
    Accept := False;
  end;
end;

end.
