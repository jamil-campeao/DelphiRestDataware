unit Controllers.Usuario;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions, Controllers.Auth;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure RegistrarRotasLogin(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure RegistrarRotasPerfil(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);


procedure InserirUsuario(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure Login(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure RegistrarRotasPush(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure Push(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure EditarUsuario(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

implementation

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtPost then
    InserirUsuario(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure RegistrarRotasLogin(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtPost then
    Login(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure RegistrarRotasPush(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtPost then
    Push(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure RegistrarRotasPerfil(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtPut then
    EditarUsuario(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure InserirUsuario(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vNome, vEmail, vSenha: String;
  vCodUsuario: Integer;
  vDMGlobal: TDmGlobal;
  vBody: System.JSON.TJSONValue;
  vJson: TJSONObject;
begin
  vDMGlobal := TDMGlobal.Create(nil);
  try
    try
      vBody  := ParseBody(Params.RawBody.AsString);
      vNome  := vBody.GetValue<string>('nome', '');
      vEmail := vBody.GetValue<string>('email', '');
      vSenha := vBody.GetValue<string>('senha', '');
      FreeAndNil(vBody);

      vJson := vDmGlobal.fInserirUsuario(vNome, vEmail, vSenha);

      vCodUsuario := vJson.GetValue<Integer>('cod_usuario',0);
      //gero o token contendo o cod do usuario
      vJson.AddPair('token',fCriarToken(vCodUsuario));

      Result := vJson.ToJSON;
      FreeAndNil(vJson);
      StatusCode := 201;

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := e.Message;
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;

procedure Login(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vEmail, vSenha: String;
  vCodUsuario: Integer;
  vDMGlobal: TDmGlobal;
  vBody: System.JSON.TJSONValue;
  vJson: TJSONObject;
begin
  vDMGlobal := TDMGlobal.Create(nil);
  try
    try
      vBody  := ParseBody(Params.RawBody.AsString);
      vEmail := vBody.GetValue<string>('email', '');
      vSenha := vBody.GetValue<string>('senha', '');
      FreeAndNil(vBody);

      vJson := vDmGlobal.fLogin(vEmail, vSenha);
      if vJson.Size = 0 then
      begin
        StatusCode := 401;
        Result := CreateJsonObjStr('erro', 'E-mail ou senha inválida');
      end
      else
      begin
        vCodUsuario := vJson.GetValue<Integer>('cod_usuario',0);
        //gero o token contendo o cod do usuario
        vJson.AddPair('token',fCriarToken(vCodUsuario));

        StatusCode := 200;
        Result := vJson.ToJSON;
      end;

      FreeAndNil(vJson);

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := e.Message;
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;

procedure Push(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vTokenPush  : String;
  vCodUsuario : Integer;
  vDMGlobal   : TDmGlobal;
  vBody       : System.JSON.TJSONValue;
begin
  vDMGlobal := TDMGlobal.Create(nil);
  try
    try
      vBody  := ParseBody(Params.RawBody.AsString);
      vCodUsuario := fGetUsuarioRequest();
      vTokenPush  := vBody.GetValue<string>('token_push', '');
      FreeAndNil(vBody);

      vDmGlobal.fPush(vCodUsuario, vTokenPush);

      Result := CreateJsonObjStr('id_usuario', vCodUsuario);
      StatusCode := 200;

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := e.Message;
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;

procedure EditarUsuario(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vNome, vEmail  : String;
  vCodUsuario : Integer;
  vDMGlobal   : TDmGlobal;
  vBody       : System.JSON.TJSONValue;
  vJson       : TJSONObject;
begin
  vDMGlobal := TDMGlobal.Create(nil);
  try
    try
      vCodUsuario := fGetUsuarioRequest();

      vBody  := ParseBody(Params.RawBody.AsString);
      vNome  := vBody.GetValue<string>('nome','');
      vEmail  := vBody.GetValue<string>('email','');
      FreeAndNil(vBody);

      vJson := vDmGlobal.fEditarUsuario(vCodUsuario, vNome, vEmail);

      Result := vJson.ToJSON;
      FreeAndNil(vJson);
      StatusCode := 200;

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := e.Message;
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;


end.
