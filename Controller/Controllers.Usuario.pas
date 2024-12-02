unit Controllers.Usuario;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure RegistrarRotasLogin(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure InserirUsuario(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure Login(var Params: TRESTDWParams; var Result: string;
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

      vJson := vDmGlobal.InserirUsuario(vNome, vEmail, vSenha);

      Result := vJson.ToJSON;
      FreeAndNil(vJson);
      StatusCode := 201;

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := CreateJsonObjStr('erro', e.Message);
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

      vJson := vDmGlobal.Login(vEmail, vSenha);
      if vJson.Size = 0 then
      begin
        StatusCode := 401;
        Result := CreateJsonObjStr('erro', 'E-mail ou senha inválida');
      end
      else
      begin
        Result := vJson.ToJSON;
        StatusCode := 200;
      end;

      FreeAndNil(vJson);

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := CreateJsonObjStr('erro', e.Message);
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;

end.
