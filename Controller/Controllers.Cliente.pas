unit Controllers.Cliente;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions, Controllers.Auth;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure ListarClientes(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure InserirEditarCliente(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

implementation

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtGet then
    ListarClientes(Params, Result, RequestType, StatusCode, RequestHeader)
  else
  if RequestType = rtPost then
    InserirEditarCliente(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure ListarClientes(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vDMGlobal   : TDmGlobal;
  vJson       : TJsonArray;
  vDtUltSinc  : String;
  vPagina     : Integer;
begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      try
        vDtUltSinc := Params.ItemsString['dt_ult_sincronizacao'].AsString;
      except
        vDtUltSinc := '';
      end;

      try
        vPagina := Params.ItemsString['pagina'].AsInteger;
      except
        vPagina := 1;
      end;

      vJson := vDmGlobal.fListarClientes(vDtUltSinc, vPagina);

      Result := vJson.ToJson;
      StatusCode := 200;
      FreeAndNil(vJSon);

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

procedure InserirEditarCliente(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vCodUsuario: Integer;
  vDMGlobal: TDmGlobal;
  vBody: System.JSON.TJSONValue;
  vJson: TJSONObject;
begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      vCodUsuario := fGetUsuarioRequest;

      vBody  := ParseBody(Params.RawBody.AsString);

      vJson := vDmGlobal.fInserirEditarCliente(vCodUsuario,
                                               vBody.GetValue<integer>('cod_cliente_local',0),
                                               vBody.GetValue<string>('cnpj_cpf',''),
                                               vBody.GetValue<string>('nome',''),
                                               vBody.GetValue<string>('fone',''),
                                               vBody.GetValue<string>('email',''),
                                               vBody.GetValue<string>('endereco',''),
                                               vBody.GetValue<string>('numero',''),
                                               vBody.GetValue<string>('complemento',''),
                                               vBody.GetValue<string>('bairro',''),
                                               vBody.GetValue<string>('cidade',''),
                                               vBody.GetValue<string>('uf',''),
                                               vBody.GetValue<string>('cep',''),
                                               vBody.GetValue<double>('latitude',0),
                                               vBody.GetValue<double>('longitude',0),
                                               vBody.GetValue<integer>('limite_disponivel',0),
                                               vBody.GetValue<integer>('cod_cliente_oficial',0),
                                               vBody.GetValue<string>('dt_ult_sincronizacao','')
                                              );
      vJson.AddPair('cod_cliente_local', TJSONNumber.Create(vBody.GetValue<integer>('cod_cliente_local',0)));

      Result := vJson.ToJSON;
      FreeAndNil(vJson);
      FreeAndNil(vBody);
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



end.
