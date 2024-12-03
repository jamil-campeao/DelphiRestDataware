unit Controllers.Pedido;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions, Controllers.Auth,
FMX.Graphics;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure InserirEditarPedido(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure ListarPedidos(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

implementation

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtGet then
    ListarPedidos(Params, Result, RequestType, StatusCode, RequestHeader)
  else
  if RequestType = rtPost then
    InserirEditarPedido(Params, Result, RequestType, StatusCode, RequestHeader);

end;


procedure ListarPedidos(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vDMGlobal   : TDmGlobal;
  vJson       : TJsonArray;
  vDtUltSinc  : String;
  vPagina     : Integer;
  vCodUsuario : Integer;

begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      vCodUsuario := fGetUsuarioRequest();
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

      vJson := vDmGlobal.fListarPedidos(vDtUltSinc, vPagina, vCodUsuario);

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

procedure InserirEditarPedido(var Params: TRESTDWParams; var Result: string;
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

      vJson := vDmGlobal.fInserirEditarProduto(vCodUsuario,
                                               vBody.GetValue<integer>('cod_produto_local',0),
                                               vBody.GetValue<string>('descricao',''),
                                               vBody.GetValue<double>('valor',0),
                                               vBody.GetValue<double>('qtd_estoque',0),
                                               vBody.GetValue<integer>('cod_produto_oficial',0),
                                               vBody.GetValue<string>('dt_ult_sincronizacao','')
                                              );
      vJson.AddPair('cod_produto_local', TJSONNumber.Create(vBody.GetValue<integer>('cod_produto_local',0)));

      Result := vJson.ToJSON;
      FreeAndNil(vJson);
      FreeAndNil(vBody);
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
