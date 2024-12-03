unit Controllers.Produto;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions, Controllers.Auth,
FMX.Graphics;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure RegistrarRotasFoto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

<<<<<<< HEAD
procedure RegistrarRotasFotoStream(const Params: TRESTDWParams; var ContentType: string;
  const Result: TMemoryStream; const RequestType: TRequestType;
  var StatusCode: Integer);

=======
>>>>>>> 5c055714880672f08bbcb77e5d930c712f0ea39b
procedure ListarProdutos(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure InserirEditarProduto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure EditarFoto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

<<<<<<< HEAD
procedure ListarFoto(const Params: TRESTDWParams; var ContentType: string;
  const Result: TMemoryStream; const RequestType: TRequestType;
  var StatusCode: Integer);

=======
>>>>>>> 5c055714880672f08bbcb77e5d930c712f0ea39b
implementation


procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtGet then
    ListarProdutos(Params, Result, RequestType, StatusCode, RequestHeader)
  else
  if RequestType = rtPost then
    InserirEditarProduto(Params, Result, RequestType, StatusCode, RequestHeader);

end;

procedure RegistrarRotasFoto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtPut then
    EditarFoto(Params, Result, RequestType, StatusCode, RequestHeader)
  end;

<<<<<<< HEAD
=======

>>>>>>> 5c055714880672f08bbcb77e5d930c712f0ea39b
procedure ListarProdutos(var Params: TRESTDWParams; var Result: string;
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

      vJson := vDmGlobal.fListarProdutos(vDtUltSinc, vPagina);

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

procedure InserirEditarProduto(var Params: TRESTDWParams; var Result: string;
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

procedure EditarFoto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vCodProduto: Integer;
  vDMGlobal: TDmGlobal;
  vBody: System.JSON.TJSONValue;
  vFoto : TBitmap;
begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      try
        vCodProduto := Params.ItemsString['0'].AsInteger;
      except
        vCodProduto := 0;
      end;

      vBody := ParseBody(Params.RawBody.AsString);
      if vBody.GetValue<string>('foto64','') = '' then
      begin
        raise Exception.Create('Parâmetro foto64 não informado');
      end;

      vFoto := BitmapFromBase64(vBody.GetValue<string>('foto64',''));
      vDmGlobal.EditarFoto(vCodProduto, vFoto);

      Result := 'Foto alterada com sucesso';
      StatusCode := 200;

    except on e:Exception do
      begin
        StatusCode := 500;
        Result := e.Message;
      end;

    end;

  finally
    FreeAndNil(vDMGlobal);
    FreeAndNil(vBody);
    FreeAndNil(vFoto);

  end;
end;
<<<<<<< HEAD

procedure RegistrarRotasFotoStream(const Params: TRESTDWParams; var ContentType: string;
  const Result: TMemoryStream; const RequestType: TRequestType;
  var StatusCode: Integer);
begin
  if RequestType = rtGet then
    ListarFoto(Params, ContentType, Result, RequestType, StatusCode);
end;

procedure ListarFoto(const Params: TRESTDWParams; var ContentType: string;
  const Result: TMemoryStream; const RequestType: TRequestType;
  var StatusCode: Integer);
var
  vCodProduto : Integer;
  vDMGlobal   : TDMGlobal;
  vFotoStream : TStream;
begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      try
        vCodProduto := Params.ItemsString['0'].AsInteger;
      except
        vCodProduto := 0;
      end;

      vFotoStream := vDmGlobal.fListarFoto(vCodProduto);

      Result.LoadFromStream(vFotoStream);
      StatusCode := 200;
      FreeAndNil(vFotoStream);

    except on e:Exception do
      StatusCode := 500;

    end;

  finally
    FreeAndNil(vDMGlobal);

  end;
end;

=======
>>>>>>> 5c055714880672f08bbcb77e5d930c712f0ea39b

end.
