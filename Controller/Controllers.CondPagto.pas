unit Controllers.CondPagto;

interface

uses System.JSON, uRESTDWConsts, uRESTDWParams, System.Classes,
System.SysUtils, DataModule.Global, uFunctions, Controllers.Auth;

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

procedure ListarCondPagto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);

implementation

procedure RegistrarRotas(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  if RequestType = rtGet then
    ListarCondPagto(Params, Result, RequestType, StatusCode, RequestHeader);
end;

procedure ListarCondPagto(var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
var
  vDMGlobal   : TDmGlobal;
  vJson       : TJsonArray;
begin
  try
    try
      vDMGlobal := TDMGlobal.Create(nil);
      vJson := vDmGlobal.fListarCondPagto;

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

end.
