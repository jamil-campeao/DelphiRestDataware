unit uFunctions;

interface

uses System.JSON, System.SysUtils, System.NetEncoding, FMX.Graphics, System.Classes;

function BitmapFromBase64(const base64: string): TBitmap;
function CreateJsonObj(pairName: string; value: string): TJSONObject; overload;
function CreateJsonObj(pairName: string; value: integer): TJSONObject; overload;
function CreateJsonObj(pairName: string; value: double): TJSONObject; overload;
function CreateJsonObjStr(pairName: string; value: string): string; overload;
function CreateJsonObjStr(pairName: string; value: integer): string; overload;
function CreateJsonObjStr(pairName: string; value: double): string; overload;
function ParseBody(body: string): TJsonValue;


implementation

function BitmapFromBase64(const base64: string): TBitmap;
var
  Input: TStringStream;
  Output: TBytesStream;
  Encoding: TBase64Encoding;
begin
  Input := TStringStream.Create(base64, TEncoding.UTF8);
  try
    Output := TBytesStream.Create;
    try
      Encoding := TBase64Encoding.Create(0);
      Encoding.Decode(Input, Output);

      Output.Position := 0;
      Result := TBitmap.Create;
      try
        Result.LoadFromStream(Output);
      except
        Result.Free;
        raise;
      end;
    finally
      Encoding.DisposeOf;
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

function CreateJsonObj(pairName: string; value: string): TJSONObject;
begin
    Result := TJSONObject.Create(TJSONPair.Create(pairName, value));
end;

function CreateJsonObj(pairName: string; value: integer): TJSONObject;
begin
    Result := TJSONObject.Create(TJSONPair.Create(pairName, TJSONNumber.Create(value)));
end;

function CreateJsonObj(pairName: string; value: double): TJSONObject;
begin
    Result := TJSONObject.Create(TJSONPair.Create(pairName, TJSONNumber.Create(value)));
end;

function ParseBody(body: string): TJsonValue;
begin
    Result := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(body), 0) as TJsonValue;
end;

function CreateJsonObjStr(pairName: string; value: string): string;
var
    json: TJSONObject;
begin
    try
        json := TJSONObject.Create;
        json.AddPair(pairname, value);
        Result := json.ToJSON;
    finally
        json.free;
    end;
end;

function CreateJsonObjStr(pairName: string; value: integer): string;
var
    json: TJSONObject;
begin
    try
        json := TJSONObject.Create;
        json.AddPair(pairname, TJSONNumber.Create(value));
        Result := json.ToJSON;
    finally
        json.free;
    end;
end;

function CreateJsonObjStr(pairName: string; value: double): string;
var
    json: TJSONObject;
begin
    try
        json := TJSONObject.Create;
        json.AddPair(pairname, TJSONNumber.Create(value));
        Result := json.ToJSON;
    finally
        json.free;
    end;
end;


end.
