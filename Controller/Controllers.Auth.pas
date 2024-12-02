unit Controllers.Auth;

interface

uses System.SysUtils, System.JSON, uRESTDWDataUtils;

const
  cSecret = 'PASS@123';

var
  vID_USUARIO: Integer;

function fCriarToken(pIdUsuario: Integer): String;
function fGetUsuarioRequest: Integer;

implementation

function fCriarToken(pIdUsuario: Integer): String;
var
  vTokenValue: TTokenValue;
  vJson      : TJSONValue;
begin
  vTokenValue := TTokenValue.Create;
  vTokenValue.TokenHash := cSecret;
  vTokenValue.TokenType := rdwJWT;
  vTokenValue.CryptType := rdwAES256;
  vTokenValue.Secrets   := pIdUsuario.ToString;
//  vTokenValue.BeginTime := now + 30;

  try
    vJson := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(vTokenValue.Token), 0) as TJSONValue;
    Result := vJson.GetValue<string>('token','');
    vJson.Free;
  finally
    FreeAndNil(vTokenValue);

  end;

end;

function fGetUsuarioRequest: Integer;
begin
  Result := vID_USUARIO;
end;

end.
