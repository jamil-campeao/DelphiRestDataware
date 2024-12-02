unit DataModule.Services;

interface

uses
  System.SysUtils, System.Classes, uRESTDWDatamodule, uRESTDWComponentBase,
  uRESTDWServerEvents, uRESTDWParams, uRESTDWConsts, DataSet.Serialize.Config;

type
  TDMServices = class(TServerMethodDataModule)
    ServerEvents: TRESTDWServerEvents;
    procedure ServerEventsEventsusuariosReplyEventByType(
      var Params: TRESTDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    procedure ServerMethodDataModuleCreate(Sender: TObject);
    procedure ServerEventsEventsloginReplyEventByType(var Params: TRESTDWParams;
      var Result: string; const RequestType: TRequestType;
      var StatusCode: Integer; RequestHeader: TStringList);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMServices: TDMServices;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses Controllers.Usuario;

{$R *.dfm}

procedure TDMServices.ServerEventsEventsloginReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  //POST: /USUARIOS/LOGIN
  Controllers.Usuario.RegistrarRotasLogin(Params, Result, RequestType, StatusCode, RequestHeader);
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

end.
