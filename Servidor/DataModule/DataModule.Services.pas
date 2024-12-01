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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMServices: TDMServices;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDMServices.ServerEventsEventsusuariosReplyEventByType(
  var Params: TRESTDWParams; var Result: string;
  const RequestType: TRequestType; var StatusCode: Integer;
  RequestHeader: TStringList);
begin
  Result := '{"retorno: "Usuário cadastrado com sucesso"}';
end;

procedure TDMServices.ServerMethodDataModuleCreate(Sender: TObject);
begin
  TDataSetSerializeConfig.GetInstance.CaseNameDefinition := cndLower;
  TDataSetSerializeConfig.GetInstance.Import.DecimalSeparator := '.';

end;

end.
