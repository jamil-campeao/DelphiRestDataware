unit UnitPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, uRESTDWComponentBase, uRESTDWBasic, uRESTDWIdBase,
  uRESTDWDataUtils;

type
  TfrmPrincipal = class(TForm)
    Switch: TSwitch;
    Label1: TLabel;
    ServicePooler: TRESTDWIdServicePooler;
    cbLerIni: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SwitchSwitch(Sender: TObject);
    procedure cbLerIniChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
  aLerIni: Boolean;
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.fmx}

uses DataModule.Services, Controllers.Auth;

procedure TfrmPrincipal.cbLerIniChange(Sender: TObject);
begin
  if cbLerIni.IsChecked then
    aLerIni := True
  else
    aLerIni := False;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  ServicePooler.ServerMethodClass := TDMServices;
  ServicePooler.Active := Switch.IsChecked;

  //Configurações JWT
  with ServicePooler do
  begin
    AuthenticationOptions.AuthorizationOption := rdwAOBearer;
    TRESTDWAuthOptionBearerServer(AuthenticationOptions.OptionParams).TokenType       := rdwJWT;
    TRESTDWAuthOptionBearerServer(AuthenticationOptions.OptionParams).TokenHash       := Controllers.Auth.cSecret;
    TRESTDWAuthOptionBearerServer(AuthenticationOptions.OptionParams).ServerSignature := '';
    TRESTDWAuthOptionBearerServer(AuthenticationOptions.OptionParams).LifeCycle       := 0;
  end;
end;

procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  if cbLerIni.IsChecked then
    aLerIni := True
  else
    aLerIni := False;
end;

procedure TfrmPrincipal.SwitchSwitch(Sender: TObject);
begin
  ServicePooler.Active := Switch.IsChecked;
end;

end.
