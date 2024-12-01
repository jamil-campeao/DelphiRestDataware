unit UnitPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, uRESTDWComponentBase, uRESTDWBasic, uRESTDWIdBase;

type
  TfrmPrincipal = class(TForm)
    Switch: TSwitch;
    Label1: TLabel;
    ServicePooler: TRESTDWIdServicePooler;
    procedure FormCreate(Sender: TObject);
    procedure SwitchSwitch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.fmx}

uses DataModule.Services;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  ServicePooler.ServerMethodClass := TDMServices;
  ServicePooler.Active := Switch.IsChecked;
end;

procedure TfrmPrincipal.SwitchSwitch(Sender: TObject);
begin
  ServicePooler.Active := Switch.IsChecked;
end;

end.
