program Servidor;

uses
  System.StartUpCopy,
  FMX.Forms,
  DataModule.Services in 'Servidor\DataModule\DataModule.Services.pas' {DMServices: TDataModule},
  UnitPrincipal in 'Servidor\UnitPrincipal.pas' {frmPrincipal},
  DataModule.Global in 'Servidor\DataModule\DataModule.Global.pas' {DmGlobal: TDataModule},
  Controllers.Usuario in 'Controller\Controllers.Usuario.pas',
  uFunctions in 'Units\uFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDMServices, DMServices);
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
