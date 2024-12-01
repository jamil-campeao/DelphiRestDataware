program Servidor;

uses
  System.StartUpCopy,
  FMX.Forms,
  DataModule.Services in 'Servidor\DataModule\DataModule.Services.pas' {DMServices: TDataModule},
  UnitPrincipal in 'Servidor\UnitPrincipal.pas' {frmPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDMServices, DMServices);
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
