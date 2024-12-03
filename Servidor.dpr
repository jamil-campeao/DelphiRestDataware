program Servidor;

uses
  System.StartUpCopy,
  FMX.Forms,
  DataModule.Services in 'Servidor\DataModule\DataModule.Services.pas' {DMServices: TDataModule},
  UnitPrincipal in 'Servidor\UnitPrincipal.pas' {frmPrincipal},
  DataModule.Global in 'Servidor\DataModule\DataModule.Global.pas' {DmGlobal: TDataModule},
  Controllers.Usuario in 'Controller\Controllers.Usuario.pas',
  uFunctions in 'Units\uFunctions.pas',
  Controllers.Auth in 'Controller\Controllers.Auth.pas',
  uMD5 in 'Units\uMD5.pas',
  Controllers.Notificacao in 'Controller\Controllers.Notificacao.pas',
  Controllers.CondPagto in 'Controller\Controllers.CondPagto.pas',
  Controllers.Cliente in 'Controller\Controllers.Cliente.pas',
  Controllers.Produto in 'Controller\Controllers.Produto.pas',
  Controllers.Pedido in 'Controller\Controllers.Pedido.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDMServices, DMServices);
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
