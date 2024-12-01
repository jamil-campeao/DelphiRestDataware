unit DataModule.Services;

interface

uses
  System.SysUtils, System.Classes, uRESTDWDatamodule;

type
  TDMServices = class(TServerMethodDataModule)
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

end.
