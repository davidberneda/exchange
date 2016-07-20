program Array_Exchange_Test;

uses
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Unit35 in 'Unit35.pas' {FormExchangeTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormExchangeTest, FormExchangeTest);
  Application.Run;
end.
