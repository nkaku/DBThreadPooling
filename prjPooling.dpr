program prjPooling;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainF in 'MainF.pas' {Form2},
  untPool in 'untPool.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
