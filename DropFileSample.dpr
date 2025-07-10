program DropFileSample;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  DropFile in 'DropFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
