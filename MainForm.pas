unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,DropFile, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    FDrop          : TDropFile;
    procedure OnSelfDropFile(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDrop := TDropFile.Create;
  FDrop.OnDropReceived  := OnSelfDropFile;
  //FDrop.Pattern := '*.*';
  FDrop.Attach(Self);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FDrop.Free;
end;

procedure TFormMain.OnSelfDropFile(Sender: TObject; Control: TWinControl;
  const FileNames: TArray<string>);
var
  s : string;
begin
  ListBox1.Clear;
  for s in FileNames do
    ListBox1.Items.Add(s);

end;

end.
