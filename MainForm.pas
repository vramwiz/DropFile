unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,DropFile, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    FDrop1          : TDropFile;
    FDrop2          : TDropFile;
    procedure OnSelfDropFile1(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>);
    procedure OnSelfDropFile2(Sender: TObject; Control: TWinControl; const FileNames: TArray<string>);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDrop1 := TDropFile.Create;
  FDrop1.OnDropReceived  := OnSelfDropFile1;
  FDrop1.Attach(ListBox1);
  FDrop2 := TDropFile.Create;
  FDrop2.OnDropReceived  := OnSelfDropFile2;
  FDrop2.Attach(ListBox2);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FDrop2.Detach(ListBox2);
  FDrop2.Free;
  FDrop1.Detach(ListBox1);
  FDrop1.Free;
end;

procedure TFormMain.OnSelfDropFile1(Sender: TObject; Control: TWinControl;
  const FileNames: TArray<string>);
var
  s : string;
begin
  ListBox1.Clear;
  for s in FileNames do
    ListBox1.Items.Add(s);

end;

procedure TFormMain.OnSelfDropFile2(Sender: TObject; Control: TWinControl;
  const FileNames: TArray<string>);
var
  s : string;
begin
  ListBox2.Clear;
  for s in FileNames do
    ListBox2.Items.Add(s);

end;

end.
