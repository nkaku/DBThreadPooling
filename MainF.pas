unit MainF;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, Data.DB, FMX.Layouts, FMX.ListBox, FireDAC.Comp.Client,
  untPool;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Button2: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterRun(ST: TSQLType; Q: TFDQuery);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  s: string;
begin
  s := 'select * from users where UID = ' + Random(531).ToString;
  RunSQL(s, stOpen, AfterRun);

  s := 'insert into users (username) values ('+QuotedStr(Random(99999).ToString)+')';
  RunSQL(s, stExecute, AfterRun);

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TForm2.AfterRun(ST: TSQLType; Q: TFDQuery);
begin
  if ST = stExecute then
    ShowMessage(Q.RowsAffected.ToString)
  else
    ShowMessage(Q.FieldByName('username').AsString);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  s := 'insert into users (username) values ('+QuotedStr(Random(99999).ToString)+')';
  RunSQL(s, stExecute);
end;

end.
