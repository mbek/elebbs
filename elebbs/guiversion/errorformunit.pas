unit ErrorFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TErrorForm = class(TForm)
    Timer1: TTimer;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ErrorForm: TErrorForm;

implementation

{$R *.DFM}

procedure TErrorForm.FormActivate(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TErrorForm.Timer1Timer(Sender: TObject);
begin
  Close;
end;

procedure TErrorForm.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
