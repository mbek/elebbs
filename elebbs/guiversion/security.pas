unit Security;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin;

type
  TUserLevel = class(TForm)
    GroupBox1: TGroupBox;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserLevel: TUserLevel;

implementation

{$R *.DFM}

procedure TUserLevel.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TUserLevel.SpinEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key=VK_RETURN then Button1Click(Sender);
end;

end.
