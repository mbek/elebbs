unit EditPsw;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Crc_Unit;

type
  TPassword = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Password: TPassword;

implementation

{$R *.DFM}

uses EditMain;

procedure TPassword.FormActivate(Sender: TObject);
begin
   Edit1.Text := UserEdit.UserInf.Password;
   Edit1.ReadOnly := True;
   If Edit1.Text='' then Edit1.Text := '[Not visible]';

   Edit2.Text := UserEdit.Userinf.Password;
end;

procedure TPassword.Button2Click(Sender: TObject);
begin
 Close;
end;

procedure TPassword.Button1Click(Sender: TObject);
begin
  UserEdit.Userinf.Password := Edit2.Text;
  UserEdit.UserInf.PasswordCRC := RaCrc(SupCase(Edit2.Text));

  Close;
end;

end.
