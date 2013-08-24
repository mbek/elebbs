unit SysSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, RXSpin, ToolEdit;

type
  TSystemSetForm = class(TForm)
    SystemBox: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    MaskEdit11: TMaskEdit;
    Button1: TButton;
    Button2: TButton;
    RxSpinEdit1: TRxSpinEdit;
    RxSpinEdit2: TRxSpinEdit;
    RxSpinEdit3: TRxSpinEdit;
    RxSpinEdit4: TRxSpinEdit;
    RxSpinEdit5: TRxSpinEdit;
    DateEdit1: TDateEdit;
    DateEdit2: TDateEdit;
    DateEdit3: TDateEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SystemSetForm: TSystemSetForm;

implementation

uses EditMain;

{$R *.DFM}

procedure TSystemSetForm.FormActivate(Sender: TObject);
begin
  ShortDateFormat := 'm/d/y';
  DateSeparator := '-';

  If UserEdit.CurOnline then Label21.Caption := 'Time left'
    else Label21.Caption := 'Time used';

  MaskEdit11.Text := UserEdit.UserInf.LastTime;
  DateEdit1.Text := UserEdit.UserInf.LastDate;
  DateEdit3.Text := UserEdit.UserInf.FirstDate;
  DateEdit2.Text := UserEdit.UserInf.SubDate;

  If UserEdit.CurOnline then
   RxSpinEdit1.Text := IntToStr(UserEdit.CurTimeLimit)
    else RxSpinEdit1.Text := IntToStr(UserEdit.UserInf.Elapsed);


  RxSpinEdit4.Text := IntToStr(UserEdit.Userinf.LastPwdChange);
  RxSpinEdit2.Text := IntToStr(UserEdit.UserInf.LastDobCheck);
  RxSpinEdit5.Text := IntToStr(UserEdit.UserInf.LastRead);
  RxSpinEdit3.Text := IntToStr(UserEdit.UserInf.TodayK);
end;

function FixDateStr(S: String): String;
begin
  S[03] := '-';
  S[06] := '-';

  if Length(S) > 8 then SetLength(S, 8);
  Result := s;
end; { func. FixDateStr }


procedure TSystemSetForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin


  UserEdit.UserInf.LastTime     := MaskEdit11.Text;
  UserEdit.UserInf.LastDate     := FixDateStr(DateEdit1.Text);
  UserEdit.UserInf.FirstDate    := FixDateStr(DateEdit3.Text);
  UserEdit.UserInf.SubDate      := FixDateStr(DateEdit2.Text);

  If UserEdit.CurOnline then
   UserEdit.CurTimeLimit := StrToInt(RxSpinEdit1.Text)
    else UserEdit.UserInf.Elapsed := StrToInt(RxSpinEdit1.Text);

  UserEdit.UserInf.LastPwdChange:= StrToInt(RxSpinEdit4.Text);
  UserEdit.UserInf.LastDobCheck := StrToInt(RxSpinEdit2.Text);
  UserEdit.UserInf.LastRead     := StrToInt(RxSpinEdit5.Text);
  UserEdit.UserInf.TodayK       := StrToInt(RxSpinEdit3.Text);
end;

procedure TSystemSetForm.Button1Click(Sender: TObject);
begin
  SystemSetForm.Close;
end;

procedure TSystemSetForm.Button2Click(Sender: TObject);
begin
  FormActivate(Self);
  SystemSetForm.Close;
end;

end.
