unit EditMain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, EditPsw,
  EditUsag, EditPref, ExtCtrls, CfgRec,
  RecDif, ToolEdit, ComCtrls, RXSpin, DateUtil, Global, Jdates,
  {$IFDEF WITH_FULL}
    Limit_U,
    Support,
  {$ENDIF}
  RAL, FileRout;
  
type
  TUseredit = class(TForm)
    Personal: TGroupBox;
    MaskEdit1: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    MaskEdit2: TMaskEdit;
    Label3: TLabel;
    MaskEdit3: TMaskEdit;
    Label4: TLabel;
    Button1: TButton;
    Label5: TLabel;
    Label6: TLabel;
    MaskEdit4: TMaskEdit;
    MaskEdit5: TMaskEdit;
    Label7: TLabel;
    ComboBox1: TComboBox;
    Bevel1: TBevel;
    Label8: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label9: TLabel;
    Edit4: TEdit;
    Label10: TLabel;
    Edit5: TEdit;
    Label15: TLabel;
    Edit6: TEdit;
    Label16: TLabel;
    Button6: TButton;
    Button7: TButton;
    Button3: TButton;
    Button4: TButton;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Label11: TLabel;
    Label12: TLabel;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    Label13: TLabel;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    Label14: TLabel;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    CheckBox32: TCheckBox;
    DateEdit1: TDateEdit;
    RxSpinEdit1: TRxSpinEdit;
    RxSpinEdit2: TRxSpinEdit;
    RxSpinEdit3: TRxSpinEdit;
    RxSpinEdit4: TRxSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    SaveInf: UsersRecord;
  public
    { Public declarations }
    UserInf: UsersRecord;
    CurOnline: Boolean;
    CurTimeLimit: Word;
  end;

var
    UserEdit: TUserEdit;

implementation

{$R *.DFM}

uses SysSet, BitWise;

procedure TUseredit.Button1Click(Sender: TObject);
begin
   Password.ShowModal;
end;

procedure TUseredit.Button6Click(Sender: TObject);
begin
  Usage.ShowModal;
end;

procedure TUseredit.Button7Click(Sender: TObject);
begin
  Preferences.ShowModal;
end;


procedure TUseredit.FormActivate(Sender: TObject);
begin
  SaveInf := UserInf;
  ShortDateFormat := 'mm/dd/yy';
  DateSeparator := '-';
   
  MaskEdit1.Text := UserInf.Name;
  MaskEdit2.Text := UserInf.Handle;
  MaskEdit3.Text := UserInf.Location;
  MaskEdit4.Text := UserInf.VoicePhone;
  MaskEdit5.Text := UserInf.DataPhone;
  ComboBox1.ItemIndex := Userinf.Sex;
  Edit1.Text     := UserInf.Address1;
  Edit2.Text     := UserInf.Address2;
  Edit3.Text     := UserInf.Address3;
  Edit4.Text     := UserInf.ForwardTo;
  Edit5.Text     := UserInf.Organisation;
  Edit6.Text     := UserInf.Comment;

  RxSpinEdit1.Text := IntToStr(Userinf.Security);
  RxSpinEdit2.Text := IntToStr(Userinf.Credit);
  RxSpinEdit3.Text := IntToStr(Userinf.Pending);
  RxSpinEdit4.Text := IntToStr(Userinf.Group);

  try
    DateEdit1.Text := UserInf.BirthDate;
  except
  end;

  CheckBox1.Checked := ReadBit(Userinf.Flags[1], 0);
  CheckBox2.Checked := ReadBit(Userinf.Flags[1], 1);
  CheckBox3.Checked := ReadBit(Userinf.Flags[1], 2);
  CheckBox4.Checked := ReadBit(Userinf.Flags[1], 3);
  CheckBox5.Checked := ReadBit(Userinf.Flags[1], 4);
  CheckBox6.Checked := ReadBit(Userinf.Flags[1], 5);
  CheckBox7.Checked := ReadBit(Userinf.Flags[1], 6);
  CheckBox8.Checked := ReadBit(Userinf.Flags[1], 7);

  CheckBox9.Checked := ReadBit(Userinf.Flags[2], 0);
  CheckBox10.Checked := ReadBit(Userinf.Flags[2], 1);
  CheckBox11.Checked := ReadBit(Userinf.Flags[2], 2);
  CheckBox12.Checked := ReadBit(Userinf.Flags[2], 3);
  CheckBox13.Checked := ReadBit(Userinf.Flags[2], 4);
  CheckBox14.Checked := ReadBit(Userinf.Flags[2], 5);
  CheckBox15.Checked := ReadBit(Userinf.Flags[2], 6);
  CheckBox16.Checked := ReadBit(Userinf.Flags[2], 7);

  CheckBox17.Checked := ReadBit(Userinf.Flags[3], 0);
  CheckBox18.Checked := ReadBit(Userinf.Flags[3], 1);
  CheckBox19.Checked := ReadBit(Userinf.Flags[3], 2);
  CheckBox20.Checked := ReadBit(Userinf.Flags[3], 3);
  CheckBox21.Checked := ReadBit(Userinf.Flags[3], 4);
  CheckBox22.Checked := ReadBit(Userinf.Flags[3], 5);
  CheckBox23.Checked := ReadBit(Userinf.Flags[3], 6);
  CheckBox24.Checked := ReadBit(Userinf.Flags[3], 7);

  CheckBox25.Checked := ReadBit(Userinf.Flags[4], 0);
  CheckBox26.Checked := ReadBit(Userinf.Flags[4], 1);
  CheckBox27.Checked := ReadBit(Userinf.Flags[4], 2);
  CheckBox28.Checked := ReadBit(Userinf.Flags[4], 3);
  CheckBox29.Checked := ReadBit(Userinf.Flags[4], 4);
  CheckBox30.Checked := ReadBit(Userinf.Flags[4], 5);
  CheckBox31.Checked := ReadBit(Userinf.Flags[4], 6);
  CheckBox32.Checked := ReadBit(Userinf.Flags[4], 7);

end;

procedure CheckBit(CheckBox: TCheckBox; var Bits: Byte; Typ: Byte);
begin
  if CheckBox.Checked then SetBit(Bits, Typ)
    else ClearBit(Bits, Typ);
end; { proc. CheckBit }

function FixDateStr(S: String): String;
begin
  S[03] := '-';
  S[06] := '-';

  if Length(S) > 8 then SetLength(S, 8);
  Result := s;
end; { func. FixDateStr }

procedure TUseredit.FormClose(Sender: TObject; var Action: TCloseAction);
var Temp: Word;
begin
  UserInf.Name         := MaskEdit1.Text;
  UserInf.Handle       := MaskEdit2.Text;
  UserInf.Location     := MaskEdit3.Text;
  UserInf.VoicePhone   := MaskEdit4.Text;
  UserInf.DataPhone    := MaskEdit5.Text;
  UserInf.Sex          := ComboBox1.ItemIndex;
  UserInf.Address1     := Edit1.Text;
  UserInf.Address2     := Edit2.Text;
  UserInf.Address3     := Edit3.Text;
  UserInf.ForwardTo    := Edit4.Text;
  UserInf.Organisation := Edit5.Text;
  UserInf.Comment      := Edit6.Text;

  UserInf.Security     := SmallWord(StrToInt(RxSpinEdit1.Text));
  UserInf.Credit       := Longint(StrToInt(RxSpinEdit2.Text));
  UserInf.Pending      := Longint(StrToInt(RxSpinEdit3.Text));
  UserInf.Group        := SmallWord(StrToInt(RxSpinEdit4.Text));

  UserInf.BirthDate    := FixDateStr(DateEdit1.Text);
    
  CheckBit(CheckBox1, Userinf.Flags[1], 0);
  CheckBit(CheckBox2, Userinf.Flags[1], 1);
  CheckBit(CheckBox3, Userinf.Flags[1], 2);
  CheckBit(CheckBox4, Userinf.Flags[1], 3);
  CheckBit(CheckBox5, Userinf.Flags[1], 4);
  CheckBit(CheckBox6, Userinf.Flags[1], 5);
  CheckBit(CheckBox7, Userinf.Flags[1], 6);
  CheckBit(CheckBox8, Userinf.Flags[1], 7);

  CheckBit(CheckBox9, Userinf.Flags[2], 0);
  CheckBit(CheckBox10, Userinf.Flags[2], 1);
  CheckBit(CheckBox11, Userinf.Flags[2], 2);
  CheckBit(CheckBox12, Userinf.Flags[2], 3);
  CheckBit(CheckBox13, Userinf.Flags[2], 4);
  CheckBit(CheckBox14, Userinf.Flags[2], 5);
  CheckBit(CheckBox15, Userinf.Flags[2], 6);
  CheckBit(CheckBox16, Userinf.Flags[2], 7);

  CheckBit(CheckBox17, Userinf.Flags[3], 0);
  CheckBit(CheckBox18, Userinf.Flags[3], 1);
  CheckBit(CheckBox19, Userinf.Flags[3], 2);
  CheckBit(CheckBox20, Userinf.Flags[3], 3);
  CheckBit(CheckBox21, Userinf.Flags[3], 4);
  CheckBit(CheckBox22, Userinf.Flags[3], 5);
  CheckBit(CheckBox23, Userinf.Flags[3], 6);
  CheckBit(CheckBox24, Userinf.Flags[3], 7);

  CheckBit(CheckBox25, Userinf.Flags[4], 0);
  CheckBit(CheckBox26, Userinf.Flags[4], 1);
  CheckBit(CheckBox27, Userinf.Flags[4], 2);
  CheckBit(CheckBox28, Userinf.Flags[4], 3);
  CheckBit(CheckBox29, Userinf.Flags[4], 4);
  CheckBit(CheckBox30, Userinf.Flags[4], 5);
  CheckBit(CheckBox31, Userinf.Flags[4], 6);
  CheckBit(CheckBox32, Userinf.Flags[4], 7);

 If RecordsDifferent(UserInf, SaveInf, SizeOf(UsersRecord)) then
  Temp := MessageDlg('Save made changes?',
                     mtInformation, mbYesNoCancel,
                     0)
                      else Temp := mrYes;

  If Temp=mrCancel then
     begin;
       Action := caNone;          { Abort exit }
       EXIT;
     end; { Cancel }


  If Temp=mrNo then
    begin
      UserInf := SaveInf;
    end; { no }

  if CurOnline then
    begin
      {$IFDEF WITH_FULL}
        Limit_U.SetTime(Exitinfo^.TimeLimit);
      {$ENDIF}

      AnsiOn := ReadBit(Exitinfo^.UserInfo.Attribute, 3);
      AvatarOn := ReadBit(Exitinfo^.UserInfo.Attribute2, 1);
      GuestUser := ReadBit(Exitinfo^.Userinfo.Attribute2, 6);
      if RipOn then AnsiOn := True;
      UserAgeSave := GetUserAge;   { Forces EleBBS to recalculate the user's age }

      {$IFDEF WITH_FULL}
        ReadLanguageRA(Byte(Exitinfo^.Userinfo.Language - 01));
        if NOT ReadRalFile(Language^.DefName) then
          LoadDefaultLanguage;
      {$ENDIF}
    end; { if }
end;

procedure TUseredit.Button3Click(Sender: TObject);
begin
  SystemSetForm.ShowModal;
end;

procedure TUseredit.Button4Click(Sender: TObject);
begin
  Close;
end;

end.
