unit Editpref;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, ExtCtrls, Buttons, LongStr, RXSpin;

type
  TPreferences = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    MaskEdit1: TMaskEdit;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    RxSpinEdit1: TRxSpinEdit;
    RxSpinEdit2: TRxSpinEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Cancel: Boolean;
  end;

var
  Preferences: TPreferences;

implementation

{$R *.DFM}

uses EditMain, BitWise;

procedure Bool2Bit(B: Boolean; var Attribute: Byte; Bit: Byte);
begin
 If b then SetBit(Attribute, bit)
  else ClearBit(Attribute, Bit);
end; { bool2bit }

procedure TPreferences.FormActivate(Sender: TObject);
begin
  Cancel := False;

  With UserEdit,Preferences do
   begin;
    MaskEdit1.Text := UserInf.DefaultProtocol;
    RxSpinEdit2.Text := FStr(UserInf.Language);
    RxSpinEdit1.Text := FStr(UserInf.ScreenLength);
    ComboBox2.ItemIndex := UserInf.DateFormat - 1;

    CheckBox1.Checked := ReadBit(UserInf.Attribute, 0);
    CheckBox2.Checked := ReadBit(UserInf.Attribute, 1);
    CheckBox3.Checked := ReadBit(UserInf.Attribute, 2);
    CheckBox4.Checked := ReadBit(UserInf.Attribute, 3);
    CheckBox5.Checked := ReadBit(UserInf.Attribute2, 1);
    CheckBox6.Checked := ReadBit(UserInf.Attribute, 4);
    CheckBox7.Checked := ReadBit(UserInf.Attribute, 5);
    CheckBox8.Checked := ReadBit(UserInf.Attribute, 6);
    CheckBox9.Checked := ReadBit(UserInf.Attribute, 7);

    CheckBox10.Checked := ReadBit(UserInf.Attribute2, 0);
    CheckBox11.Checked := ReadBit(UserInf.Attribute2, 2);
    CheckBox12.Checked := ReadBit(UserInf.Attribute2, 3);
    CheckBox13.Checked := ReadBit(UserInf.Attribute2, 4);
    CheckBox14.Checked := ReadBit(UserInf.Attribute2, 5);
    CheckBox15.Checked := ReadBit(UserInf.Attribute2, 6);
    CheckBox16.Checked := ReadBit(UserInf.Attribute2, 7);
    CheckBox17.Checked := ReadBit(UserInf.Attribute3, 0);
   end; { with useredit }
end;

procedure TPreferences.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 If NOT Cancel then
 With UserEdit,Preferences do
  begin;
   UserInf.DefaultProtocol:= MaskEdit1.Text[1];
   UserInf.Language       := FVal(RxSpinEdit2.Text);
   UserInf.ScreenLength   := FVal(RxSpinEdit1.Text);
   UserInf.DateFormat     := ComboBox2.ItemIndex + 1;

   Bool2Bit(CheckBox1.Checked, UserInf.Attribute, 0);
   Bool2Bit(CheckBox2.Checked, UserInf.Attribute, 1);
   Bool2Bit(CheckBox3.Checked, UserInf.Attribute, 2);
   Bool2Bit(CheckBox4.Checked, UserInf.Attribute, 3);
   Bool2Bit(CheckBox5.Checked, UserInf.Attribute2, 1);
   Bool2Bit(CheckBox6.Checked, UserInf.Attribute, 4);
   Bool2Bit(CheckBox7.Checked, UserInf.Attribute, 5);
   Bool2Bit(CheckBox8.Checked, UserInf.Attribute, 6);
   Bool2Bit(CheckBox9.Checked, UserInf.Attribute, 7);

   Bool2Bit(CheckBox10.Checked, UserInf.Attribute2, 0);
   Bool2Bit(CheckBox11.Checked, UserInf.Attribute2, 2);
   Bool2Bit(CheckBox12.Checked, UserInf.Attribute2, 3);
   Bool2Bit(CheckBox13.Checked, UserInf.Attribute2, 4);
   Bool2Bit(CheckBox14.Checked, UserInf.Attribute2, 5);
   Bool2Bit(CheckBox15.Checked, UserInf.Attribute2, 6);
   Bool2Bit(CheckBox16.Checked, UserInf.Attribute2, 7);
   Bool2Bit(CheckBox17.Checked, UserInf.Attribute3, 0);
  end; { not cancel }

end;

procedure TPreferences.Button1Click(Sender: TObject);
begin
  Cancel := False;
  Close;
end;

procedure TPreferences.Button2Click(Sender: TObject);
begin
  Cancel := True;
  Close;
end;

end.
