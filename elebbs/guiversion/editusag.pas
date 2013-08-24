unit EditUsag;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, ExtCtrls, LongStr, RXSpin;

type
  TUsage = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Bevel2: TBevel;
    Button1: TButton;
    Button2: TButton;
    RxSpinEdit1: TRxSpinEdit;
    RxSpinEdit2: TRxSpinEdit;
    RxSpinEdit3: TRxSpinEdit;
    RxSpinEdit4: TRxSpinEdit;
    RxSpinEdit5: TRxSpinEdit;
    RxSpinEdit6: TRxSpinEdit;
    RxSpinEdit7: TRxSpinEdit;
    RxSpinEdit8: TRxSpinEdit;
    RxSpinEdit9: TRxSpinEdit;
    RxSpinEdit10: TRxSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Cancel: Boolean;
  public
    { Public declarations }
  end;

var
  Usage: TUsage;

implementation

{$R *.DFM}
uses Global, EditMain;

procedure TUsage.Button1Click(Sender: TObject);
begin
 Cancel := False;
 Close;
end;

procedure TUsage.Button2Click(Sender: TObject);
begin
  Cancel := True;
  Close;
end;

procedure TUsage.FormActivate(Sender: TObject);
begin
  Cancel := False;

  With UserEdit,Usage do
  begin;
   RxSpinEdit1.Text := FStr(UserInf.Downloads);
   RxSpinEdit2.Text := FStr(UserInf.Uploads);
   RxSpinEdit3.Text := FStr(UserInf.UploadsK);
   RxSpinEdit4.Text := FStr(UserInf.DownloadsK);
   RxSpinEdit5.Text := FStr(UserInf.MsgsPosted);
   RxSpinEdit6.Text := FStr(UserInf.NoCalls);
   RxSpinEdit7.Text := FStr(UserInf.FileArea);
   RxSpinEdit8.Text := FStr(UserInf.FileGroup);
   RxSpinEdit9.Text := FStr(UserInf.MsgArea);
   RxSpinEdit10.Text := FStr(UserInf.MsgGroup);
  end; { with }
end;

procedure TUsage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  If NOT cancel then
  With UserEdit,Usage do
   begin;
    UserInf.Downloads      := FVal(RxSpinEdit1.Text);
    UserInf.Uploads        := FVal(RxSpinEdit2.Text);
    UserInf.UploadsK       := FVal(RxSpinEdit3.Text);
    UserInf.DownloadsK     := FVal(RxSpinEdit4.Text);
    UserInf.MsgsPosted     := FVal(RxSpinEdit5.Text);
    UserInf.NoCalls        := FVal(RxSpinEdit6.Text);
    UserInf.FileArea       := FVal(RxSpinEdit7.Text);
    UserInf.FileGroup      := FVal(RxSpinEdit8.Text);
    UserInf.MsgArea        := FVal(RxSpinEdit9.Text);
    UserInf.MsgGroup       := FVal(RxSpinEdit10.Text);
   end; { if cancel }
end;

end.
