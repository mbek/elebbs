unit SelGroup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DdeMan;

type
  TProgramGroupForm = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    DdeClientConv1: TDdeClientConv;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SelectedName: String;
  end;

var
  ProgramGroupForm: TProgramGroupForm;

implementation

{$R *.DFM}

procedure TProgramGroupForm.FormCreate(Sender: TObject);
var GroupItems: PChar;
    CharCount: Word;
    TempStr: String;
    TempCH: Char;
begin
  ListBox1.Items.Clear;

  GroupItems := DdeClientConv1.RequestData('GROUPS');
  CharCount:= 00;
  TempStr := '';

  repeat
    Application.ProcessMessages;
    TempCH := GroupItems[CharCount];

    if (TempCH in [#10]) then
      begin
        ListBox1.Items.Add(TempStr);
        TempStr := '';
      end { if }
       else If (NOT (TempCH in [#10,#13])) then TempStr := TempStr + TempCH;

    Inc(CharCount);
  until (CharCount >= StrLen(GroupItems));

  StrDispose(GroupItems);
end;

procedure TProgramGroupForm.Button1Click(Sender: TObject);
begin
  SelectedName := ListBox1.Items[Listbox1.ItemIndex];
  Close;
end;

end.
