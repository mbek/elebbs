{$I APDEFINE.INC}
{$X+}
{.$DEFINE Debug}

{******************************************************}
{*                  MODDEMO.PAS  2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program ModemDemo;
  {-Demonstrates loading, viewing and editing the modem database}

uses
{$IFDEF UseTPro}
  TpDos,
{$ENDIF}
{$IFDEF UseOPro}
  OpDos,
  OpRoot,
  OpString,
{$ENDIF}
{$IFDEF Debug}
  Gadgets,
{$ENDIF}
  Dos,
  App,
  MsgBox,
  Dialogs,
  StdDlg,
  Objects,
  Menus,
  Memory,
  Drivers,
  Views,
  Crt,
  ApMisc,
  ApPort,
  OoModDB,
  OoIniDB,
  OoIni;

type
  {-Abstract object type whose HandleEvent converts Enter and Esc}
  TAbsDialog =
    object (TDialog)
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Handles the modem index}
  PModemIndex = ^TModemIndex;
  TModemIndex =
    object(TStringCollection)
      constructor Init(InitList : DoubleListPtr);
      function Compare (Key1, Key2 : Pointer) : Integer; virtual;
    end;

  {-Derived list box which makes a double click modify the selected modem}
  PModemListBox = ^TModemListBox;
  TModemListBox =
    object(TListBox)
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Dialog box which allows the user to modify compression or correction}
  PCompCorrWin = ^TCompCorrWin;
  TCompCorrWin =
    object (TAbsDialog)
      CompWin : Boolean;
      constructor Init(CW : Boolean);
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Dialog box which allows the user to modify link rate information}
  PLinkRateWin = ^TLinkRateWin;
  TLinkRateWin =
    object (TAbsDialog)
      MyChkBox : PCheckBoxes;
      constructor Init;
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Dialog box which allows the user to modify basic data information}
  PChgDataWin = ^TChgDataWin;
  TChgDataWin =
    object(TAbsDialog)
      constructor Init;
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Dialog box which contains index of modems}
  PPickWindow = ^TPickWindow;
  TPickWindow =
    object(TDialog)
      MyListBox : PModemListBox;
      constructor Init;
      destructor Done; virtual;
      procedure DoChgAdd(var Event : TEvent);
      procedure HandleEvent(var Event : TEvent); virtual;
    end;

  {-Main application object}
  PModemDBApp = ^TModemDBApp;
  TModemDBApp =
    object (TApplication)
      PickWindow    : PPickWindow;
      {$IFDEF Debug}
      Heap          : PHeapView;
      procedure Idle; virtual;
      {$ENDIF}
      constructor Init;
      destructor Done; virtual;
      function Confirm : Word;
      procedure InitMenuBar; virtual;
      procedure InitStatusLine; virtual;
      procedure HandleEvent (var Event : TEvent); virtual;
      procedure ReportError;
      procedure Save;
      function SaveAs: Word;
      function EditMenuItems (NextP : PMenuItem)   : PMenuItem;
      function FileMenuItems (NextP : PMenuItem)   : PMenuItem;
      function StdStatusKeys (NextP : PStatusItem) : PStatusItem;
    end;


const
  dbTempFile  = 'MODEMTMP.INI';
  cmNew       = 241;
  cmOpen      = 242;
  cmSave      = 243;
  cmAddRec    = 244;
  cmUpd       = 245;
  cmCorrect   = 246;
  cmCompress  = 247;
  cmBaud      = 248;
  cmExit      = 249;
  cmDontDo    = 250;
  cmSaveAs    = 251;
  cmAdd       = 252;
  cmChg       = 253;
  cmDel       = 254;
  StdCommands = [cmAdd, cmChg, cmDel, cmNew, cmOpen, cmSave, cmSaveAs, cmQuit];

var
  ModemDBApp    : PModemDBApp;
  DB            : ModemDBasePtr;
  CurrData      : ModemDataPtr;
  FMIP, EMIP    : PMenuItem;
  CurrModemName : string;
{$IFDEF Debug}
  CurrMem       : array[1..5] of LongInt;
  I             : Integer;
{$ENDIF}

function Long2Str (Num : LongInt) : string;

var
  S : string;

begin
  Str(Num, S);
  Long2Str := S;
end;

procedure TAbsDialog.HandleEvent(var Event : TEvent);
{-Converts Esc to cmCancel and Enter to cmOK}
begin
  TWindow.HandleEvent(Event);
  if (Event.What = evKeyDown) then begin
    if (Event.KeyCode = kbEsc) then begin
      Event.What := evCommand;
      Event.Command := cmCancel;
    end else
      if (Event.KeyCode = kbEnter) then begin
        Event.What := evCommand;
        Event.Command := cmOK;
      end;
  end;
end;

constructor TModemIndex.Init(InitList : DoubleListPtr);
{-Inserts modem index defined by InitList}
var
  Temp    : IniLinePtr;
  TempStr : string;

begin
  TCollection.Init(150, 10);
  if (InitList = nil) then
    Exit
  else begin
    Temp := IniLinePtr(InitList^.dlHead);
    while Temp <> nil do begin
      Insert(NewStr(Temp^.PL^));
      Temp := IniLinePtr(InitList^.Next(Temp));
    end;
  end;
end;

function TModemIndex.Compare (Key1, Key2 : Pointer) : Integer;
{-Insures that the modems are listed in the same order they are on disk}
begin
  Compare := -1;
end;

procedure TModemListBox.HandleEvent(var Event : TEvent);
{-Has a double click modify the selected modem}
begin
  if Event.What = evMouseDown then
    if Event.Double then begin
      Event.Command := cmChg;
      ModemDBApp^.PickWindow^.DoChgAdd(Event);
    end;
  TListBox.HandleEvent(Event);
end;

constructor TChgDataWin.Init;
{-Creates buttons and fields for the ChgDataWin}
var
  R     : TRect;
  Field : PInputLine;

begin
  R.Assign(1, 1, 76, 23);
  TDialog.Init(R, 'Modem Data');
  Options := Options or OfCentered;

  R.Assign(16, 1, 49, 2);
  Field := New(PInputLine, Init(R, ModemNameLen));
  Insert(Field);
  R.Assign(10, 1, 15, 2);
  Insert(New(PLabel, Init(R, '~N~ame', Field)));

  R.Assign(3, 3, 11, 4);
  Insert(New(PStaticText, Init(R, 'Commands')));

  R.Assign(14, 4, 39, 5);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 4, 13, 5);
  Insert(New(PLabel, Init(R, '~I~nitialize', Field)));

  R.Assign(14, 6, 39, 7);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 6, 13, 7);
  Insert(New(PLabel, Init(R, '~D~ial', Field)));

  R.Assign(14, 8, 39, 9);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 8, 13, 9);
  Insert(New(PLabel, Init(R, 'Dial ~s~uffix', Field)));

  R.Assign(14, 10, 39, 11);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 10, 13, 11);
  Insert(New(PLabel, Init(R, 'Cance~l~ Dial', Field)));

  R.Assign(14, 12, 39, 13);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 12, 13, 13);
  Insert(New(PLabel, Init(R, '~H~ang Up', Field)));

  R.Assign(14, 14, 39, 15);
  Field := New(PInputLine, Init(R, ConfigLen));
  Insert(Field);
  R.Assign(1, 14, 13, 15);
  Insert(New(PLabel, Init(R, 'Con~f~igure', Field)));

  R.Assign(14, 16, 39, 17);
  Field := New(PInputLine, Init(R, CmdLen));
  Insert(Field);
  R.Assign(1, 16, 13, 17);
  Insert(New(PLabel, Init(R, '~A~nswer', Field)));

  R.Assign(40, 2, 55, 3);
  Insert(New(PStaticText, Init(R, 'Return Codes')));

  R.Assign(52, 3, 73, 4);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 3, 51, 4);
  Insert(New(PLabel, Init(R, 'O~k~ay', Field)));

  R.Assign(52, 5, 73, 6);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 5, 51, 6);
  Insert(New(PLabel, Init(R, '~C~onnect', Field)));

  R.Assign(52, 7, 73, 8);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 7, 51, 8);
  Insert(New(PLabel, Init(R, 'B~u~sy', Field)));

  R.Assign(52, 9, 73, 10);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 9, 51, 10);
  Insert(New(PLabel, Init(R, '~V~oice', Field)));

  R.Assign(52, 11, 73, 12);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 11, 51, 12);
  Insert(New(PLabel, Init(R, 'N~o~ Carrier', Field)));

  R.Assign(52, 13, 73, 14);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 13, 51, 14);
  Insert(New(PLabel, Init(R, 'No Dial~t~one', Field)));

  R.Assign(52, 15, 73, 16);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 15, 51, 16);
  Insert(New(PLabel, Init(R, '~E~rror', Field)));

  R.Assign(52, 17, 73, 18);
  Field := New(PInputLine, Init(R, RspLen));
  Insert(Field);
  R.Assign(39, 17, 51, 18);
  Insert(New(PLabel, Init(R, 'Rin~g~', Field)));

  R.Assign(1, 19, 17, 21);
  Insert(New(PButton, Init(R, 'Co~r~rection', cmCorrect, bfNormal)));
  R.Assign(19, 19, 36, 21);
  Insert(New(PButton, Init(R, 'Co~m~pression', cmCompress, bfNormal)));
  R.Assign(38, 19, 48, 21);
  Insert(New(PButton, Init(R, 'Ba~u~d', cmBaud, bfNormal)));
  R.Assign(50, 19, 58, 21);
  Insert(New(PButton, Init(R, 'OK', cmOK, bfDefault)));
  R.Assign(61, 19, 73, 21);
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  SetData(CurrData^.Data);
  SelectNext(False);
end;

procedure TChgDataWin.HandleEvent(var Event : TEvent);
{-Will open up CompCorrWin and LinkRateWin with appropriate buttons, close
and save data on cmOK, close but not save data on cmCancel}
var
  CompCorrWin : PCompCorrWin;
  LinkRateWin : PLinkRateWin;
  MBOptions   : Word;

begin
  TAbsDialog.HandleEvent(Event);
  if (Event.What = evCommand) then
    case Event.Command of
      cmOK :
        begin
          GetData(CurrData^.Data);
          if (CurrData^.Data.Name = '') then begin
            MBOptions := mfOKButton or mfWarning;
            MessageBox(#3 + 'You must input a modem name!', nil, MBOptions);
            ClearEvent(Event);
          end else
            EndModal(cmUpd);
        end;
      cmCancel :
        EndModal(cmDontDo);
      cmCompress :
        begin
          CompCorrWin := New(PCompCorrWin, Init(True));
          Desktop^.ExecView(CompCorrWin);
          Dispose(CompCorrWin, Done);
        end;
      cmCorrect :
        begin
          CompCorrWin := New(PCompCorrWin, Init(False));
          Desktop^.ExecView(CompCorrWin);
          Dispose(CompCorrWin, Done);
        end;
      cmBaud :
        begin
          LinkRateWin := New(PLinkRateWin, Init);
          Desktop^.ExecView(LinkRateWin);
          Dispose(LinkRateWin, Done);
        end;
    end;

end;

constructor TCompCorrWin.Init(CW : Boolean);

const
  CCWinTitle : array [Boolean] of string = ('Correction', 'Compression');

var
  R     : TRect;
  I     : Integer;
  Field : PInputLine;

begin
  R.Assign(1, 1, 40, 16);
  TDialog.Init(R, CCWinTitle[CW]);
  Options := Options or ofCentered;
  Flags := Flags and not wfClose;
  CompWin := CW;

  R.Assign(13, 2, 34, 3);
  Field := New(PInputLine, Init(R, TagLen));
  Insert(Field);
  R.Assign(3, 2, 12, 3);
  Insert(New(PLabel, Init(R, 'String ~1~', Field)));


  R.Assign(13, 4, 34, 5);
  Field := New(PInputLine, Init(R, TagLen));
  Insert(Field);
  R.Assign(3, 4, 12, 5);
  Insert(New(PLabel, Init(R, 'String ~2~', Field)));

  R.Assign(13, 6, 34, 7);
  Field := New(PInputLine, Init(R, TagLen));
  Insert(Field);
  R.Assign(3, 6, 12, 7);
  Insert(New(PLabel, Init(R, 'String ~3~', Field)));

  R.Assign(13, 8, 34, 9);
  Field := New(PInputLine, Init(R, TagLen));
  Insert(Field);
  R.Assign(3, 8, 12, 9);
  Insert(New(PLabel, Init(R, 'String ~4~', Field)));

  R.Assign(13, 10, 34, 11);
  Field := New(PInputLine, Init(R, TagLen));
  Insert(Field);
  R.Assign(3, 10, 12, 11);
  Insert(New(PLabel, Init(R, 'String ~5~', Field)));

  R.Assign(4, 12, 16, 14);
  Insert(New(PButton, Init(R, '~O~k', cmOK, bfDefault)));
  R.Assign(20, 12, 34, 14);
  Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
  if CompWin then
    SetData(CurrData^.Compression)
  else
    SetData(CurrData^.Errors);
  SelectNext(False);
end;

function FixData(var T : TTagArray) : Word;
{-Compresses all of the strings in T and returns the number of strings}
var
  J, K    : Word;
  Finished: Boolean;

begin
  J := 1;
  repeat
    Finished := True;
    if (T[J] = '') then begin
      Move(T[J + 1], T[J], SizeOf(TagStringType) * (MaxTags - J));
      T[MaxTags] := '';
    end else
      Inc(J);
    for K := Succ(J) to MaxTags do
      if (T[K] <> '') then
        Finished := False;
  until Finished or (J = MaxTags);
  FixData := J;
end;

procedure TCompCorrWin.HandleEvent(var Event : TEvent);
{-Stores data and exits on cmOK; simply exits on cmCancel}
var
  NumStrings : Word;
  TempData   : TTagArray;

begin
  TAbsDialog.HandleEvent(Event);
  if (Event.What = evCommand) then
    case Event.Command of
      cmOK :
        begin
          GetData(TempData);
          NumStrings := FixData(TempData);
          if CompWin then begin
            CurrData^.Compression := TempData;
            CurrData^.NumComps := NumStrings;
          end else begin
            CurrData^.Errors := TempData;
            CurrData^.NumErrors := NumStrings;
          end;
          EndModal(cmDontDo);
        end;
      cmCancel :
        EndModal(cmDontDo);
    end;
end;

constructor TLinkRateWin.Init;
{-Creates the link rate window with appropriate data}
var
  Field   : PInputLine;
  R       : TRect;
  S       : string;


begin
  R.Assign(1, 1, 26, 11);
  TDialog.Init(R, 'Link Rate Info');
  Options := Options or ofCentered;
  Flags := Flags and not wfClose;

  R.Assign(2, 3, 20, 4);
  Field := New(PInputLine, Init(R, BaudLen));
  Insert(Field);
  R.Assign(2, 2, 20, 3);
  Insert(New(PLabel, Init(R, 'Default ~B~PS Rate', Field)));

  R.Assign(2, 5, 22, 6);
  MyChkBox := New(PCheckBoxes, Init(R, NewSItem('Lock ~D~TE Rate',nil)));
  Insert(MyChkBox);

  R.Assign(2, 7, 10, 9);
  Insert(New(PButton, Init(R, '~O~k', cmOK, bfDefault)));
  R.Assign(12, 7, 22, 9);
  Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));


  S := Long2Str(CurrData^.DefBaud);
  SetData(S);
  if not CurrData^.LockDTE then
    MyChkBox^.Press(0);
  SelectNext(False);
end;

procedure TLinkRateWin.HandleEvent(var Event : TEvent);
{-Saves data on cmOK and exits; on cmCancel, just exits}
  procedure DoOK;
  {-Saves data to CurrData}
  var
    Check      : Integer;
    MBOptions  : Word;
    Num        : LongInt;
    S          : string;

  begin
    GetData(S);
    Val(S, Num, Check);
    if (Check <> 0) then begin
      MBOptions := mfOKButton or mfWarning;
      MessageBox(#3 + 'Invalid BPS Rate.', nil, MBOptions);
      ClearEvent(Event);
      Exit;
    end;
    CurrData^.DefBaud := Num;
    CurrData^.LockDTE := MyChkBox^.Mark(0);
    EndModal(cmDontDo);
  end;

begin
  TAbsDialog.HandleEvent(Event);
  if (Event.What = evCommand) then
    case Event.Command of
      cmOK :
        DoOK;
      cmCancel :
        EndModal(cmDontDo);
    end;
end;

constructor TPickWindow.Init;
{-Creates a pick window with MyListBox associated with it}
var
  R        : TRect;
  Control  : PView;
  ScrollBar: PScrollBar;

begin
  R.Assign(0, 0, 45, 21);
  TDialog.Init(R, 'Modem Index');
  Options := Options or ofCentered;
  R.Assign(42, 2, 43, 17);
  New(ScrollBar, Init(R));
  Insert(ScrollBar);
  R.Assign(2, 2, 42, 17);
  MyListBox := New(PModemListBox, Init(R, 1, ScrollBar));
  Insert(MyListBox);
  R.Assign(2, 18, 14, 20);
  Insert(New(PButton, Init(R, '~A~dd', cmAdd, bfNormal)));
  R.Assign(30, 18, 42, 20);
  Insert(New(PButton, Init(R, '~D~elete', cmDel, bfNormal)));
  R.Assign(16, 18, 28, 20);
  Insert(New(PButton, Init(R, '~C~hange', cmChg, bfDefault)));
  SelectNext(False);
end;

destructor TPickWindow.Done;
{-Gets rid of list box, then calls ancestor destructor}
begin
  MyListBox^.NewList(nil);
  Dispose(MyListBox, Done);
  TDialog.Done;
end;

procedure TPickWindow.DoChgAdd(var Event : TEvent);
var
  TempCmd    : Word;
  ChgDataWin : PChgDataWin;

begin
  TempCmd := Event.Command;
  if (TempCmd = cmChg) then begin
    CurrModemName := MyListBox^.GetText(MyListBox^.Focused, ModemNameLen);
    if (CurrModemName <> '') then
      DB^.RetrieveModem(CurrModemName, CurrData^)
    else
      Exit;
  end else
    FillChar(CurrData^, SizeOf(ModemData), 0);
  ModemDBApp^.ReportError;
  ClearEvent(Event);

  ModemDBApp^.DisableCommands(StdCommands);
  EMIP^.Disabled := True;
  FMIP^.Disabled := True;

  ChgDataWin := New(PChgDataWin, Init);
  Event.Command := Desktop^.ExecView(ChgDataWin);
  Dispose(ChgDataWin, Done);

  Event.What := evCommand;
  if (Event.Command = cmUpd) then
    if (TempCmd = cmAdd) then
      Event.Command := cmAddRec
    else
      Event.Command := cmUpd;

  ModemDBApp^.EnableCommands(StdCommands);
  EMIP^.Disabled := False;
  FMIP^.Disabled := False;
end;

procedure TPickWindow.HandleEvent(var Event : TEvent);
{-Opens up ChgDataWin with cmAdd or cmChg command, open a warning dialog before
deleting modem on cmDel command}
var
  MBOptions : Word;
  InitList  : DoubleListPtr;

begin
  if (Event.What = evCommand) then
    case Event.Command of
      cmClose :
        begin
          Event.What := evCommand;
          Event.Command := cmExit;
        end;
      cmAdd, cmChg :
        DoChgAdd(Event);
      cmDel :
        begin
          CurrModemName := MyListBox^.GetText(MyListBox^.Focused, ModemNameLen);
          if (CurrModemName <> '') then begin
            MBOptions := mfYesButton or mfNoButton or mfCancelButton
                         or mfConfirmation;
            if (MessageBox('Are you sure you want to delete this modem record?',
                           nil, MBOptions) = cmYes) then begin
              DB^.DelModem(CurrModemName);
              ModemDBApp^.ReportError;
              New(InitList, Init);
              DB^.EnumGroupItems(InitList, dbIndex, False);
              ModemDBApp^.ReportError;
              if (InitList^.dlHead = nil) then
                ModemDBApp^.PickWindow^.MyListBox^.NewList(nil)
              else
                ModemDBApp^.PickWindow^.MyListBox^.NewList(New(PModemIndex, Init(
                                                               InitList)));
              Dispose(InitList, Done);
            end;
          end;
          ClearEvent(Event);
        end;
    end;
  TDialog.HandleEvent(Event);
end;

constructor TModemDBApp.Init;
{-Initializes menu bar, pick window, and data objects}
var
  MBOptions  : Word;
  R          : TRect;

begin
  LowMemSize := (32 * 1024) div 16;
  TApplication.Init;
{$IFDEF Debug}
  GetExtent(R);
  R.A.Y := Pred(R.B.Y);
  Heap := New(PHeapView, Init(R));
  Insert(Heap);
{$ENDIF}
  R.Assign(5, 6, 25, 8);
  PickWindow := New(PPickWindow, Init);
  Desktop^.Insert(PickWindow);

  New(CurrData);
  FillChar(CurrData^,SizeOf(ModemData),0);

  New(DB, Init(dbTempFile, False, False));
  if (DB = nil) then begin
    MBOptions := mfOKButton or mfError;
    MessageBox(#3 + StatusStr(AsyncStatus), nil, MBOptions);
  end;
end;

destructor TModemDBApp.Done;
{-Disposes data objects, then disposes itself}
begin
  Dispose(DB, Done);
  Dispose(CurrData);
  TApplication.Done;
end;

{$IFDEF Debug}
procedure TModemDBApp.Idle;
{-Updates heap information}
begin
  TProgram.Idle;
  Heap^.Update;
end;
{$ENDIF}

function TModemDBApp.Confirm : Word;
  {-Creates a dialog box asking the user to save the current database.}

var
  IsNewFile : Boolean;
  MBOptions : Word;
  TempCmd   : Word;

begin
  MBOptions := mfYesButton or mfNoButton or mfCancelButton or mfWarning;
  if DB^.GetFileName = dbTempFile then begin
    IsNewFile := True;
    TempCmd := MessageBox('You have modified a new file.' + #13 +
                          'Do you wish to save changes?', nil, MBOptions);
  end else begin
    IsNewFile := False;
    TempCmd := MessageBox(DB^.GetFileName + ' has been modified.' + #13 +
                          'Do you wish to save changes?', nil, MBOptions);
  end;
  case TempCmd of
    cmYes:
      begin
        if IsNewFile then begin
          if SaveAs = cmCancel then
            TempCmd := cmCancel
          else
            TempCmd := cmOk;
        end else begin
          Save;
          TempCmd := cmOK;
        end;
      end;
    cmNo:
      TempCmd := cmOK;
    cmCancel:
      TempCmd := cmCancel;
  end;
  Confirm := TempCmd;
end;

procedure TModemDBApp.InitMenuBar;

var
  R : TRect;

begin
  GetExtent(R);
  R.B.Y := Succ(R.A.Y);
  MenuBar := New(PMenuBar, Init(R, NewMenu(FileMenuItems(EditMenuItems(nil)))));
end;

procedure TModemDBApp.InitStatusLine;

var
  R : TRect;

begin
{$IFNDEF Debug}
  GetExtent(R);
  R.A.Y := Pred(R.B.Y);
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      StdStatusKeys(nil),nil)));
  WriteStr(0, Size.Y - 1, dbTempFile,1);
{$ENDIF}
end;

procedure TModemDBApp.HandleEvent(var Event: TEvent);
{-Handles cmNew, cmOpen, cmSave, cmSaveAs. cmExit, cmAddRec, and cmUpd commands}
  var
    MBOptions : Word;
    InitList  : DoubleListPtr;

  procedure DoNew;

  begin
    if DB^.Modified then
      if Confirm = cmCancel then begin
        ClearEvent(Event);
        Exit;
      end;
    Dispose(DB, Done);
    New(DB, Init(dbTempFile, False, False));
    if (DB = nil) then begin
      MBOptions := mfOKButton or mfError;
      MessageBox(#3 + StatusStr(AsyncStatus), nil, MBOptions);
    end;

    PickWindow^.MyListBox^.NewList(nil);
  end;

  procedure DoOpen;

  var
    RWFileWindow  : PFileDialog;
    RWFileOptions : Word;
    InitList      : DoubleListPtr;
    IniFileName   : PathStr;

  begin
    if DB^.Modified then
      if Confirm = cmCancel then begin
        ClearEvent(Event);
        Exit;
      end;

    RWFileOptions :=  fdOpenButton;
    RWFileWindow := New(PFileDialog, Init('*.INI', 'Open Database',
                        'File Name :', RWFileOptions, 0));
    if (Desktop^.ExecView(RWFileWindow) = cmCancel) then begin
      Dispose(RWFileWindow, Done);
      ClearEvent(Event);
      Exit;
    end;
    RWFileWindow^.GetFileName(IniFileName);
    if not ExistFile(IniFileName) then begin
      MBOptions := mfOKButton or mfWarning;
      MessageBox(#3 + 'File doesn''t exist.', nil, MBOptions);
      ClearEvent(Event);
      Exit;
    end;
    Dispose(RWFileWindow, Done);

    Dispose(DB, Done);
    New(DB, Init(IniFileName, False, False));
    if (DB = nil) then begin
      MBOptions := mfOKButton or mfError;
      MessageBox(#3 + StatusStr(AsyncStatus), nil, MBOptions);
    end;

    New(InitList, Init);
    DB^.EnumGroupItems(InitList, dbIndex, False);
    ReportError;
    if (AsyncStatus = ecOK) then
      PickWindow^.MyListBox^.NewList(New(PModemIndex,Init(InitList)));
    Dispose(InitList, Done);
  end;

begin
  TApplication.HandleEvent(Event);
  if (Event.What = evCommand) then
    case Event.Command of
      cmNew:
        begin
          DoNew;
          ClearEvent(Event);
        end;
      cmOpen:
        begin
          DoOpen;
          ClearEvent(Event);
        end;
      cmSave:
        begin
          if DB^.GetFileName = dbTempFile then
            SaveAs
          else
            Save;
          ClearEvent(Event);
        end;
      cmSaveAs:
        begin
          SaveAs;
          ClearEvent(Event);
        end;
      cmExit:
        begin
          if DB^.Modified then
            if Confirm = cmCancel then begin
              ClearEvent(Event);
              Exit;
            end;
          EndModal(cmQuit);
        end;
      cmDontDo:
        ClearEvent(Event);
      cmUpd :
        begin
          DB^.UpdModem(CurrModemName, CurrData^);
          ReportError;
          New(InitList, Init);
          DB^.EnumGroupItems(InitList, dbIndex, False);
          ReportError;
          PickWindow^.MyListBox^.NewList(New(PModemIndex, Init(InitList)));
          Dispose(InitList, Done);
          ClearEvent(Event);
        end;
      cmAddRec :
        begin
          DB^.AddModem(CurrData^);
          ReportError;
          New(InitList, Init);
          DB^.EnumGroupItems(InitList, dbIndex, False);
          ReportError;
          PickWindow^.MyListBox^.NewList(New(PModemIndex, Init(InitList)));
          Dispose(InitList, Done);
          ClearEvent(Event);
        end;
    end;
end;

procedure TModemDBApp.ReportError;
{-Checks AsyncStatus and opens up a dialog if there's an error}
var
  MBOptions : Word;

begin
  if (AsyncStatus <> ecOK) then begin
    MBOptions := mfOKButton or mfError;
    MessageBox(#3 + StatusStr(AsyncStatus), nil, MBOptions);
  end;
end;

procedure TModemDBApp.Save;
{-Writes data to disk}
var
  MBOptions : Word;

begin
  DB^.FlushFile;
  if (AsyncStatus <> ecOK) then begin
    MBOptions := mfOKButton or mfError;
    MessageBox('I/O error saving database :' + Long2Str(AsyncStatus), nil,
               MBOptions);
  end;
end;

function TModemDBApp.SaveAs : Word;
{-Writes data to disk in file IniFileName}
var
  RWFileWindow  : PFileDialog;
  MBOptions     : Word;
  RWFileOptions : Word;
  IniFileName   : PathStr;

begin
  RWFileOptions := fdOpenButton;
  RWFileWindow := New(PFileDialog, Init('*.INI', 'Save Database',
                                        'File Name :',RWFileOptions, 0));
  if (Desktop^.ExecView(RWFileWindow) = cmCancel) then begin
    Dispose(RWFileWindow, Done);
    SaveAs := cmCancel;
    Exit;
  end;
  RWFileWindow^.GetFileName(IniFileName);
  Dispose(RWFileWindow, Done);
  DB^.ChangeFileName(IniFileName);
  DB^.ForceUpd;
  if (AsyncStatus <> ecOK) then begin
    MBOptions := mfOKButton or mfError;
    MessageBox('I/O error saving database :' + Long2Str(AsyncStatus), nil,
               MBOptions);
  end;
  SaveAs := cmOK;
end;

function TModemDBApp.EditMenuItems (NextP : PMenuItem) : PMenuItem;

begin
  EMIP :=
    NewSubMenu('~E~dit', hcNoContext, NewMenu(
      NewItem('~A~dd', '', kbNoKey, cmAdd, hcNoContext,
      NewItem('~C~hange', '',kbNoKey, cmChg, hcNoContext,
      NewItem('~D~elete', '',kbNoKey, cmDel, hcNoContext, nil)))),NextP);
  EditMenuItems := EMIP;
end;

function TModemDBApp.FileMenuItems (NextP : PMenuItem) : PMenuItem;

begin
  FMIP :=
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('~N~ew', '', kbNoKey, cmNew, hcNoContext,
      NewItem('~O~pen', 'F3', kbF3, cmOpen, hcNoContext,
      NewItem('~S~ave', 'F2', kbF2, cmSave, hcNoContext,
      NewItem('Save ~A~s', '', kbNoKey, cmSaveAs, hcNoContext,
      NewLine(
      NewItem('E~x~it', '', kbNoKey, cmExit, hcNoContext, nil))))))), NextP);
  FileMenuItems := FMIP;
end;

function TModemDBApp.StdStatusKeys (NextP: PStatusItem) : PStatusItem;

begin
  StdStatusKeys :=
    NewStatusKey('', kbAltX, cmQuit,
    NewStatusKey('', kbF10, cmMenu,
    NewStatusKey('', kbAltF3, cmClose, NextP)));
end;

function HeapFunc (Size : Word) : Integer; far;
var
  Amt : Word;
begin
  Amt := Size;
  HeapFunc := 2;
end;

begin
  HeapError := @HeapFunc;
  New(ModemDBApp, Init);
  ModemDBApp^.Run;
  Dispose(ModemDBApp, Done);
{$IFDEF Debug}
  for I := 1 to 5 do
    Writeln(CurrMem[I]);
{$ENDIF}
end.