unit articles;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
(*
**
** Support Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 01-May-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, StrUnit;

Const DestFido         : String = '';
      PktPath          : String = '';
      OutboundPath     : String = '';
      GlobEmailHostName: String = '';
      DisclaimName     : String = '';

      DoAttach    : Boolean = FALSE;    { Need to builtin support into EleBBS }

type
  MessageTextType  = Array[0..(1024 * 1024) * 5] of Char;

type
  FilterArticleRecord = record
                          FromWho : String;
                          Subject : String;
                          MsgSize : Longint;
                          MsgLines: Longint;
                          Headers : Array[1..50] of String;
                          HdrLines: Longint;
                        end; { FilterArticleRecord }

{$IFDEF MSDOS}
function  GetNextLine(var MsgText: MessageTextType; var Counter, MaxLen: Longint): String;
{$ELSE}
function  GetNextLine(var MsgText: MessageTextType; var Counter, MaxLen: Longint): AnsiString;
{$ENDIF}
function  Firstfield(FieldName, MsgText: String; var S: String): Boolean;
function  GetFieldLine(FieldName: String; var MsgText: MessageTextType): String;
function  StrLen(var MsgText: MessageTextType): Longint;
function  ExtractToName(var MsgText: MessageTextType; MailAddress: Boolean; FieldName: String): String;
function  MakeHostEmail(EmailHostName, FromStr: String; Addr: AddrType): String;
function  MakeNntpDate(Time, Date: String): String;
function  GetRawEmail(TempStr: String): String;

procedure AttachToFileBase(fName: String; AttachInf: FilesRecord);
procedure PurgeSentBase(ArticleFileName: String; MaxSent: Longint);
procedure AddDisclaimer(var MsgText: MessageTextType);
procedure ScanMsgAreas(MsgTyp: MsgType; OutName: String);
procedure ProcessAttachments(const AreaNum: Word;
                             const Article: NewsArticleRecord;
                             var   Msgtext: MessageTextType;
                             var   MaxLen : Longint);
procedure ProcessNewArticles(FileName: String);
procedure FatalError(S: String; ErrorCode: Longint);
procedure AddLineToMessageText(var OrigTxt: MessageTextType; var StringLines: StringArrayObj; DoLines: Longint);
procedure AddStringLn(var MsgText: MessageTextType; Str: String);
procedure ScanMsgArea(Areanum: Longint; ArticleFName: String; MsgTyp: MsgType);
procedure AddMsgToBase(FileName: String; GroupName: String; AreaNr: Word; ArticleNr: Longint;
                       var MsgText: MessageTextType; IsEmail: Boolean);

var FilterArticle: FilterArticleRecord;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Cases, MgrFdb, WordStr, Stutils, SysUtils, CentrStr, Dos, GenFile,
     MimeDec, FileObj, ElLog_U, Debug_U, FileRout,
     StrPath, LongStr, JDates, Mail, MkMsgAbs,
     PktSup, Memman, SockFunc, Bitwise, GenDos,
     MkOpen, User_U, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var AttachInf      : FilesRecord;
    Attach_F       : pFileObj;
    Dest_Txt       : ^MessageTextType;
    Dest_Ptr       : Longint;

    HasAttachment  : Boolean;
    AttachDir      : String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function GetNextLine(var MsgText: MessageTextType; var Counter, MaxLen: Longint): String;
{$ELSE}
function GetNextLine(var MsgText: MessageTextType; var Counter, MaxLen: Longint): AnsiString;
{$ENDIF}
begin
  Result := '';

  While (MsgText[Counter] <> #13) AND (Counter < MaxLen)
         {$IFDEF MSDOS}AND (Length(Result) < 79) {$ENDIF} do
   begin
     Result := Result + MsgText[Counter];
     Inc(Counter);
   end; { while }

  if MsgText[Counter] = #13 then
   if MsgText[Counter + 1] = #10 then Inc(Counter, 2);
end; { func. GetNextline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Firstfield(FieldName, MsgText: String; var S: String): Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'FirstField - begin');
    DebugObj.DebugLog(logString, 'FirstField - FieldName: "' + FieldName + '"');
    DebugObj.DebugLog(logString, 'FirstField - MsgText  : "' + MsgText + '"');
    DebugObj.DebugLog(logString, 'FirstField - S        : "' + S + '"');
  {$ENDIF}

  if SUpCase(Copy(MsgText, 1, Length(FieldName))) = SUpCase(FieldName) then
   begin
     S := Trim(Copy(MsgText, Length(Fieldname) + 1, 255));
     FirstField := true;

     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logString, 'FirstField - FirstField = true');
     {$ENDIF}
   end
    else begin
           FirstField := false;

           {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logString, 'FirstField - FirstField = false');
           {$ENDIF}
         end; { else }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'FirstField - end');
  {$ENDIF}
end; { func. FirstField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetFieldLine(FieldName: String; var MsgText: MessageTextType): String;
var MaxLen  : Longint;
    Counter : Longint;
    CurLine : Longint;
    TempStr : String;
    OldStr  : String;
begin
  MaxLen := StrLen(MsgText);
  Counter := 0;
  CurLine := 0;
  GetFieldLine := '';

  {-------------------------- Get the headers -------------------------------}
  repeat
    TempStr := GetNextLine(MsgText, Counter, MaxLen);

    if FirstField(FieldName, TempStr, OldStr) then
       begin
         GetFieldLine := TempStr;
         BREAK;
       end; { if }

    Inc(CurLine);
  until (Counter >= MaxLen) OR (CurLine >= MaxMsgLines) OR (TempStr = '');
end; { func. GetFieldLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AttachToFileBase(fName: String; AttachInf: FilesRecord);
var AreaMgr: MgrFileObj;
    TempHdr: FilesHdrRecord;
    DT     : DateTime;
    Dow    : Word;
begin
  AreaMgr.Init(AttachInf.AreaNum, true);

  FillChar(TempHDR, SizeOf(FilesHdrRecord), #0);

  GetDate(DT.Year, Dt.Month, DT.Day, Dow);
  GetTime(DT.Hour, DT.Min, DT.Sec, Dow);
  PackTime(DT, TempHdr.LastDl);
  PackTime(Dt, TempHdr.UploadDate);

  TempHdr.Name       := fName;
  TempHdr.Size       := GetFileSize(fName, 1);
  TempHdr.CRC32      := -1;
  TempHdr.Uploader   := 'EleNEWS';
  TempHdr.FileDate   := GetPackedFileTime(AttachInf.FilePath + FName);
  TempHdr.LongDescPtr:= -1;
  Temphdr.LfnPtr     := AreaMgr.AddLfnPtr(FName, AttachInf.FilePath, TempHdr.Name);

  AreaMgr.AddHeader(TempHdr);
  AreaMgr.Done;
end; { proc. AttachToFilebase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type tDecMime = Class
        MimeDecode: TMimeDecode;

        procedure PartBegin(Sender: tObject);
        procedure PartLine(Sender  : tObject;
                           Data    : pChar;
                           DataLen : Integer);
        procedure PartEnd(Sender: tObject);

        procedure HeaderLine(Sender: tObject);
        procedure HeaderEnd(Sender: tObject);
     end; { tDecMime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDecMime.PartBegin(Sender: TObject);
begin
  if MimeDecode.PartFileName <> '' then
    begin
      if DoAttach then
        HasAttachment := TRUE;

      New(Attach_F, Init);
      Attach_F^.FileMode := ReadWriteMode + DenyNone;
      Attach_F^.Assign(AttachInf.FilePath + MimeDecode.PartFileName);

      Attach_F^.Create(1);

      RaLog('>', 'Converting attachment (mime: '+MimeDecode.PartContentType+'), file='+MimeDecode.PartFileName);
    end { if }
      else Attach_F := nil;
end; { proc. Partbegin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDecMime.PartEnd(Sender: TObject);
begin
  if Attach_F <> nil then
    begin
      Dispose(Attach_F, Done);
      AttachToFileBase(MimeDecode.PartFileName, AttachInf);

      if DoAttach then
        begin
          if AttachDir = '' then
            AttachDir := ForceBack(CreateTempDir(globalCfg^.RaConfig^.AttachPath, LineCfg^.RaNodeNr));

          if AttachDir = '' then
            HasAttachment := FALSE
             else begin
                    FileCopy(AttachInf.FilePath + MimeDecode.PartFileName,
                             AttachDir + MimeDecode.PartFileName,
                             true, false);
                  end; { else }
        end; { if }

    end; { if }
end; { proc. PartEnd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDecMime.PartLine(Sender  : TObject;
                            Data    : pChar;
                            DataLen : Integer);
const TempBuf: Array[0..1] of Char = (#13, #10);
begin
  if Attach_F <> nil then Attach_F^.BlkWrite(Data^, DataLen)
    else begin
           if (Dest_Ptr + DataLen) < (SizeOf(Dest_Txt^) - 1) then
             begin
               Move(Data^, Dest_Txt^[Dest_Ptr], DataLen);
               Inc(Dest_Ptr, DataLen);
             end; { if }

           if (Dest_Ptr + SizeOf(TempBuf)) < (SizeOf(Dest_Txt^) - 1) then
             begin
               Move(TempBuf, Dest_Txt^[Dest_Ptr], SizeOf(TempBuf));
               Inc(Dest_Ptr, SizeOf(TempBuf));
             end; { if }
         end; { else }
end; { proc. PartLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDecMime.HeaderLine(Sender : tObject);
const TempBuf: Array[0..1] of Char = (#13, #10);

var Data   : pChar;
    DataLen: Longint;
begin
  Data := MimeDecode.CurrentData;
  DataLen := SysUtils.StrLen(Data);

  if (Dest_Ptr + DataLen) < (SizeOf(Dest_Txt^) - 1) then
    begin
      Move(Data^, Dest_Txt^[Dest_Ptr], DataLen);
      Inc(Dest_Ptr, DataLen);
    end; { if }

  if (Dest_Ptr + SizeOf(TempBuf)) < (SizeOf(Dest_Txt^) - 1) then
    begin
      Move(TempBuf, Dest_Txt^[Dest_Ptr], SizeOf(TempBuf));
      Inc(Dest_Ptr, SizeOf(TempBuf));
    end; { if }
end; { proc. Headerline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDecMime.HeaderEnd(Sender: tObject);
const TempBuf: Array[0..1] of Char = (#13, #10);
begin
  if (Dest_Ptr + SizeOf(TempBuf)) < (SizeOf(Dest_Txt^) - 1) then
    begin
      Move(TempBuf, Dest_Txt^[Dest_Ptr], SizeOf(TempBuf));
      Inc(Dest_Ptr, SizeOf(TempBuf));
    end; { if }
end; { proc. HeaderEnd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddLineToMessageText(var OrigTxt: MessageTextType; var StringLines: StringArrayObj; DoLines: Longint);
var MsgText : ^MessageTextType;
    Counter : Longint;
    TmpStr  : String;
    TxtCnt  : Longint;
    MoveLen : Longint;
    MaxLen  : Longint;
    OrigPtr : Longint;
begin
  {-- Initialize the variables ---------------------------------------------}
  New(MsgText);
  FillChar(MsgText^, SizeOf(MessageTextType), #0);
  TxtCnt := 0;
  Counter := 0;

  {-- First get the message header -----------------------------------------}
  MaxLen := StrLen(OrigTxt);

  repeat
    {-- Extract the message line -------------------------------------------}
    TmpStr := GetNextLine(OrigTxt, Counter, MaxLen);

    {-- Add the header lines -----------------------------------------------}
    if TmpStr <> '' then
      begin
        TmpStr := TmpStr + #13#10;

        Move(TmpStr[1], MsgText^[TxtCnt], Length(TmpStr));
        Inc(TxtCnt, Length(TmpStr));
      end; { if }
  until (Counter >= MaxLen) OR (TmpStr = '');

  {-- Add the latest line (empty line) -------------------------------------}
  OrigPtr := Counter;
  TmpStr := #13#10;
  Move(TmpStr[1], MsgText^[TxtCnt], Length(TmpStr));
  Inc(TxtCnt, Length(TmpStr));

  {-- Now add the new lines to the message ---------------------------------}
  for Counter := 01 to DoLines do
    begin
      TmpStr := StringLines.Get(Counter) + #13#10;

      Move(TmpStr[1], MsgText^[TxtCnt], Length(TmpStr));
      Inc(TxtCnt, Length(TmpStr));
    end; { for }

  {-- Add the old ----------------------------------------------------------}
  MoveLen := StrLen(OrigTxt);
  if (MoveLen + TxtCnt) > (SizeOf(MessageTextType) - 512) then
    MoveLen := (SizeOf(MessageTextType) - TxtCnt) - 512;
  Move(OrigTxt[OrigPtr], MsgText^[TxtCnt], MoveLen);

  OrigTxt := msgText^;

  {-- Dispose all used memory ----------------------------------------------}
  Dispose(MsgText);
end; { proc. AddLineToMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessAttachments(const AreaNum: Word;
                             const Article: NewsArticleRecord;
                             var   Msgtext: MessageTextType;
                             var   MaxLen : Longint);
var Temp_F   : pFileObj;
    TempName : String;
    DecMime  : tDecMime;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments (begin)');
  {$ENDIF}

  GetFilesRecord(AttachInf, AreaNum, true);

  HasAttachment := FALSE;
  Attach_F := nil;
  AttachDir := '';
  TempName := GetEnv('TEMP');
  if TempName = '' then TempName := GlobalCfg^.RaConfig^.SemPath;
  TempName := ForceBack(CreateTempDir(TempName, 999)) + '$ELE$ATT.$$$';

  {------------------- Write the info to a file -----------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(TempName);
  Temp_F^.FileMode := ReadWriteMode + Denynone;
  if NOT Temp_F^.OpenOrCreate(1) then EXIT;
  Temp_F^.BlkWrite(MsgText, MaxLen);
  Dispose(Temp_F, Done);

  {-- Allocate memory for our new buffer ------------------------------------}
  New(Dest_Txt);
  Dest_Ptr := 0;

  {------------------------- Now, let's decode it ---------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments (decode - start)');
  {$ENDIF}

  DecMime := TDecMime.Create;
  DecMime.MimeDecode := TMimeDecode.Create(nil);

  DecMime.MimeDecode.OnHeaderLine := {$IFDEF FPC}{$IFDEF VER20}@{$ENDIF}{$ENDIF}DecMime.HeaderLine;
  DecMime.MimeDecode.OnHeaderEnd := {$IFDEF FPC}{$IFDEF VER20}@{$ENDIF}{$ENDIF}DecMime.HeaderEnd;
  DecMime.MimeDecode.OnPartBegin := {$IFDEF FPC}{$IFDEF VER20}@{$ENDIF}{$ENDIF}DecMime.PartBegin;
  DecMime.MimeDecode.OnPartLine := {$IFDEF FPC}{$IFDEF VER20}@{$ENDIF}{$ENDIF}DecMime.PartLine;
  DecMime.MimeDecode.OnPartEnd := {$IFDEF FPC}{$IFDEF VER20}@{$ENDIF}{$ENDIF}DecMime.PartEnd;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments (decode - actual)');
  {$ENDIF}

  try
    DecMime.MimeDecode.DecodeFile(TempName);
  except
  end;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments (decode - actual end)');
  {$ENDIF}

  DecMime.MimeDecode.Destroy;
  DecMime.Destroy;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments (decode - end)');
  {$ENDIF}

  {-- From now on, use our new buffer and discard the old one ---------------}
  Move(Dest_Txt^, MsgText, MaxLen);
  Dispose(Dest_Txt);


  EraseFile(TempName);
  EraseDir(JustPath(TempName), true);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessAttachments ( end )');
  {$ENDIF}
end; { proc. ProcessAttachments }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetRawEmail(TempStr: String): String;
begin
  GetRawEmail := TempStr;

  if Pos('>', TempStr) > 0 then
    GetRawEmail := Trim(Copy(TempStr, Pos('<', TempStr) + 1, Pos('>', TempStr) - Succ(Pos('<', TempStr))));
end; { func. GetRawEmail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ExtractToName(var MsgText: MessageTextType; MailAddress: Boolean; FieldName: String): String;
var MaxLen  : Longint;
    Counter : Longint;
    ToWho   : String;
    TempStr : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ExtractToName - begin');
    DebugObj.DebugLog(logString, 'ExtractToName - FieldName: "' + FieldName + '"');
    if MailAddress then
      DebugObj.DebugLog(logString, 'ExtractToName - MailAddress=TRUE')
       else DebugObj.DebugLog(logString, 'ExtractToName - MailAddress=FALSE');
  {$ENDIF}

  MaxLen := StrLen(MsgText);
  ToWho := '';
  Counter := 0;

  {-- Extract to TO: field -------------------------------------------------}
  repeat
    TempStr := GetNextLine(MsgText, Counter, MaxLen);

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logString, 'ExtractToName - TempStr="'+TempStr+'"');
    {$ENDIF}

    if FirstField(FieldName, TempStr, ToWho) then
      begin
        BREAK;
      end; { if }
  until (Counter >= MaxLen) OR (TempStr = '');

  {-- log some info --------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ExtractToName - TmpToWho = ' +ToWho);
  {$ENDIF}

  {-- Extract the email address of this ------------------------------------}
  if MailAddress then
    begin
      if Pos('>', ToWho) > 0 then
        ToWho := Copy(ToWho, Pos('<', ToWho) + 1, Pos('>', ToWho) - 3);

      {-- Extract the "TO" name --------------------------------------------}
      ToWho := Copy(ToWho, 1, Pos('@', ToWho) - 1);
      for Counter := 01 to Length(ToWho) do
        Case ToWho[Counter] of
          '.' : ToWho[Counter] := #32;
        end; { case }
    end
      else begin
             if Pos('<', ToWho) > 0 then
               ToWho := Copy(ToWho, 1, Pos('<', ToWho) - 1);

             ToWho := Trim(ToWho);

             if ToWho[1] = '"' then Delete(ToWho, 1,1);
             if ToWho[Length(ToWho)] = '"' then Delete(ToWho, Length(ToWho), 1);
           end; { if }


  ExtractToName := ToWho;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ExtractToName - ToWho = ' +ToWho);
    DebugObj.DebugLog(logString, 'ExtractToName - end');
  {$ENDIF}
end; { func. ExtractToName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TossArticle(var Article     : NewsArticleRecord;
                      var MsgText     : MessageTextType;
                      var MessageInf  : MessageRecord;
                      var EleMsgInf   : EleMessageRecord;
                      var MsgPtr      : AbsMsgPtr);
var ToWho        : String;
    FromWho      : String;
    Subject      : String;
    ReplyTo      : String;
    MsgId        : String;
    DateField    : String;
    Counter      : Longint;
    EditorLines  : pStringArrayObj;
    Curline      : Longint;

    TempStr      : String;
    PostStr      : String;
    MsgNrWritten : Longint;

    MsgTimeStr   : String;
    MsgDateStr   : String;

    IsSent       : Boolean;
    IsRcvd       : Boolean;
    IsPriv       : Boolean;
    MaxLen       : Longint;
    SavePos      : Longint;
    DestAddr     : NetAddress;
    OrigAddr     : NetAddress;
    MsgDate      : DateTime;
    UserRec      : Longint;
    UserInf      : UsersRecord;
    UserExt      : UserExtensionRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (begin)');
  {$ENDIF}

  MaxLen := StrLen(MsgText);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (MessageInf.Name='+MessageInf.Name+')');
    DebugObj.DebugLog(logTcpIp, 'TossArticle (AttachArea='+FStr(EleMsgInf.AttachArea)+')');
  {$ENDIF}


  if EleMsgInf.AttachArea <> 0 then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logTcpIp, 'TossArticle (Attach - begin)');
      {$ENDIF}

      ProcessAttachments(EleMsgInf.AttachArea, Article, MsgText, MaxLen);

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logTcpIp, 'TossArticle (Attach - end)');
      {$ENDIF}
    end; { if attacharea }

  Counter := 00;
  CurLine := 00;
  New(EditorLines, Init(MaxMsgLines));

  IsSent := TRUE;
  IsRcvd := FALSE;
  IsPriv := FALSE;
  ReplyTo := '';
  ToWho := 'All';
  FromWho := 'EleBBS (unknown sender)';
  Subject := Article.Groupname;
  DateField := '';
  Msgid := '';
  MaxLen := StrLen(MsgText);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (EditorLines initted)');
  {$ENDIF}


  {-------------------------- Get the headers -------------------------------}
  repeat
    TempStr := GetNextLine(MsgText, Counter, MaxLen);

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTcpIp, 'TossArticle (TempStr="'+TempStr+'")');
    {$ENDIF}

    if NOT FirstField('From:', TempStr, FromWho) then
     if NOT FirstField('To:', TempStr, ToWho) then
      if NOT FirstField('Subject: ', TempStr, Subject) then
       if NOT FirstField('Reply-To: ', TempStr, ReplyTo) then
        if NOT FirstField('Message-id: ', TempStr, MsgId) then
         if NOT FirstField('Date: ', TempStr, DateField) then
          if NOT Firstfield('Path: ', TempStr, TempStr) then
           if NOT FirstField('References: ', TempStr, ReplyTo) then
             if TempStr <> '' then
               begin
                 While Length(TempStr) > 0 do
                   begin
                     PostStr := TempStr;
                     TempStr := '';

                     if Length(PostStr) > 70 then
                       DoWrap(PostStr, TempStr, 70);

                     Inc(CurLine);
                     EditorLines^.Put(CurLine, ^A + PostStr);
                     if TempStr <> '' then
                       EditorLines^.Put(CurLine, ^A + ' -- ' + PostStr);
                   end; { while }

                 TempStr := 'Dummy'; { Make sure below routine doesnt blow it }
               end; { if }
  until (Counter >= MaxLen) OR (CurLine >= MaxMsgLines) OR (TempStr = '');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (Filtered all headers)');
  {$ENDIF}

  {-------------------------- Get the messagetext ---------------------------}
  SavePos := Counter;

  repeat
    Inc(CurLine);
    TempStr := GetNextLine(MsgText, Counter, MaxLen);

    EditorLines^.Put(CurLine, TempStr);
  until (Counter >= MaxLen) OR (CurLine >= MaxMsgLines);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (Before converting date)');
  {$ENDIF}

  NntpDateToString(DateField, MsgDateStr, MsgTimeStr);

  {$IFDEF WITH_DEBUG}
    DebugObj.DEbugLog(logString, 'MsgDate       = '+MsgDateStr);
    DebugObj.DebugLog(logString, 'MsgTime       = '+MsgTimeStr);
    DebugObj.DebugLog(logString, 'DateField     = '+DateField);
    DebugObj.DebugLog(logString, 'PacketPath    = '+PktPath);
    DebugObj.DebugLog(logString, 'Outboundpath  = '+OutboundPath);
  {$ENDIF}

  if HasAttachment then
    if DoAttach then
      Subject := AttachDir;

  {-- Check if its email ---------------------------------------------------}
  if ReadBit(Article.Attribute, 1) then
    begin
      ToWho := Article.GroupName;
      IsSent := TRUE;
      IsPriv := TRUE;
      IsRcvd := FALSE;
    end; { if }

  {-- Forward if necessary -------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (Before SearchUser)');
  {$ENDIF}

  UserRec := SearchUser(ToWho);
  if UserRec >= 0 then
    begin
      GetUserRecord(UserRec, UserInf, UserExt, false);

      if UserInf.ForwardTo <> '' then
        ToWho := UserInf.ForwardTo;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (Before PktPath/PostMsg)');
  {$ENDIF}

  {-- fix the REPLY and MSGID headers if necessary -------------------------}
  if MsgId <> '' then
    begin
      Delete(MsgId, 1, 1);                         { delete the starting < }
      Delete(MsgId, Length(msgId), 1);             { delete the starting > }
      MsgId := '0:0/0 ' + MsgId;
    end; { if }

  if ReplyTo <> '' then
    begin
      Delete(ReplyTo, 1, 1);                       { delete the starting < }
      Delete(ReplyTo, Length(ReplyTo), 1);         { delete the starting > }
      ReplyTo := '0:0/0 ' + ReplyTo;
    end; { if }

  {-- Post the article -----------------------------------------------------}
  if PktPath = '' then
    PostMessage(Ra250msgArea(Article.AreaNum),
                ToWho,
                FromWho,
                Subject,
                false,
                IsPriv,
                false, false, false,
                HasAttachment,
                IsSent,
                CurLine,
                EditorLines^,
                MsgNrWritten,
                '',
                '', false,
                IsRcvd,
                MsgDateStr, MsgTimeStr,
                ReplyTo,        { FTN reply kludge }
                MsgId,        { FTN msgid Kludge }
                MsgPtr);


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (Before CreatePacket)');
  {$ENDIF}

  if PktPath <> '' then
    begin
      FillChar(DestAddr, SizeOf(DestAddr), 00);
      FillChar(OrigAddr, SizeOf(OrigAddr), 00);
      FillChar(MsgDate, SizeOf(MsgDate), #00);

      With DestAddr do
        StringToAddr(DestFido, Zone, Net, Node, Point);

      GetAddress(MessageInf.AkaAddress, OrigAddr);

      {----------------- Remove the headers from the msg text ---------------}
      Move(MsgText[Counter], MsgText[1], StrLen(MsgText) - Counter);
      MsgText[Counter + 01] := #00;

      CreatePacket(Article.GroupName,
                   ForceBack(PktPath) + GetPacketName(DestAddr),
                   '',
                   ToWho,
                   FromWho,
                   Subject,
                   MsgDate,
                   MsgText,
                   OrigAddr,
                   DestAddr);
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle (After CreatePacket)');
  {$ENDIF}

  Dispose(EditorLines, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'TossArticle ( end )');
  {$ENDIF}
end; { proc. TossArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanMsgArea(Areanum: Longint; ArticleFName: String; MsgTyp: MsgType);
var MsgRead    : AbsMsgPtr;
    MessageInf : MessageRecord;
    EleMsgInf  : EleMessageRecord;
    MsgText    : ^MessageTextType;
    TempStr    : String;
    DoPost     : Boolean;
    OrigAddr   : AddrType;
    HostAddress: String;
begin
  {-- Initialize all variables ----------------------------------------------}
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);

  GetMessageRecord(MessageInf, AreaNum, true);
  GetEleMessageRecord(EleMsgInf, AreaNum, true);
  MsgRead := nil;

  {-- Check to make sure theres an areaname defined etc ---------------------}
  if MessageInf.Name = '' then
    begin
      ReleaseMem(MsgText, SizeOf(MessageTextType));
      EXIT;
    end; { if }

  {-- Try to open the messagebase -------------------------------------------}
  if NOT OpenMsgArea(MsgRead, MakeMsgId(AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                     EleMsgInf.Attribute)) then
    begin
      ReleaseMem(MsgText, SizeOf(MessageTextType));
      CloseMsgArea(MsgRead);
      RaLog('!', 'Unable to read message board ('+MessageInf.Name+')');
      EXIT;
    end; { if }

  {-- Loop through all messages in this area --------------------------------}
  MsgRead^.SeekFirst(01);                        { Seek to correct message# }
  While MsgRead^.SeekFound do
    begin
      MsgRead^.MsgStartUp;     { Initialize the header (date, from who etc) }

      {-- Only handle if it isnt sent already -------------------------------}
      if NOT MsgRead^.IsSent then
        begin
          {-- Get some of the message characteristics -----------------------}
          MsgRead^.GetOrig(OrigAddr);
          MsgRead^.MsgTxtStartUp;             { Initialize the message text }
          MsgRead^.SetSent(true);                         { Receive message }
          MsgRead^.ReWriteHdr;
          HostAddress := MakeHostEmail(GlobEmailHostName, MsgRead^.GetFrom, OrigAddr);

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logTcpIp, 'Composing message, from: "' + MsgRead^.GetFrom + '"');
            DebugObj.DebugLog(logTcpIp, 'Composing message, to  : "' + MsgRead^.GetTo + '"');
            DebugObj.DebugLog(logTcpIp, 'Composing message, subj: "' + MsgRead^.GetSubj + '"');
          {$ENDIF}

          {-- Add the appropriate headers to the messagebuffer --------------}
          FillChar(MsgText^, SizeOf(MessageTextType), #00);
          AddStringLn(MsgText^, 'Date: ' + MakeNNTPdate(MsgRead^.GetTime, MsgRead^.GetDate));
          AddStringLn(MsgText^, 'From: "' + MsgRead^.GetFrom + '" <'+HostAddress+'>');

          {-- Now differentiate between area --------------------------------}
          Case MsgTyp of
             Newsgroup: begin
                          AddStringLn(MsgText^, 'Newsgroups: ' + EleMsgInf.GroupName);
                          AddStringLn(MsgText^, 'X-Newsreader: ' + PidName);
                        end; { newsgroup }
             Internet : begin
                          AddStringLn(MsgText^, 'To: ' + MsgRead^.GetTo);
                          AddStringLn(MsgText^, 'X-Mailer: ' + PidName);
                        end; { internet (email) }
          end; { case }

          AddStringLn(MsgText^, 'Subject: ' + MsgRead^.GetSubj);
          AddStringLn(MsgText^, '');

          {-- Loop through the email to handle kludges correctly ------------}
          While NOT MsgRead^.EOM do
            begin
               TempStr := MsgRead^.GetString(78);
               DoPost := true;

               if TempStr[1] = ^A then
                begin
                  DoPost := false;

                  if NOT FirstField(^A + 'MSGID:', TempStr, TempStr) then
                   if NOT FirstField(^A + 'CHRS:', TempStr, TempStr) then
                    if NOT FirstField(^A + 'PID:', TempStr, TempStr) then
                     if NOT FirstField(^A + 'TID:', TempStr, TempStr) then
                      if NOT FirstField(^A + 'CODEPAGE: ', TempStr, TempStr) then
                       if NOT FirstField(^A + 'CHRS: ', TempStr, TempStr) then
                        if NOT FirstField(^A + 'TZUTC: ', TempStr, TempStr) then
                         if NOT FirstField(^A + 'REPLY: ', TempStr, TempStr) then
                           DoPost := true;

                  TempStr := '(' + Copy(TempStr, 2, Length(TempStr) - 1) + ')';
                end; { if }

               if DoPost then
                 AddStringLn(MsgText^, TempStr);
            end; { while }

          {-- Eventually add the whole buffer to the e-mail out file --------}
          AddMsgToBase(ArticleFName, EleMsgInf.GroupName, MessageInf.AreaNum, -1, MsgText^, false)
        end; { if }

      {-- Seek to the next message ------------------------------------------}
      MsgRead^.SeekNext;
    end; { while }

  {-- Dispose of all pending objects ----------------------------------------}
  CloseMsgArea(MsgRead);
  ReleaseMem(MsgText, SizeOf(MessageTextType));
end; { proc. ScanMsgArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessNewArticles(FileName: String);
var Article_F : pFileObj;
    Article   : NewsArticleRecord;
    MsgText   : ^MessageTextType;
    Tossed    : Longint;
    AreaCnt   : Longint;

    ThisMsgPtr: AbsMsgPtr;
    MessageInf: MessageRecord;
    EleMsgInf : EleMessageRecord;

    LastName  : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles  (begin)');
  {$ENDIF}

  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);

  Tossed := 00;
  AreaCnt := 0;
  Lastname := '';
  ThisMsgPtr := nil;

  RaLog('>', 'Tossing articles into message base');
  WriteLn(#32, SystemMsgPrefix, 'Tossing articles into message base');

  New(Article_F, Init);
  Article_F^.Assign(FileName);
  Article_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT Article_F^.Open(1) then
    begin
      Dispose(Article_F, Done);

      WriteLn('   (no articles to process)');
      RaLog('>', 'Tossing completed, no articles processed');
      EXIT;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles - File opened, looping');
  {$ENDIF}

  While NOT Article_F^.EOF do
     begin
       {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles - In Loop (01)');
       {$ENDIF}

       FillChar(MsgText^, SizeOf(MessageTextType), #00);

       Article_F^.BlkRead(Article, SizeOf(NewsArticleRecord));
       if Article_F^.IoResult > 00 then
         FatalError('Unable to read article database', -1);

       {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles - In Loop (02)');
       {$ENDIF}

       Article_F^.BlkRead(MsgText^, Article.BodyLen);
       if Article_F^.IoResult > 00 then
         FatalError('Error occured while trying to read article text', -1);

       if ThisMsgPtr = nil then
         begin
           GetMessageRecord(MessageInf, Article.AreaNum, true);
           GetEleMessageRecord(EleMsgInf, Article.AreaNum, true);

           if NOT OpenOrCreateMsgArea(ThisMsgPtr,
                                      MakeMsgId(MessageInf.AreaNum,
                                                MessageInf.JamBase,
                                                MessageInf.Attribute,
                                                EleMsgInf.Attribute)) then
             begin
               {$IFDEF WITH_DEBUG}
                 DebugObj.DebugLog(logMailSys, 'Failed to open message area '+MessageInf.Jambase);
               {$ENDIF}

               EXIT;
             end; { if }
         end; { if }

       Inc(AreaCnt);
       if (AreaCnt MOD 50) = 0 then
         Write(FStr(AreaCnt), ')', Dup(#8,Length(FStr(AreaCnt)) + 1));

       {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles - In Loop (03)');
       {$ENDIF}

       TossArticle(Article,
                   MsgText^,
                   MessageInf,
                   EleMsgInf,
                   ThisMsgPtr);

       {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles - In Loop (04)');
       {$ENDIF}

       if LastName <> Article.GroupName then
         begin
           if LastName <> '' then
             WriteLn(FStr(AreaCnt), ')'); { skip to the next line }

           Write(#32, SystemMsgPrefix, 'Processing mail from ', Article.Groupname, ' (#');

           if ThisMsgPtr <> nil then
             begin
               CloseMsgArea(ThisMsgPtr);
               ThisMsgPtr := nil;
               AreaCnt := 0;
             end; { if }
         end; { if }
       Lastname := Article.GroupName;

       Inc(Tossed);
     end; { while }

  RaLog('>', 'Tossing completed ('+FStr(Tossed)+' articles processed)');
  WriteLn(FStr(AreaCnt), ')'); { skip to the next line }
  WriteLn('   (', Tossed,' messages processed)');

  Article_F^.Close;
  Article_F^.Erase;
  Dispose(Article_F, Done);

  ReleaseMem(MsgText, SizeOf(MessageTextType));

  if ThisMsgPtr <> nil then
    begin
      CloseMsgArea(ThisMsgPtr);
      ThisMsgPtr := nil;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'ProcessNewArticles  ( end )');
  {$ENDIF}
end; { proc. ProcessNewArticles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrLen(var MsgText: MessageTextType): Longint;
var Counter: Longint;
begin
  for Counter := 00 to (SizeOf(MsgText) - 01) do
    if MsgText[Counter] = #00 then
      begin
        BREAK;
      end; { if }

  StrLen := Counter;
end; { func. StrLen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String; ErrorCode: Longint);
begin
  WriteLn;
  WriteLn(#32, SystemMsgPrefix, 'Fatal error occured!');
  WriteLn(#32, SystemMsgPrefix, S);
  if ErrorCode <> -1 then
    WriteLn(#32, SystemMsgPrefix, '#', ErrorCode, ', ', SockGetErrStr(ErrorCode));
  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddMsgToBase(FileName: String; GroupName: String; AreaNr: Word; ArticleNr: Longint;
                       var MsgText: MessageTextType; IsEmail: Boolean);
var Article_F: pFileObj;
    Article  : NewsArticleRecord;
    TmpAttr  : Byte;
begin
  New(Article_F, Init);
  Article_F^.Assign(FileName);
  Article_F^.FileMode := ReadWriteMode + DenyAll;

  if NOT Article_F^.Open(1) then
    begin
      if NOT Article_F^.Create(1) then
         FatalError('Unable to update article file! ('+FileName+')', -1);
    end; { if }

  Article_F^.Seek(Article_F^.FileSize);

  FillChar(Article, SizeOf(NewsArticleRecord), #00);
  Article.GroupName := GroupName;
  Article.AreaNum := AreaNr;
  Article.ArticleNr := ArticleNr;
  Article.BodyLen := StrLen(MsgText) + 01;
  Article.TimesSent := 0;

  TmpAttr := Article.Attribute;
  ClearBit(TmpAttr, 0);                              { Already processed? No. }
  if IsEmail then
    SetBit(TmpAttr, 1)
      else ClearBit(TmpAttr, 1);
  Article.Attribute := TmpAttr;

  Article_F^.BlkWrite(Article, SizeOf(NewsArticleRecord));
  Article_F^.BlkWrite(MsgText, Article.BodyLen);

  Dispose(Article_F, Done);
end; { proc. AddMsgToBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddStringLn(var MsgText: MessageTextType; Str: String);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, ' -- AddStringLn(Str="' + Str + '"');
  {$ENDIF}

  Str := Str + #13#10;

  Move(Str[1], MsgText[Strlen(MsgText)], Length(Str));
end; {  proc. AddStringLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeNntpDate(Time, Date: String): String;
var TzStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MakeNntpDate - TZ = ' + GetEnv('TZ'));
  {$ENDIF}

  TzStr := GetEnv('TZ');
  if TzStr = '' then
    TzStr := 'GMT';

 { old Time := Time + ':00 GMT'; }                                  { Add seconds }
  Time := Time + ':00 ' + TzStr;

  Date := Copy(GetDayOfWeek, 1, 3) + ', ' + ReFormatDate(Date, 'DD NNN YYYY');

  Result := Date + #32 + Time;
end; { func. MakeNntpDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeHostEmail(EmailHostName, FromStr: String; Addr: AddrType): String;
var TempStr  : String;
     Counter : Longint;
begin
  {-- Check if the from address isnt already an email-addr. -------------------}
  if Pos('@', FromStr) > 0 then
    begin
      MakeHostEmail := FromStr;
      EXIT;
    end; { if }

  {-- If not, create one ------------------------------------------------------}
  if (EmailHostName <> '') then
    begin
      for Counter := 01 to Length(fromStr) do
        Case FromStr[Counter] of
          #32 : FromStr[Counter] := '.';
        end; { case }

      MakeHostEmail := SlowCase(FromStr) + '@' + EmailhostName;
    end
      else begin
             for Counter := 01 to Length(fromStr) do
               Case FromStr[Counter] of
                 #32 : FromStr[Counter] := '.';
               end; { case }

             With Addr do
               TempStr :=  'p' + fStr(Point) +
                          '.f' + fStr(Node) +
                          '.n' + fStr(Net) +
                          '.z' + fStr(Zone) +
                          '.fidonet.org';

             MakeHostEmail := FromStr + '@' + TempStr;
           end; { if }

end; { func. MakeHostEmail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddDisclaimer(var MsgText: MessageTextType);
var Text_F : Text;
    TempStr: String;
begin
  if DisclaimName = '' then EXIT;

  AddStringLn(MsgText, '');

  Assign(Text_F, DisclaimName);
  {$i-} System.Reset(Text_F); {$i+}
  if IoResult = 0 then
    begin
      While NOT Eof(Text_F) do
        begin
          {$i-} ReadLn(Text_F, TempStr); {$i+}
          if IoResult > 0 then ;

          AddStringLn(MsgText, TempStr);
        end; { while }

      {$i-} Close(Text_F); {$i+}
      if IoResult > 0 then ;
    end
      else RaLog('!', 'Unable to open disclaimer file, '+DisclaimName);
end; { proc. AddDisclaimer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanMsgAreas(MsgTyp: MsgType; OutName: String);
var AreaInf   : MessageRecord;
    Counter   : Longint;
    FirstMsg  : Longint;
    HighMsg   : Longint;
    DiskLoMsg : Longint;
    Area_F    : pFileObj;
begin
  {-- Open the area file ----------------------------------------------------}
  New(Area_F, Init);
  Area_F^.Assign(MessagesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if NOT Area_F^.Open(1) then
    FatalError('Error while opening '+MessagesFileName, -1);

  {-- Scan through all areas ------------------------------------------------}
  While NOT Area_F^.EOF do
    begin
      Area_F^.BlkRead(AreaInf, SizeOf(MessageRecord));
      if Area_F^.IoResult > 0 then BREAK;

      {-- Only act when its an defined area ---------------------------------}
      if AreaInf.Typ = Msgtyp then
        begin
          Write(#32, SystemMsgPrefix);

          Write('Scanning message area for new articles (', AreaInf.Name, ')');

          ScanMsgArea(AreaInf.AreaNum, Outname, MsgTyp);

          WriteLn;
        end; { if }
    end; { while }

  {-- Dispose of any pending objects ----------------------------------------}
  Dispose(Area_F, Done);
end; { proc. ScanMsgAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PurgeSentBase(ArticleFileName: String; MaxSent: Longint);
var Article_F    : pFileObj;
    Article      : NewsArticleRecord;
    MsgText      : ^MessageTextType;
    Tempor_F     : pFileObj;
    KeepInBase   : Boolean;
    KeptMsgs     : Longint;
begin
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);

  RaLog('>', 'Purging outbound articlesfile');
  WriteLn(#32, SystemMsgPrefix, 'Purging outbound articlesfile');

  New(Tempor_F, Init);
  Tempor_F^.Assign('$$ELE$$.OUT');
  Tempor_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT Tempor_F^.Create(1) then
    FatalError('Unable to create $$ELE$$.OUT for purging outbound message base', -1);

  New(Article_F, Init);
  Article_F^.Assign(ArticlefileName);
  Article_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT Article_F^.Open(1) then
    begin
      Dispose(Article_F, Done);
      Dispose(Tempor_F, Done);

      WriteLn('   (no articles to process)');
      RaLog('>', 'Posting completed, no articles processed');
      EXIT;
    end; { if }

  KeptMsgs := 00;

  While NOT Article_F^.EOF do
     begin
       FillChar(MsgText^, SizeOf(MessageTextType), #00);

       Article_F^.BlkRead(Article, SizeOf(NewsArticleRecord));
       Article_F^.BlkRead(MsgText^, Article.BodyLen);
       if Article_F^.IoResult > 00 then FatalError('Unable to read article database', -1);

       KeepInBase := true;
       if ReadBit(Byte(Article.Attribute), 0) then KeepInBase := false;
       if Article.TimesSent > MaxSent then
         begin
           {-- Article expired, but make sure it wasnt sent already --------}
           if KeepInBase then
             begin
               RaLog('!', 'Email message #' + FStr(Article.ArticleNr) + ' purged - unable to send');
             end; { if }

           KeepInBase := false;
         end; { if }

       if KeepInBase then
         begin
          Tempor_F^.BlkWrite(Article, SizeOf(NewsArticleRecord));
          Tempor_F^.BlkWrite(MsgText^, Article.BodyLen);
          if Tempor_F^.IoResult > 00 then FatalError('Unable to update purging database', -1);

          Inc(KeptMsgs);
         end; { if }
     end; { while }

  RaLog('>', 'Purging completed ('+FStr(KeptMsgs)+' articles left)');
  WriteLn('   (', KeptMsgs,' messages left in outbound)');

  Article_F^.Close;
  Article_F^.Erase;
  Dispose(Article_F, Done);

  Tempor_F^.Close;

  if KeptMsgs > 00 then RenameObjWithDrive(Tempor_F, ArticleFileName)
   else Tempor_F^.Erase;
  Dispose(Tempor_F, Done);

  ReleaseMem(MsgText, SizeOf(MessageTextType));
end; { proc. PurgeSentBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit articles }
