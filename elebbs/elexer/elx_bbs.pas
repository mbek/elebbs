UNIT ELX_BBS;
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
{$IFDEF FPC}
  {$R-}
{$ENDIF}
(*
**
** BBS module for EleXer. Interpreter interface.
**
** Copyright (c) 2000-2002 by Maarten Bekers
**
** Created : 17-Mar-2001
** Last update : 14-Oct-2002
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF Win32}
       Windows,
     {$ENDIF}

    GenDos,
     elx_Glob,
      MkMsgAbs,
       CharUnit,
        StrUnit,
         MgrFdb,
          Dos
       {$IFDEF ELX_MYSQL}
          ,MySQl
       {$ENDIF}

       {$IFDEF TCPIP}
         ,SockFunc
         ,SockDef
         ,TcpSrv
       {$ENDIF} ;

{.$DEFINE ELX_PROFILE}


type elxMsgBaseRecord = record
                          AreaNum      : Longint;
                          MsgPtr       : AbsMsgPtr;
                          EditorLines  : pStringArrayObj;
                          MaxEditLines : Longint;
                        end; { record }


type
  pelx_GlobalsType = ^elx_globalstype;


type elxDataRecord = record
         web_CgiWrite     : elx_UserWriteTyp; { default: nil; }
         elx_KeyPressed   : Boolean;
         elx_KeysString   : String;
         elx_ReturnString : String;
         elx_FileBase     : ^MgrFileObj;
         elx_MessageBase  : ^ElxMsgBaseRecord;
         {$IFDEF TCPIP}
           elx_Sockets      : Array[1..5] of tTcpServer;
         {$ENDIF}
         elx_FindData     : Array[1..5] of SearchRec;
     end; { elxDataRecord }


type tElxBbsObj = Object
         elxData: ^elxDataRecord;
         RunError: Longint;                                 { 0 = Succesful }
                                                       { 1 = file not found }
                                                            { 2 = undefined }
                                                    { 3 = error with memory }
                                                      { 4 = disk read error }
                                                 { 5 = incompatible version }
                                                     { 6 = error during run }
         elxErrorStr: String;                                { Error string }

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function  RunElexerScript(FileName,
                                   MiscData: String;
                                   LogRun  : Boolean): boolean;
         function  GetElxReturnString: String;

         {-- Private routines ----------------------------------------------}
         function InterpretScript(Filename, Params: String;
                                  var elx_Globals: pelx_GlobalsType;
                                  PreserveObject: Boolean): Boolean;
     end; { tElxBbsObj }

type pElxBBSObj = ^tElxBbsObj;

function GetScriptType(FName: String): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 uses
       TimeStmp,
          Elx_Blck,
           Elx_Int,
            Global,
             CfgRec,
              ElLog_U,
               LongStr,
                Flags,
                 Ral,
                  BitWise,
                   StUtils,
                     Access_U,
                      WinTitle,
                       Multi,
                        GenFile,
                         Colors,
                          Debug_U,
                           Limit_U,
                            StrPath,
                             CentrStr,
                              WordStr,
                               FlagStr,
                                Jdates,
                                 UnixDate,
                                  CurTime,
                                   User_U,
                                    UsrExt,
                                    FileRout,
                                     Terminal,
                                      FileSys,
                                       Desc_U,
                                        Memman,
                                         Cases,
                                          MultiLn,
                                           ListFile,
                                            TagUnit,
                                             Crc_Unit,
                                              MkOpen,
                                               Mail,
                                                WriteMsg,
                                                 ReadMsg,
                                                  ObjDec,
                                                   MkMsgJam
                                                    {$IFDEF FPC}
                                                      ,md5
                                                    {$ENDIF}


  {$IFNDEF MSDOS}
   {$IFNDEF ELEUNIX}
    {$IFNDEF GO32V2}
     ,Articles
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}

  {$IFDEF ISCGI}
    ,Strings
    ,web_Sup
    ,web_Scr,
     web_Glob
  {$ENDIF}

  {$IFNDEF MSDOS}
    ,SysUtils
  {$ENDIF}

  {$IFDEF WITH_FULL}
    {-- The routines that require Input/Output with te user --}
    {$IFNDEF ISCGI}
     ,Crt
    {$ENDIF}
    ,Support,
      Input_U,
       AreaSel,
        ViewFile,
         OutBlock,
          FastScrn,
           InOut_U,
            Control,
             Transfer,
              LineEd,
               StatusB,
                SysKey,
                 EleMenu,
                  Dispans
  {$ENDIF} ;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_UserRead(var TmpStr; typ: tType; DoLn: Boolean);
begin
  { Read(Ln) is not supported under EleBBS }
end; { proc. elx_UserRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure elx_UserWrite(const TmpStr: String; DoLn: Boolean);
{$ELSE}
procedure elx_UserWrite(const TmpStr: AnsiString; DoLn: Boolean);
{$ENDIF}
begin
  if TmpStr <> '' then
    Write(TmpStr);

  if DoLn then
    WriteLn;
end; { proc. elx_UserWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_UserFuncs(const FuncNr: Longint;
                        var   elx_GlobalsBuf;
                        var   TmpStack: Array of StackType;
                        var   FuncReturn: StackType;
                        var   CallPtr: Pointer;
                        const CallTypes: String);

var elx_Globals: elx_GlobalsType ABSOLUTE elx_GlobalsBuf;
    ObjPtr     : ^elxDataRecord;


procedure SetReturnString(var   StackEntry: StackType;
{$IFDEF MSDOS}
                          const TmpStr: String;
{$ELSE}
                          const TmpStr: AnsiString;
{$ENDIF}
                          const ReUseOld: Boolean);
begin
  if (StackEntry.S = 0) OR (NOT ReUseOld) then
  begin
    StrAllocNew(elx_Globals, StackEntry.S, 1);
{    writeln('Alloced: ', StackEntry.S, ' (for: ', tmpstr,')'); }
  end; { if }

  if StackEntry.S <> -1 then
    begin
      {$IFDEF MSDOS}
        elx_Globals.StringTable^[StackEntry.S]^.Content := TmpStr;
        elx_Globals.StringTable^[StackEntry.S]^.Len := Length(TmpStr);
      {$ELSE}
        elx_Globals.StringTable^[StackEntry.S]^.Len := Length(TmpStr);
        
(*        if length(tmpstr) > 0 then
        writeln('debug: setting length: ', length(tmpstr), ' ->> ', tmpstr); *)

        with elx_Globals do
          if StringTable^[StackEntry.S]^.AllocSz <
              StringTable^[StackEntry.S]^.Len then
                StrGrowString(elx_Globals, StackEntry.S,
                              StringTable^[StackEntry.S]^.Len);

        SysUtils.StrPCopy(PChar(elx_Globals.StringTable^[StackEntry.S]^.Content), TmpStr);
      {$ENDIF}
    end; { if }
end; { proc. SetReturnString }

{$IFNDEF MSDOS}
procedure SetDynArrayString(var   ArrayRef  : Longint;
                                  ArrayIdx  : Longint;
                                  StrIdx    : Longint;
                            const TmpStr    : PChar;
                            const UpdateHash: Boolean;
                            const ArrayName : AnsiString);
var TmpStack: StackType;
begin
  with elx_Globals do
  begin
    {-- allocate a new string ---------------------------------------------------------------}
    if StrIdx <= 0 then
      begin
        StrAllocNew(elx_Globals, StrIdx, 1);
      end; { if }

    {-- update the length -------------------------------------------------------------------}
    if TmpStr <> nil then
      {$IFDEF FPC}
        elx_Globals.StringTable^[StrIdx]^.Len := StrLen(TmpStr)
      {$ELSE}
        elx_Globals.StringTable^[StrIdx]^.Len := Sysutils.StrLen(TmpStr)
      {$ENDIF}
         else elx_Globals.StringTable^[StrIdx]^.Len := 0;

    {-- actually put the string into the array ----------------------------------------------}
    with elx_Globals do
      if StringTable^[StrIdx]^.AllocSz <
         StringTable^[StrIdx]^.Len then
           StrGrowString(elx_Globals, StrIdx,
                         StringTable^[StrIdx]^.Len);

    {-- and copy the contents ---------------------------------------------------------------}
    if TmpStr <> nil then
      SysUtils.StrCopy(PChar(elx_Globals.StringTable^[StrIdx]^.Content), TmpStr);

    {-- and add it to the hash table --------------------------------------------------------}
    if UpdateHash then
      begin
        with DynArrayTable^[ArrayRef] do
          begin
            {-- create a temp stack entry ----------------------------------}
            FillChar(TmpStack, SizeOf(TmpStack), 0);
            TmpStack.S := StrIdx;
            TmpStack.cn := typ_strngs;
            TmpStack.DynArraySet := ArrayRef;

            {-- use an itnernal routine to update it -----------------------}
            ArrayPtrRec^[ArrayIdx] := TmpStack;

            {-- and update the array name if necessary ---------------------}
            if ArrayName <> '' then
              begin
               try
                 ArrayNames.Put(ArrayName, ArrayIdx);
               except
                 ArrayNames.Remove(ArrayName);
                 ArrayNames.Put(ArrayName, ArrayIdx);
               end; { except }
              end; { if }

            {-- and update the highest item --------------------------------}
            if HighestItem < Arrayidx then
              HighestItem := ArrayIdx;
            LastElementLd := -1;
          end; { if }
      end; { if }
  end; { with }
end; { proc. SetDynArrayString }
{$ENDIF}

var TmpStr,
    TmpStr2,
    TmpStr3,
    TmpStr4,
    TmpStr5,
    TmpStr6,
    TmpStr7,
    {$IFDEF MSDOS}
    TmpStr8   : String;
    {$ELSE}
    TmpStr8   : AnsiString;
    {$ENDIF}
    {$IFDEF TCPIP}
     TmpStrA   : AnsiString;
    {$ENDIF}

    ShortTmp,
    ShortTmp2 : ShortString;

    TempSet   : CharSet;
    TmpBool   : Boolean;
    TmpByte,
    TmpByte2  : Byte;
    TmpLong,
    TmpLong2,
    TmpLong3,
    TmpLong4  : Longint;

    TmpWord,
    TmpWord2,
    TmpWord3,
    TmpWord4,
    TmpWord5,
    TmpWord6  : Word;

    TmpSmallWord,
    TmpSmallWord2,
    TmpSmallWord3,
    TmpSmallWord4  : SmallWord;

    {$IFDEF TCPIP}
      SockAddrTemp : TSockAddr;
    {$ENDIF}

    {$IFDEF ISCGI}
      MsgText_1       : ^web_MsgTextRecord;
      MaxTxtLen       : Longint;
    {$ENDIF}

{$IFDEF elx_mysql}
type
  TMYSQL_ROW = MYSQL_ROW;
  TMYSQL_ROW_OFFSET = MYSQL_ROW_OFFSET;
{$endif}
  
var
    {$IFDEF elx_MySQL}
      MyRow           : TMYSQL_ROW;
      MyFields        : PMYSQL_FIELD;
    {$ENDIF}


    TmpBuf_1        : Array[0..MaxRecByteSize] of Byte;
    TmpFlag_1       : FlagType ABSOLUTE TmpBuf_1;
    TmpFileArea_1   : FilesRecord ABSOLUTE TmpBuf_1;
    TmpGroup_1      : GroupRecord ABSOLUTE TmpBuf_1;
    TmpLang_1       : LanguageRecord ABSOLUTE TmpBuf_1;
    TmpMsgArea_1    : MessageRecord ABSOLUTE TmpBuf_1;
    TmpMenu_1       : MnuRecord ABSOLUTE TmpBuf_1;
    TmpUserOn_1     : UseronRecord ABSOLUTE TmpBuf_1;
    TmpTag_1        : TagFileRecord ABSOLUTE TmpBuf_1;
    TmpFileHdr_1    : FilesHdrRecord ABSOLUTE TmpBuf_1;
    TmpFileIdx_1    : FilesIdxRecord ABSOLUTE TmpBuf_1;
    TmpUser_1       : UsersRecord ABSOLUTE TmpBuf_1;
    TmpUserExt_1    : UserExtensionRecord ABSOLUTE TmpBuf_1;
    TmpEleFileArea_1: EleFilesRecord ABSOLUTE TmpBuf_1;
    TmpEleMsgArea_1 : EleMessageRecord ABSOLUTE TmpBuf_1;
    TmpSysInfo_1    : SysInfoRecord ABSOLUTE TmpBuf_1;
    TmpExitinfo_1   : ExitinfoRecord ABSOLUTE TmpBuf_1;
    TmpEvent_1      : EventRecord ABSOLUTE TmpBuf_1;
    TmpCombined_1   : CombinedRecord ABSOLUTE TmpBuf_1;
    TmpAddress_1    : NetAddress ABSOLUTE TmpBuf_1;
    TmpProtocol_1   : ProtocolRecord ABSOLUTE TmpBuf_1;

    {$IFNDEF MSDOS}
     {$IFNDEF ELEUNIX}
      {$IFNDEF GO32V2}
       TmpArticle_1    : FilterArticleRecord ABSOLUTE TmpBuf_1;
      {$ENDIF}
     {$ENDIF}
    {$ENDIF}

    TmpUserExt_2   : UserExtensionRecord;
    TmpFlag_2      : FlagType;
    TmpFlag_3      : FlagType;
    TmpEleMsgArea_2: EleMessageRecord;

procedure OneToHundred;
begin
  if FuncNr < 10 then
    begin
      Case FuncNr of
         {-- Insert -----------------------------------------------------------}
         1 : begin { insert }
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);
               Insert(TmpStr, TmpStr2, TmpStack[2].I);

               {-- Return the string ------------------------------------------}
               SetReturnString(TmpStack[1], TmpStr2, true);
             end; { insert }

         {-- Delete -----------------------------------------------------------}
         2 : begin { delete }
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               Delete(TmpStr, TmpStack[1].I, TmpStack[2].i);

               SetReturnString(TmpStack[0], TmpStr, true);
             end; { delete }

         {-- ClearKeyBuffer ---------------------------------------------------}
         {$IFDEF WITH_FULL}
           3 : InputObj^.DorCleanKeys;
         {$ENDIF}

         {-- WaitForEnter -----------------------------------------------------}
         {$IFDEF WITH_FULL}
           4 : begin
                 InputObj^.PressEnter(TmpStack[0].B,   { Force enter or use system }
                                   TmpStack[1].B);  { add line feeds }
               end; { waitforenter }
         {$ENDIF}

         {-- PutInKeyBuffer ---------------------------------------------------}
         5 : begin
               {-- Make sure we get something usefull -------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- and put it into the keybuffer ------------------------------}
               {$IFDEF WITH_FULL}
                 InputObj^.PutInBuffer(TmpStr, TmpStack[1].B);
               {$ENDIF}
             end; { PutInKeyBuffer }

         {-- AskYesNo ---------------------------------------------------------}
         6 : begin
               {$IFDEF WITH_FULL}
                 FuncReturn.B := InputObj^.YesNoAsk(TmpStack[0].B);
               {$ENDIF}
             end; { AskYesNo }

         {-- UpdateLocalScreen ------------------------------------------------}
         7 : begin
               {$IFDEF WITH_FULL}
                 InputObj^.UpdateScrn;
               {$ENDIF}
             end; { UpdateLocalScreen }

         {-- RangeEdit --------------------------------------------------------}
         8 : begin
               {-- Make sure we get something usefull -------------------------}
               if CallTypes[2] = 'c' then
                 TmpStr := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- Actually edit the content ----------------------------------}
               {$IFDEF WITH_FULL}
                 RangeEdit(TmpStack[0].I,         { Input var }
                           TmpStr,                { Show message }
                           TmpStack[2].I,         { Start number }
                           TmpStack[3].I,         { End number }
                           TmpStack[4].I);        { Default number }
               {$ENDIF}
             end; { RangeEdit }

         {-- LineEdit ---------------------------------------------------------}
         9 : begin
               {-- Make sure we get something usefull -------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- Set up a default character set -----------------------------}
               TempSet := [#32..#255];
               Case TmpStack[2].I of
                 0 : TempSet := [#32..#255];    { Normal }
                 1 : TempSet := [#32..#255];    { Password }
                 2 : TempSet := [#32..#255];    { Username }
               end; { case }

               {-- Actually edit the content ----------------------------------}
               {$IFDEF WITH_FULL}
                 ShortTmp := TmpStr;
                 GetString(ShortTmp,                 { Input var }
                           TmpStack[1].I,          { Input length }
                           TempSet,                { Character set with legals }
                           TmpStack[3].B,          { Capitalize }
                           TmpStack[4].B,          { Password }
                           TmpStack[5].B,          { WordWrap }
                           TmpStack[6].B);         { IEMSI }
                 TmpStr := ShortTmp;
               {$ENDIF}

               {-- Set the content of the return var --------------------------}
               SetReturnString(TmpStack[0], TmpStr, true);
             end; { LineEdit }
      end; { case }
   end { FuncNr < 10 }

    {-- 10 .. 19 --------------------------------------------------------------}
    else if FuncNr < 20 then
     begin
       Case FuncNr of
         {-- ResetMorePrompt --------------------------------------------------}
         {$IFDEF WITH_FULL}
           10 : OutputObj^.ResetLines(01);
         {$ENDIF}

         {-- SetStatusBar -----------------------------------------------------}
         {$IFDEF WITH_FULL}
           11 : StatusDisplay(TmpStack[0].I, false);
         {$ENDIF}

         {-- RunSysOpKey ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           12 : LocalCommand(TmpStack[0].C, 0, false);
         {$ENDIF}

         {-- RunMenuCmd -------------------------------------------------------}
         {$IFDEF WITH_FULL}
           13 : begin
                 {-- Make sure we get something usefull -------------------------}
                 if CallTypes[2] = 'c' then
                   TmpStr := Copy(TmpStack[1].C, 1, 1)
                     else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- Execute the menu item -----------------------------------}
                  TempSet := [];
                  ExecuteMenuFunc(TmpStack[0].I, TmpStr, '', FuncReturn.B, TempSet);
                end; { RunMenuCmd }
         {$ENDIF}

         {-- GetBBSversion ----------------------------------------------------}
         14 : begin
                {-- Return the current EleBBS build-number etc ----------------}
                TmpStr := PidName;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { GetBBSversion }

         {-- GetEnvironment ---------------------------------------------------}
         15 : begin
                {-- Make sure we get something usefull ------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now retrieve that content ---------------------------------}
                TmpStr := GetEnv(TmpStr);

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { GetEnvironment }

         {-- SetUserFlags, Toggle, Reset, Check flags -------------------------}
         16,                       { Set user flags }
         17,                       { Toggle user flags }
         18,                       { Reset user flags }
         19 : begin                { Check user flags }
                {-- Make sure we get something usefull ------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Call the actual function ----------------------------------}
                Case FuncNr of
                  16 : RaSetFlags(TmpFlag_1, TmpStr);
                  17 : RaToggleFlags(TmpFlag_1, TmpStr);
                  18 : RaReSetFlags(TmpFlag_1, TmpStr);
                  19 : FuncReturn.B := RaCheckFlags(TmpFlag_1, TmpStr);
                end; { case }

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { SetUserFlags }
       end; { case }
     end  { FuncNr < 20 }

    {-- 20 .. 29 --------------------------------------------------------------}
    else if FuncNr < 30 then
     begin
       Case FuncNr of
         {-- CmpUserFlags -----------------------------------------------------}
         20 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
                Stack2Buf(elx_Globals, TmpFlag_2, FVal(Copy(CallTypes, 7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));

                {-- Call the actual function ----------------------------------}
                FuncReturn.B := RaCmpFlags(TmpFlag_1, TmpFlag_2);

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
                Buf2Stack(elx_Globals, TmpFlag_2, FVal(Copy(CallTypes, 7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));
              end; { cmpUserFlags }

         {-- ralGet, ralGetKeys, ralGetStr, ralGetDsp, ralGetDefaultKey, ------}
         {-- ralGetTitleInfo, ralGetKey, ralHasKey ----------------------------}
         21,                                                      { ralGet }
         22,                                                  { ralGetKeys }
         23,                                                   { ralGetStr }
         24,                                                   { ralGetDsp }
         25,                                            { ralGetDefaultKey }
         26,                                             { ralGetTitleInfo }
         27,                                                   { ralGetKey }
         28,                                                   { ralHasKey }
         29 : begin                                          { ralGetcolor }
                {-- First get the ral entry number ------------------------}
                Case FuncNr of
                  21 : TmpStr := langObj^.ralGet(TmpStack[0].i);
                  22 : TmpStr := langObj^.ralGetKeys(TmpStack[0].i);
                  23 : TmpStr := langObj^.ralGetStr(TmpStack[0].i);
                  24 : TmpStr := langObj^.ralGetDsp(TmpStack[0].i);
                  25 : TmpStr := langObj^.ralGetDefaultKey(TmpStack[0].i);
                  26 : TmpStr := langObj^.ralGetTitleInfo;
                  27 : FuncReturn.C := langObj^.ralGetKey(TmpStack[0].i);
                  28 : FuncReturn.B := langObj^.ralHasKey(TmpStack[0].i);
                  29 : FuncReturn.I := langObj^.ralGetColor(TmpStack[0].i);
                end; { case }

               {-- Set the content of the return var --------------------------}
               if FuncNr in [21..26] then
                 SetReturnString(FuncReturn, TmpStr, false);
              end; { ralfunctions }
       end; { case }
     end  { else }

    {-- 30 .. 39 --------------------------------------------------------------}
    else if FuncNr < 40 then
     begin
       Case FuncNr of
         {-- ralGetEntries ----------------------------------------------------}
         30 : begin
                FuncReturn.I := langObj^.ralEntries;
              end; { ralGetEntries }

         {-- ralResetCache ----------------------------------------------------}
         31 : begin
                langObj^.ResetCache;
              end; { ralResetCache }

         {-- ralGetAll --------------------------------------------------------}
         32 : begin
                TmpByte := TmpStack[1].i;

                langObj^.ralGetAll(TmpStack[0].i,           { Entry-number }
                                   TmpByte,                        { Color }
                                   ShortTmp,                 { Text string }
                                   ShortTmp2,     { String with valid keys }
                                   TmpStack[4].C,            { Default key }
                                   TmpStack[5].B,        { Has defaultkey? }
                                   TmpStack[6].B);               { HasKeys }

                 TmpStack[1].I := TmpByte;

                 {-- Now return the both strings -------------------------------}
                 SetReturnString(TmpStack[2], ShortTmp, true);
                 SetReturnString(TmpStack[3], ShortTmp2, true);
              end; { ralGetAll }

         {-- SetBit ----------------------------------------------------------}
         33 : begin
                TmpByte := TmpStack[0].i;
                SetBit(TmpByte, TmpStack[1].I);
                TmpStack[0].I := TmpByte;
              end; { SetBit }

         {-- ClearBit --------------------------------------------------------}
         34 : begin
                TmpByte := TmpStack[0].i;
                ClearBit(TmpByte, TmpStack[1].I);
                TmpStack[0].i := TmpByte;
              end; { ClearBit }

         {-- ReadBit ---------------------------------------------------------}
         35 : begin
                TmpByte := TmpStack[0].i;
                FuncReturn.B := ReadBit(TmpByte, TmpStack[1].I);
                TmpStack[0].i := TmpByte;
              end; { ReadBit }

         {-- OpenTextFile, DisplayHotFile, DisplayWithCtr --------------------}
         {$IFDEF WITH_FULL}
           36,                                                { OpenTextFile }
           37,                                              { DisplayHotFile }
           38 : begin                                        { DisplayWithCr }
                  {-- First get the string(s) ----------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- Make sure we get something usefull ------------------------}
                  if FuncNr = 37 then
                    begin
                      if CallTypes[2] = 'c' then
                        TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                          else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);
                    end; { DisplayHotFile }

                  {-- Now execute the function ----------------------------------}
                  Case FuncNr of
                    36 : begin
                           TmpStr := OpenTextFile(TmpStr);
                           SetReturnString(FuncReturn, TmpStr, false);
                         end; { OpenTextFile }
                    37 : begin
                           Str2Set(TmpStr2, TempSet);
                           FuncReturn.C := DisplayHotFile(TmpStr, TempSet);
                         end; { DisplayHotFile }
                    38 : DisplayWithCr(TmpStr);
                  end; { case }
                end; { OpenTextFile, DisplayHotFile, DisplayWithCr }
         {$ENDIF}

         {-- RetrieveExitinfo ------------------------------------------------}
         39 : begin
                Buf2Stack(elx_Globals, LineCfg^.Exitinfo^, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { RetrieveExitinfo }
       end; { case }
     end { else }

    {-- 40 .. 49 --------------------------------------------------------------}
    else if FuncNr < 50 then
     begin
       Case FuncNr of
         {-- PutExitinfo -----------------------------------------------------}
         40 : begin
                Stack2Buf(elx_Globals, LineCfg^.Exitinfo^, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { PutExitinfo }

         {-- SearchCombined --------------------------------------------------}
         {$IFDEF WITH_FULL}
           41 : begin
                  FuncReturn.B := SearchCombined(TmpStack[0].i);
                end; { SearchCombined }
         {$ENDIF}

         {-- ChangeGroup -----------------------------------------------------}
         {$IFDEF WITH_FULL}
           42 : begin
                  {-- Get optional data ----------------------------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- run the function -----------------------------------------}
                  ChangeGroup(TmpStack[0].B, TmpStr);
                end; { ChangeGroup }
         {$ENDIF}

         {-- ChangeFileArea --------------------------------------------------}
         {$IFDEF WITH_FULL}
           43 : begin
                  {-- Get optional data ----------------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- run the function -----------------------------------------}
                  ChangeFileArea(TmpStr, LineCfg^.Exitinfo^.Userinfo.FileArea);
                end; { ChangeFileArea }
         {$ENDIF}

         {-- ChangeMessageArea -----------------------------------------------}
         {$IFDEF WITH_FULL}
           44 : begin
                  {-- Get optional data ----------------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- run the function -----------------------------------------}
                  ChangeMessageArea(TmpStr);
                end; { ChangeMessageArea }
         {$ENDIF}

         {-- ShowMsgAreaNewMail ----------------------------------------------}
         {$IFDEF WITH_FULL}
           45 : begin
                  {-- Get optional data ----------------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- run the function -----------------------------------------}
                  ShowMsgAreaNewMail(TmpStr,                 { Optional data }
                                     TmpStack[1].B,               { Do Enter }
                                     TmpStack[2].B,              { Show ANSi }
                                     TmpStack[3].B);            { IsCombined }
                end; { ShowMsgAreaNewMail }
         {$ENDIF}

         {-- ClearCombined, SelectCombined, ToggleCombined -------------------}
         {$IFDEF WITH_FULL}
           46,                                               { ClearCombined }
           47,                                              { SelectCombined }
           48 : begin                                       { ToggleCombined }
                  {-- Get optional data ----------------------------------------}
                  if FuncNr <> 48 then
                    begin
                      if CallTypes[1] = 'c' then
                        TmpStr := Copy(TmpStack[0].C, 1, 1)
                          else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);
                    end; { if }

                  {-- run the function -----------------------------------------}
                  Case FuncNr of
                    46 : ClearCombined(TmpStr);
                    47 : SelectCombined(TmpStr);
                    48 : ToggleCombinedArea(TmpStack[0].I);
                  end; { case }
                end; { Clear, Select and ToggleCombinedArea }
         {$ENDIF}

         {-- CheckFlagAccess -------------------------------------------------}
         49 : begin
                {-- First convert the data to a record ------------------------}
{
FIXME
                Stack2Buf(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes,  2, 3)), 0, true);
                Stack2Buf(elx_Globals, TmpFlag_2, FVal(Copy(CallTypes,  6, 3)), 0, true);
                Stack2Buf(elx_Globals, TmpFlag_3, FVal(Copy(CallTypes, 10, 3)), 0, true);
}
                {-- Call the actual function ----------------------------------}
                FuncReturn.B := CheckFlagAccess(TmpFlag_1, TmpFlag_2, TmpFlag_3);

                {-- Convert the record back to data ---------------------------}
{
                Buf2Stack(elx_Globals, TmpFlag_1, FVal(Copy(CallTypes,  2, 3)), 0, true);
                Buf2Stack(elx_Globals, TmpFlag_2, FVal(Copy(CallTypes,  6, 3)), 0, true);
                Buf2Stack(elx_Globals, TmpFlag_3, FVal(Copy(CallTypes, 10, 3)), 0, true);
}
              end; { CheckFlagAccess }
       end; { case }
     end { else }

    {-- 50 .. 59 --------------------------------------------------------------}
    else if FuncNr < 60 then
     begin
       Case FuncNr of
         {-- CheckFileAreaAccess ----------------------------------------------}
         50 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpFileArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := CheckFileAreaAccess(TmpFileArea_1,   { Area }
                                                    TmpStack[1].B, { Downld }
                                                    TmpStack[2].B,  { Group }
                                                    TmpStack[3].B,   { Upld }
                                                    TmpStack[4].B,   { List }
                                                    TmpStack[5].I, { Group# }
                                                    LineCfg^.Exitinfo^);
              end; { CheckFileAreaAccess }

         {-- CheckGroupAccess -------------------------------------------------}
         51 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpGroup_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := CheckGroupAccess(TmpGroup_1, LineCfg^.Exitinfo^);
              end; { CheckFileAreaAccess }

         {-- CheckLanguageAccess ----------------------------------------------}
         52 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpLang_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := CheckLanguageAccess(TmpLang_1,
                                                    TmpStack[1].B,
                                                    LineCfg^.Exitinfo^);
              end; { CheckLanguageAccess }

         {-- CheckMessageAreaAccess -------------------------------------------}
         53 : begin
         fillchar(tmpmsgarea_1, sizeof(tmpmsgarea_1), #0);

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := CheckMsgAreaAccess(TmpMsgArea_1,
                                                   TmpStack[1].B,  { Group }
                                                   TmpStack[2].B,   { Read }
                                                   TmpStack[3].I, { Group# }
                                                   LineCfg^.Exitinfo^.UserInfo);
              end; { CheckMessageAreaAccess }

         {-- CheckMenuAccess --------------------------------------------------}
         54 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMenu_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := CheckMenuAccess(TmpMenu_1, LineCfg^.Exitinfo^);
              end; { CheckMenuAccess }

         {-- CheckReadMsgAccess -----------------------------------------------}
         55 : begin
                {-- Get the fromto --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := ReadMsgAccess(TmpStr,             { FromTo }
                                              TmpStack[1].B,   { IsPrivate }
                                              TmpStack[2].B, { SysopAccess }
                                              LineCfg^.Exitinfo^.UserInfo);
              end; { CheckReadMsgAccess }

         {-- CheckMsgSysOpAccess ----------------------------------------------}
         56 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ----------------------------------------------}
                FuncReturn.B := SysopAccess(TmpMsgArea_1,
                                            LineCfg^.Exitinfo^.UserInfo);
              end; { CheckMsgSysopAccess }

         {-- GiveTimeSlice ----------------------------------------------------}
         57 : DoSlice;

         {-- SetWindowTitle ---------------------------------------------------}
         58 : begin
                {-- Get the Window Title --------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now set the window title ----------------------------------}
                SetWindowTitle(TmpStr);
              end; { if }

         {-- GetSystemEnvironment ---------------------------------------------}
         59 : begin
                {-- now return the system environment -------------------------}
                SetReturnString(FuncReturn, GetSysEnv, false);
              end; { if }
       end; { case }
     end { else }

    {-- 60 .. 69 --------------------------------------------------------------}
    else if FuncNr < 70 then
     begin
       Case FuncNr of
         {-- MakeAttr ---------------------------------------------------------}
         60 : begin
                FuncReturn.I := MakeAttr(TmpStack[0].I, TmpStack[1].I);
              end; { MakeAttr }

         {-- GetForeAttr ------------------------------------------------------}
         61 : begin
                FuncReturn.I := GetForeAttr(TmpStack[0].I);
              end; { GetForeAttr }

         {-- GetBackAttr ------------------------------------------------------}
         62 : begin
                FuncReturn.I := GetBackAttr(TmpStack[0].I);
              end; { GetBackAttr }

         {-- GetColors --------------------------------------------------------}
         63 : begin
                TmpByte := TmpStack[1].I;
                TmpByte2 := TmpStack[2].i;
                GetColors(TmpStack[0].I,
                          TmpByte,
                          TmpByte2);

                TmpStack[1].I := TmpByte;
                TmpStack[2].I := TmpByte2;
              end; { GetColors }

         {-- RemoveRaColors, NoColorLength, GetLastColor ----------------------}
         64,                                              { RemoveRaColors }
         65,                                               { NoColorLength }
         66 : begin                                         { GetLastColor }
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the procedure -------------------------------------}
                Case FuncNr of
                  64 : begin
                         ShortTmp := TmpStr;
                         RemoveRaColors(ShortTmp);
                         TmpStr := ShortTmp;
                       end; { case }
                  65 : FuncReturn.I := NoColorLength(TmpStr);
                  66 : FuncReturn.I := GetLastColor(TmpStr);
                end; { case }

                {-- and set the return value ----------------------------------}
                if FuncNr = 64 then
                  SetReturnString(TmpStack[0], TmpStr, true);
              end; { GetForeAttr }

         {-- FastWrite --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           67 : begin
                  {-- Get the string --------------------------------------------}
                  if CallTypes[4] = 'c' then
                    TmpStr := Copy(TmpStack[3].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

                  {-- Now write the string --------------------------------------}
                  FastWrite(TmpStack[0].I,                                { X }
                            TmpStack[1].I,                                { Y }
                            TmpStack[2].I,                             { Attr }
                            TmpStr);
                end; { FastWrite }
         {$ENDIF}

         {-- BoxWindow --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           68 : begin
                  {-- Get the string --------------------------------------------}
                  if CallTypes[7] = 'c' then
                    TmpStr := Copy(TmpStack[6].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[6].S, 1, -1);

                  {-- Now create the box ----------------------------------------}
                  BoxWindow(TmpStack[0].I,                              { X1 }
                            TmpStack[1].I,                              { Y1 }
                            TmpStack[2].I,                              { X2 }
                            TmpStack[3].I,                              { Y2 }
                            TmpStack[4].I,                            { Fore }
                            TmpStack[5].I,                            { Back }
                            TmpStr);
                end; { BoxWindow }
         {$ENDIF}

         {-- ColorArea --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           69 : begin
                  {-- Now color the screen --------------------------------------}
                  ColorArea(TmpStack[0].I,                              { X1 }
                            TmpStack[1].I,                              { Y1 }
                            TmpStack[2].I,                              { X2 }
                            TmpStack[3].I,                              { Y2 }
                            TmpStack[4].I);                           { Attr }
                end; { ColorArea }
         {$ENDIF}
       end; { case }
     end { else }

    {-- 70 .. 80 --------------------------------------------------------------}
    else if FuncNr < 80 then
     begin
       Case FuncNr of
         {-- FillArea ---------------------------------------------------------}
         {$IFDEF WITH_FULL}
           70 : begin
                  {-- Now write the string --------------------------------------}
                  FillArea(TmpStack[0].I,                              { X1 }
                           TmpStack[1].I,                              { Y1 }
                           TmpStack[2].I,                              { X2 }
                           TmpStack[3].I,                              { Y2 }
                           TmpStack[4].C,                            { Char }
                           TmpStack[5].I);                           { Attr }
                end; { FillArea }
         {$ENDIF}

         {-- LocalScreen ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           71 : begin
                  {-- Get the string --------------------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- Now write the string --------------------------------------}
                  LocalScreen(TmpStr);
                end; { localscreen }
         {$ENDIF}

         {-- LocalScreenLn ----------------------------------------------------}
         {$IFDEF WITH_FULL}
           72 : begin
                  {-- Get the string --------------------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- Now write the string --------------------------------------}
                  LocalScreenLn(TmpStr);
                end; { LocalscreenLn }
         {$ENDIF}

         {-- DoBeep -----------------------------------------------------------}
         {$IFDEF WITH_FULL}
           73 : DoBeep;
         {$ENDIF}

         {-- GetScreen --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           74 : begin
                  {-- Now write the string --------------------------------------}
                  TmpByte := TmpStack[3].i;
                  GetScreen(TmpStack[0].I,                               { X }
                            TmpStack[1].I,                               { Y }
                            TmpStack[2].C,                            { Char }
                            TmpByte);                                 { Attr }
                  TmpStack[3].I := TmpByte;
                end; { GetScreen }
         {$ENDIF}

         {-- PutScreen --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           75 : begin
                  {-- Now write the string --------------------------------------}
                  PutScreen(TmpStack[0].I,                               { X }
                            TmpStack[1].I,                               { Y }
                            TmpStack[2].C,                            { Char }
                            TmpStack[3].I);                           { Attr }
                end; { PutScreen }
         {$ENDIF}

         {-- SendInteractiveMsg -----------------------------------------------}
         {$IFDEF WITH_FULL}
           76 : begin
                  {-- Get the string --------------------------------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- Now write the string --------------------------------------}
                  MultiLnObj^.SendInterActiveMsg(TmpStack[0].I, TmpStr);
                end; { SendInteractiveMsg }
         {$ENDIF}

         {-- CheckOkToSend ----------------------------------------------------}
         77 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpUseron_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- Now write the string --------------------------------------}
                FuncReturn.B := MultiLnObj^.CheckOkToSend(TmpStack[0].I,      { Linenr }
                                              TmpUserOn_1,
                                              false);       { Show prompts }
              end; { CheckOkToSend }

         {-- DoubleUserOn -----------------------------------------------------}
         78 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now write the string --------------------------------------}
                FuncReturn.B := MultiLnObj^.DoubleUserOn(TmpStr);
              end; { DoubleUserOn }

         {-- GetUserOn --------------------------------------------------------}
         79 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now write the string --------------------------------------}
                FuncReturn.I := MultiLnObj^.GetUserOn(TmpStr, TmpStack[1].B, false);
              end; { GetUserOn }
       end; { case }
     end { else }

    {-- 80 .. 90 --------------------------------------------------------------}
    else if FuncNr < 90 then
     begin
       Case FuncNr of
         {-- EmptyNodeNr ------------------------------------------------------}
         80 : begin
                FuncReturn.I := MultiLnObj^.EmptyNodeNr(TmpStack[0].B);
              end; { EmptyNodeNr }

         {-- WriteUserOn ------------------------------------------------------}
         81 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                 {-- Now actually write the description -----------------------}
                 MultiLnObj^.WriteUserOn(TmpStr, TmpStack[1].I);
              end; { WriteUserOn }

         {-- MakeDebugLogEntry ------------------------------------------------}
         82 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                 {-- Now actually write the description -----------------------}
                 {$IFDEF WITH_DEBUG}
                   DebugObj.DebugLog(LoggingType(TmpStack[0].I), TmpStr);
                 {$ELSE}
                   RaLog('#', TmpStr);
                 {$ENDIF}
              end; { MakeDebugObj.DebugLogEntry }

         {-- MakeLogEntry -----------------------------------------------------}
         83 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                 {-- Now actually write the description -----------------------}
                 RaLog(TmpStack[0].C, TmpStr);
              end; { MakeLogEntry }

         {-- LoadLimitInfo ----------------------------------------------------}
         84 : begin
                GetLevelLimitsInfo(TmpStack[0].I,              { Get level }
                                   TmpStack[1].B,            { Warning msg }
                                   TmpStack[2].B);           { Ignore time }
              end; { LoadLimitInfo }

         {-- LockOutUSer ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           85 : LockOutUsers;
         {$ENDIF}

         {-- SetTime ----------------------------------------------------------}
         {$IFDEF WITH_FULL}
           86 : begin
                  SetTime(TmpStack[0].I);
                end; { SetTime }
         {$ENDIF}

         {-- ChangeTime -------------------------------------------------------}
         {$IFDEF WITH_FULL}
           87 : begin
                  ChangeTime(TmpStack[0].I, TmpStack[0].B);
                end; { ChangeTime }
         {$ENDIF}

         {-- DownloadRestrictions ---------------------------------------------}
         {$IFDEF WITH_FULL}
           88 : begin
                  FuncReturn.B := DownloadRestrictions(TmpStack[0].B);
                end; { DownloadRestrictions }
         {$ENDIF}

         {-- CheckCarrierAndTime ----------------------------------------------}
         {$IFDEF WITH_FULL}
           89 : begin
                  FuncReturn.B := contrObj^.CheckAll;
                end; { CheckCarrierAndTime }
         {$ENDIF}
       end; { case }
     end { else }

    {-- 90 .. 100 -------------------------------------------------------------}
    else if FuncNr < 100 then
     begin
       Case FuncNr of
         {-- TimeRemain -------------------------------------------------------}
         {$IFDEF WITH_FULL}
           90 : begin
                  FuncReturn.I := contrObj^.TimeRemain;
                end; { TimeRemain }
         {$ENDIF}

         {-- CheckEventStatus -------------------------------------------------}
         {$IFDEF WITH_FULL}
           91 : begin
                  FuncReturn.B := contrObj^.CheckEventStatus;
                end; { CheckEventStatus }
         {$ENDIF}

         {-- CheckRaExit ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           92 : begin
                  FuncReturn.B := contrObj^.CheckRaExit;
                end; { CheckRaExit }
         {$ENDIF}

         {-- CheckIdle --------------------------------------------------------}
         {$IFDEF WITH_FULL}
           93 : begin
                  FuncReturn.B := contrObj^.CheckIdle;
                end; { CheckIdle }
         {$ENDIF}

         {-- WarnTimeLimit ----------------------------------------------------}
         {$IFDEF WITH_FULL}
           94 : begin
                  contrObj^.TimeWarn;
                end; { WarnTimeLimit }
         {$ENDIF}

         {-- CheckForEleMon ---------------------------------------------------}
         {$IFDEF WITH_FULL}
           95 : begin
                  contrObj^.CheckForEleMon;
                end; { CheckForEleMon }
         {$ENDIF}

         {-- TimeIsUpWarning --------------------------------------------------}
         {$IFDEF WITH_FULL}
           96 : begin
                  contrObj^.TimeOver;
                end; { TimeIsUpWarning }
         {$ENDIF}

         {-- ResetIdleTimeOut -------------------------------------------------}
         {$IFDEF WITH_FULL}
           97 : begin
                  contrObj^.SetTimeOut;
                end; { ResetIdleTimeOut }
         {$ENDIF}

         {-- SetIdleTimeOut ---------------------------------------------------}
         {$IFDEF WITH_FULL}
           98 : begin
                  contrObj^.SetIdleTimeLimit(TmpStack[0].i);
                end; { SetIdleTimeOut }
         {$ENDIF}

           99 : ;
       end; { case }
     end; { else }
end; { proc. OneToHundred }

procedure ToTwoHundred;
var CountVar: Longint;
begin
  if (FuncNr - 100) < 10 then
    begin
      Case (FuncNr - 100) of
         {-- TagGetHeader -----------------------------------------------------}
         1 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpTag_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- Now get the record ----------------------------------------}
                TaggingObj^.GetTagHeader(TmpStack[0].I,
                                      TmpTag_1);

                {-- and update the local content ------------------------------}
                Buf2Stack(elx_Globals, TmpTag_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));
             end; { TagGetHeader }

         {-- TagPutHeader -----------------------------------------------------}
         2 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpTag_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- Now get the record ----------------------------------------}
                TaggingObj^.SetTagHeader(TmpStack[0].I,
                                      TmpTag_1);

                {-- and update the local content ------------------------------}
                Buf2Stack(elx_Globals, TmpTag_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));
             end; { TagPutHeader }

         {-- TagFixFileName ---------------------------------------------------}
         3 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Fix the filename ------------------------------------------}
                ShortTmp := TmpStr;
                TaggingObj^.MakeUpName(ShortTmp);
                TmpStr := ShortTmp;

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[0], TmpStr, true);
             end; { TagFixFileName }

         {-- TagGetCurrentTagged ----------------------------------------------}
         4 : begin
               FuncReturn.I := TaggingObj^.CurrentTagged;
             end; { GetCurrentTagged }

         {-- TagGetTotalKbsTagged ---------------------------------------------}
         5 : begin
               FuncReturn.I := TaggingObj^.GetTotalKbsTagged;
             end; { TagGetTotalKbsTagged }

         {-- TagIsTagged ------------------------------------------------------}
         6 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the function --------------------------------------}
                FuncReturn.B := TaggingObj^.IsTaggedFile(TmpStr, Word(TmpStack[1].i));
             end; { TagIsTagged }

         {-- TagAddToTagFile --------------------------------------------------}
         7 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpFileHdr_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[4].S, 1, -1);

                {-- Now run the function --------------------------------------}
                FuncReturn.B := TaggingObj^.AddToTagFile(TmpFileHdr_1,
                                                      TmpStack[1].I,
                                                      TmpStack[2].I,
                                                      TmpStack[3].I,
                                                      TmpStr);
             end; { TagAddToTagFile }

         {-- TagDeleteFromTag -------------------------------------------------}
         8 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the function --------------------------------------}
                TaggingObj^.DeleteFileFromTagged(TmpStr);
             end; { TagDeleteFromTag }

         {-- TagDeleteTagNumber -----------------------------------------------}
         9 : begin
                {-- Now run the function --------------------------------------}
                TaggingObj^.DeleteTagNumber(TmpStack[0].i);
             end; { TagDeleteTagNumber }
      end; { case }
   end { FuncNr < 10 }

    {-- 10 .. 39 --------------------------------------------------------------}
    else if (FuncNr - 100) < 40 then
     begin
       Case (FuncNr - 100) of
         10 .. 36   { all sorts of string functions, all based on the same }
            : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                Case (FuncNr - 100) of
                  10 : TmpStr := ForceBack(TmpStr);
                  11 : TmpStr := NoExtension(TmpStr);
                  12 : TmpStr := JustExtension(TmpStr);
                  13 : TmpStr := JustPath(TmpStr);
                  14 : TmpStr := JustName(TmpStr);
                  15 : TmpStr := NoBackSlash(TmpStr);
                  16 : TmpStr := RdxName(TmpStr);
                  17 : TmpStr := FixTime(TmpStr);
                  18 : TmpStr := SlashStr(TmpStr);
                  19 : TmpStr := AsciiStr(TmpStr);
                  20 : TmpStr := Under2Norm(TmpStr);
                  21 : TmpStr := Space2Dot(TmpStr);
                  22 : TmpStr := Trim(TmpStr);
                  23 : TmpStr := TrimLeft(TmpStr);
                  24 : TmpStr := TrimRight(TmpStr);
                  25 : TmpStr := FirstNameOnly(TmpStr);
                  26 : TmpStr := LastNameOnly(TmpStr);
                  27 : TmpStr := InQuotes(TmpStr);
                  28 : TmpStr := InitialString(TmpStr);
                  29 : TmpStr := NoDoubleSpace(TmpStr);
                  30 : TmpStr := FixUserName(TmpStr);
                  31 : FuncReturn.R := StrToReal(TmpStr);
                  32 : FuncReturn.I := Str2Flags(TmpStr);
                  33 : FuncReturn.I := FVal(TmpStr);
                  34 : FuncReturn.B := IsFtpUrl(TmpStr);
                  35 : FuncReturn.C := LastChar(TmpStr);
                  36 : FuncReturn.C := FirstChar(TmpStr);
                end; { case }

                {-- Now set the return string value if applicable -----------}
                if (FuncNr - 100) < 31 then
                  SetReturnString(FuncReturn, TmpStr, false);
              end; { string functions }
       end; { case }
     end  { else }

    {-- 40 .. 59 --------------------------------------------------------------}
    else if (FuncNr - 100) < 60 then
     begin
       Case (FuncNr - 100) of
         40 .. 53   { all sorts of string functions, all based on the same }
            : begin
                {-- Now run the desired function ------------------------------}
                Case (FuncNr - 100) of
                  40 : TmpStr := HexLong(TmpStack[0].i);
                  41 : TmpStr := HexStr(TmpStack[0].i);
                  42 : TmpStr := ToHex(TmpStack[0].i);
                  43 : TmpStr := FStr(TmpStack[0].i);
                  44 : TmpStr := FormatMinSec(TmpStack[0].i);
                  45 : TmpStr := Word2Time(TmpStack[0].i);
                  46 : TmpStr := CommaStr(TmpStack[0].i);
                  47 : TmpStr := Byte2Flags(TmpStack[0].i);
                  48 : TmpStr := Byte2FlagsOff(TmpStack[0].i, TmpStack[1].i);
                  49 : FuncReturn.I := FixBaud(TmpStack[0].i);
                  50 : TmpStr := LeadingZero(TmpStack[0].i, TmpStack[1].i);
                  51 : TmpStr := Real2Str(TmpStack[0].R, TmpStack[1].I, TmpStack[2].I);
                  52 : TmpStr := Dup(TmpStack[0].C, TmpStack[1].i);
                end; { case }

                {-- Now set the return string value if applicable -----------}
                if (FuncNr - 100) <> 49 then
                  SetReturnString(FuncReturn, TmpStr, false);
              end; { string functions }
       end; { case }
     end  { else }

    {-- 60 .. 79 --------------------------------------------------------------}
    else if (FuncNr - 100) < 80 then
     begin
       Case (FuncNr - 100) of
         60 .. 72   { all sorts of string functions, all based on the same }
            : begin
                {-- First get the string we want ------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                Case (FuncNr - 100) of
                  60 : TmpStr := MakeLen(TmpStr,
                                         TmpStack[1].I,
                                         FillUp(TmpStack[2].I),
                                         TmpStack[3].B,
                                         TmpStack[4].B);
                  61 : TmpStr := DotPadding(TmpStr,
                                            TmpStack[1].I,
                                            TmpStack[2].B,
                                            TmpStack[3].C);
                  62 : TmpStr := GetCommaArgument(TmpStr, TmpStack[1].I);
                  63 : TmpStr := RaduCenter(TmpStr, TmpStack[1].I);
                  64 : TmpStr := RightJust(TmpStr, TmpStack[1].I);
                  65 : TmpStr := LeftJust(TmpStr, TmpStack[1].I);
                  66 : TmpStr := LeftJustChar(TmpStr,
                                              TmpStack[1].I,
                                              TmpStack[2].C);
                  67 : TmpStr := CenterJust(TmpStr,
                                            TmpStack[1].I,
                                            TmpStack[2].B);
                  68 : TmpStr := CenterJustChar(TmpStr,
                                                TmpStack[1].I,
                                                TmpStack[2].C,
                                                TmpStack[3].B);
                  69 : TmpStr := RightJustChar(TmpStr,
                                               TmpStack[1].I,
                                               TmpStack[2].C);
                  70 : TmpStr := NoColorCopy(TmpStr,
                                             TmpStack[1].I,
                                             TmpStack[2].I);
                  71 : begin
                         ShortTmp := TmpStr;
                         AddrToString(ShortTmp,
                                      TmpStack[0].I,             { Zone }
                                      TmpStack[1].I,             { net }
                                      TmpStack[2].I,             { node }
                                      TmpStack[3].I);            { point }
                         TmpStr := ShortTmp;
                       end; { 71 }
                  72 : begin
                         TmpSmallWord := TmpStack[0].I;
                         TmpSmallWord2 := TmpStack[1].I;
                         TmpSmallWord3 := TmpStack[2].I;
                         TmpSmallWord4 := TmpStack[3].I;

                         ShortTmp := TmpStr;
                         StringToAddr(ShortTmp,
                                      TmpSmallWord,                { Zone }
                                      TmpSmallWord2,               { net }
                                      TmpSmallWord3,               { node }
                                      TmpSmallWord4);              { point }
                         TmpStr := ShortTmp;

                         TmpStack[0].I := TmpSmallWord;
                         TmpStack[1].I := TmpSmallWord2;
                         TmpStack[2].I := TmpSmallWord3;
                         TmpStack[3].I := TmpSmallWord4;
                       end; { StringToAddr }
                end; { case }

                {-- Now set the return string value if applicable -----------}
                if (FuncNr - 100) <> 72 then
                  SetReturnString(FuncReturn, TmpStr, false)
                    else SetReturnString(TmpStack[0], TmpStr, true);
              end; { string functions }

         {-- FilterString, RemoveChars, ... -----------------------------------}
         73 .. 77
            : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the char delimitting set --------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- and convert the delimitting set of a charset --------------}
                Str2Set(TmpStr2, TempSet);

                {-- now run the desired function ------------------------------}
                ShortTmp := TmpStr;
                Case (FuncNr - 100) of
                  73 : ShortTmp := FilterString(ShortTmp, TempSet);
                  74 : RemoveChars(ShortTmp, TempSet);
                  75 : TmpStack[0].I := WordCount(ShortTmp, TempSet, TmpStack[2].B);
                  76 : TmpStr := FirstWord(ShortTmp, TempSet, TmpStack[2].B);
                  77 : TmpStr := LastWord(ShortTmp, TempSet, TmpStack[2].B);
                end; { case }
                TmpStr := ShortTmp;

                {-- and set the return value ----------------------------------}
                if (FuncNr - 100) in [73, 74] then { filterstring, rmv-chars }
                  SetReturnString(TmpStack[0], TmpStr, true);

                if (FuncNr - 100) in [76] then
                  SetReturnString(FuncReturn, TmpStr, false);
              end; { FilterString, RemoveChars }

         {-- RemoveWordNr -----------------------------------------------------}
         78 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStrA := Copy(TmpStack[0].C, 1, 1)
                    else TmpStrA := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the char delimitting set --------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- and convert the delimitting set of a charset --------------}
                Str2Set(TmpStr2, TempSet);

                {-- Now call the procedure ------------------------------------}
                {$IFDEF MSDOS}
                  RemoveWordNr(TmpStr, TmpStack[1].I, TempSet, TmpStack[3].B);
                {$ELSE}
                  RemoveWordNrA(TmpStrA, TmpStack[1].I, TempSet, TmpStack[3].B);
                {$ENDIF}

                {-- set the value ---------------------------------------------}
                SetReturnString(TmpStack[0], TmpStrA, true);
              end; { RemoveWordNr }

         {-- Replace ----------------------------------------------------------}
         79 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- and after all, get the third ------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr3 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Now find the instance -------------------------------------}
                if TmpStack[3].B then
                  begin
                    TmpLong := Pos(SupCaseA(TmpStr), SupCaseA(TmpStr3))
                  end
                    else TmpLong := Pos(TmpStr, TmpStr3);

                {-- actually replace ------------------------------------------}
                if TmpLong > 0 then
                  begin
                    Delete(TmpStr3, TmpLong, Length(TmpStr));
                    Insert(TmpStr2, TmpStr3, TmpLong);
                  end; { if }

                {-- set the value ---------------------------------------------}
                SetReturnString(TmpStack[2], TmpStr3, true);
              end; { Replace }
       end; { case }

     end  { else }

    {-- 80 .. 99 --------------------------------------------------------------}
    else if (FuncNr - 100) < 100 then
     begin
       Case (FuncNr - 100) of
         {-- DoWrap -----------------------------------------------------------}
         80 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Now call the procedure ------------------------------------}
                {$IFDEF MSDOS}
                  DoWrap(TmpStr, TmpStr2, TmpStack[2].I);
                {$ELSE}
                  DoWrapA(TmpStr, TmpStr2, TmpStack[2].I);
                {$ENDIF}

                {-- set the value ---------------------------------------------}
                SetReturnString(TmpStack[0], TmpStr, true);
                SetReturnString(TmpStack[1], TmpStr2, true);
              end; { DoWrap }

         {-- FullInsert -------------------------------------------------------}
         81 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Now call the procedure ------------------------------------}
                {$IFDEF MSDOS}
                  FullInsert(TmpStr, TmpStr2, TmpStack[2].I);
                {$ELSE}
                  AnsiFullInsert(TmpStr, TmpStr2, TmpStack[2].I);
                {$ENDIF}

                {-- set the value ---------------------------------------------}
                SetReturnString(TmpStack[1], TmpStr2, true);
              end; { FullInsert }

         {-- GetValue ---------------------------------------------------------}
         82 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Now call the procedure ------------------------------------}
                ShortTmp := TmpStr2;
                TmpStr := GetValue(TmpStr, ShortTmp, TmpStack[2].B);
                TmpStr2 := ShortTmp;

                {-- set the value ---------------------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
                SetReturnString(TmpStack[1], TmpStr2, true);
              end; { GetValue }

         {-- ExtractWord ------------------------------------------------------}
         83 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- and convert the delimitting set of a charset --------------}
                Str2Set(TmpStr2, TempSet);

                {-- Now call the procedure ------------------------------------}
                TmpStr := ExtractWord(TmpStr,
                                      TmpStack[1].I,
                                      TempSet,
                                      TmpStack[3].B,
                                      TmpStack[4].B);

                {-- set the value ---------------------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { ExtractWord }

         {-- SupCase ----------------------------------------------------------}
         84 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                {$IFDEF MSDOS}
                  TmpStr := SUpCase(TmpStr);
                {$ELSE}
                  TmpStr := AnsiUpperCase(TmpStr);
                {$ENDIF}

                {-- Now set the return string value if applicable -----------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { SUpCase }

         {-- SLowCase----------------------------------------------------------}
         85 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                {$IFDEF MSDOS}
                  TmpStr := SLowCase(TmpStr);
                {$ELSE}
                  TmpStr := AnsiLowerCase(TmpStr);
                {$ENDIF}

                {-- Now set the return string value if applicable -----------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { SLowCase }

         {-- FirstUpper -------------------------------------------------------}
         86 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                TmpStr := FirstUpper(TmpStr);

                {-- Now set the return string value if applicable -----------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { FirstUpper }

         {-- StrCrc -----------------------------------------------------------}
         87 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                FuncReturn.I := RaCrc(TmpStr, false);
              end; { StrCrc }

         {-- ReplaceArray -----------------------------------------------------}
         88 : begin
                {-- Determine the array offset --------------------------------}
                TmpLong := FVal(Copy(CallTypes, 3, 4));

                {-- now we have the first array, get the second ---------------}
                TmpLong2 := FVal(Copy(CallTypes, 9, 4));

                {-- now get the second string ---------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr3 := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- and start looping through it ------------------------------}
                with elx_Globals do
                 with elx_Globals.ArrayTable^[IdentTable^[TmpLong].Ref].ArrayInf do
                  for CountVar := Low to High do
                    begin
                       {-- get the string -------------------------------------}
                       {$IFNDEF MSDOS}
                         TmpStr := StrPas(PChar(StringTable^[Stack[IdentTable^[TmpLong].Adr +
                                                   Display^[IdentTable^[TmpLong].Lev] + (CountVar - Low)].S]^.Content));
                         TmpStr2 := StrPas(PChar(StringTable^[Stack[IdentTable^[TmpLong2].Adr +
                                                   Display^[IdentTable^[TmpLong2].Lev] + (CountVar - Low)].S]^.Content));
                       {$ELSE}
                         TmpStr := StringTable^[Stack[IdentTable^[TmpLong].Adr +
                                                   Display^[IdentTable^[TmpLong].Lev] + (CountVar - Low)].S]^.Content;
                         TmpStr2 := StringTable^[Stack[IdentTable^[TmpLong2].Adr +
                                                   Display^[IdentTable^[TmpLong2].Lev] + (CountVar - Low)].S]^.Content;
                       {$ENDIF}

                       {-- now find all instances, and replace them -----------}
                       TmpWord := 1;
                       TmpWord2 := 1;

                       while TmpWord > 0 do
                         begin
                           {-- Now find the instance ------------------------------}
                           if TmpStack[3].B then
                             begin
                               TmpWord := Pos(SupCaseA(TmpStr), SupCaseA(Copy(TmpStr3, TmpWord2, Length(TmpStr3))))
                             end
                               else TmpWord := Pos(TmpStr, Copy(TmpStr3, TmpWord2, Length(TmpStr3)));

                           {-- and actually replace if necessary ------------------}
                           if TmpWord > 0 then
                             begin
                               TmpWord := TmpWord + TmpWord2;

                               Delete(TmpStr3, TmpWord - 1, Length(TmpStr));
                               Insert(TmpStr2, TmpStr3, TmpWord - 1);
                             end; { if }

                           {-- Save this to prevent a loop when a macro is replaced}
                           {-- with another macro ---------------------------------}
                           TmpWord2 := (TmpWord + Length(TmpStr2)) - 1;
                         end; { while }
                    end; { for }

                {-- set the value ---------------------------------------------}
                SetReturnString(TmpStack[0], TmpStr3, true);
              end; { ReplaceArray }

         {-- md5 --------------------------------------------------------------}
         {$IFDEF FPC}
         89 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                TmpStr := md5print(md5string(TmpStr));

                {-- Now set the return string value if applicable -----------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { md5 }
         {$ENDIF}

         {-- shell ------------------------------------------------------------}
         {$IFDEF FPC}
         90 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now run the desired function ------------------------------}
                Exec('', TmpStr);
              end; { shell }
         {$ENDIF}
       end; { case }
     end; { else }
end; { proc. ToTwoHundred }

procedure ToThreeHundred;
begin
  if (FuncNr - 200) < 20 then
    begin
      Case (FuncNr - 200) of
         {-- GetHour ----------------------------------------------------------}
         1 : FuncReturn.I := GetHour;

         {-- GetSecs ----------------------------------------------------------}
         2 : FuncReturn.I := GetSecs;

         {-- GetMonth ---------------------------------------------------------}
         3 : FuncReturn.I := GetMonth;

         {-- GetYear ----------------------------------------------------------}
         4 : FuncReturn.I := GetYear;

         {-- GetDay -----------------------------------------------------------}
         5 : FuncReturn.I := GetDay;

         {-- GetMins ----------------------------------------------------------}
         6 : FuncReturn.I := GetMins;

         {-- GetUserAge -------------------------------------------------------}
         7 : FuncReturn.I := GetUserAge(LineCfg^.Exitinfo^.userinfo.Birthdate, true);

         {-- GetDow -----------------------------------------------------------}
         8 : FuncReturn.I := GetHour;

         {-- Unix2Date --------------------------------------------------------}
         9 : FuncReturn.I := Unix2Date(TmpStack[0].I);

         {-- Date2Unix --------------------------------------------------------}
         10 : FuncReturn.I := Date2Unix(TmpStack[0].i);

         {-- Norm2Unix --------------------------------------------------------}
         11 : FuncReturn.I := Norm2Unix(TmpStack[0].I,
                                        TmpStack[1].I,
                                        TmpStack[2].I,
                                        TmpStack[3].I,
                                        TmpStack[4].I,
                                        TmpStack[5].I);

         {-- NowAsUnixDate ----------------------------------------------------}
         12 : FuncReturn.I := NowAsUnixDate;

         {-- CurrentTime ------------------------------------------------------}
         13 : FuncReturn.I := CurrentTime;

         {-- UnixToRfcDate ----------------------------------------------------}
         14 : begin
                {-- Get the string --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get the actaul value --------------------------------------}
                TmpStr := UnixToRfcDate(TmpStack[0].i, TmpStr);

                {-- now return it ---------------------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { UnixToRfcDate }

         {-- GetMicroTimer ----------------------------------------------------}
         {$IFNDEF MSDOS}
         15 : FuncReturn.R := GetMicroSeconds;
         {$ENDIF}

         {-- Ceil() -----------------------------------------------------------}
         16 : begin
                FuncReturn.R := Trunc(Tmpstack[0].R);
                if Frac(TmpStack[0].R) > 0 then
                  FuncReturn.R := Trunc(TmpStack[0].R) + 1;
              end; { if }

         {-- Floor() ----------------------------------------------------------}
         17 : begin
                FuncReturn.R := Trunc(Tmpstack[0].R);
                if Frac(TmpStack[0].R) < 0 then
                  FuncReturn.R := Trunc(TmpStack[0].R) - 1;
              end; { if }
      end; { case }
   end { FuncNr < 20 }

    {-- 20 .. 39 --------------------------------------------------------------}
    else if (FuncNr - 200) < 40 then
     begin
       Case (FuncNr - 200) of
         {-- DateStr ----------------------------------------------------------}
         20 : SetReturnString(FuncReturn, DateStr, false);

         {-- DateStrY2k -------------------------------------------------------}
         21 : SetReturnString(FuncReturn, DateStrY2k, false);

         {-- TimeStr ----------------------------------------------------------}
         22 : SetReturnString(FuncReturn, TimeStr(TmpStack[0].B, TmpStack[1].B), false);

         {-- Date2Str ---------------------------------------------------------}
         23 : SetReturnString(FuncReturn, Date2Str(TmpStack[0].I), false);

         {-- GetDayOfWeek -----------------------------------------------------}
         24 : SetReturnString(FuncReturn, GetDayOfWeek, false);

         {-- FutureDate -------------------------------------------------------}
         25 : SetReturnString(FuncReturn, FutureDate(TmpStack[0].I), false);

         {-- MinsIsTime -------------------------------------------------------}
         26 : SetReturnString(FuncReturn, MinsIsTime(TmpStack[0].I), false);

         {-- IsLeapYear -------------------------------------------------------}
         27 : FuncReturn.B := IsLeapYear(TmpStack[0].I);

         {-- MinsTillTime, MinsFromTime, ... ----------------------------------}
         28 .. 33
            : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string (if necessary) ------------------}
                if (FuncNr - 200) = 33 then
                  begin
                    if CallTypes[2] = 'c' then
                      TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                        else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);
                  end; { if }

                {-- Now call the function -------------------------------------}
                Case (FuncNr - 200) of
                  28 : FuncReturn.I := MinsTillTime(TmpStr);
                  29 : FuncReturn.I := MinsFromTime(TmpStr);
                  30 : FuncReturn.I := DaysAgo(TmpStr);
                  31 : FuncReturn.I := DaysToGo(TmpStr);
                  32 : FuncReturn.I := MonthNameToNum(TmpStr);
                  33 : FuncReturn.I := PackTimeStr(TmpStr, TmpStr2);
                end; { case }
              end; { MinsTillTime .. PackTimeStr }

         {-- Unix2Norm --------------------------------------------------------}
         34 : begin
                TmpWord := TmpStack[1].I;
                TmpWord2 := TmpStack[2].I;
                TmpWord3 := TmpStack[3].I;
                TmpWord4 := TmpStack[4].I;
                TmpWord5 := TmpStack[5].I;
                TmpWord6 := TmpStack[6].I;

                Unix2Norm(TmpStack[0].I,
                          TmpWord,
                          TmpWord2,
                          TmpWord3,
                          TmpWord4,
                          TmpWord5,
                          TmpWord6);

                TmpStack[1].I := TmpWord;
                TmpStack[2].I := TmpWord2;
                TmpStack[3].I := TmpWord3;
                TmpStack[4].I := TmpWord4;
                TmpStack[5].I := TmpWord5;
                TmpStack[6].I := TmpWord6;
              end; { Unix2Norm }

         {-- SetStartTime -----------------------------------------------------}
         {$IFDEF WITH_FULL}
         35 : contrObj^.SetStartTime;
         {$ENDIF}

         {-- NntpDateToString -------------------------------------------------}
         36 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- and after all, get the third ------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr3 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Call the actual procedure ---------------------------------}
                ShortTmp := TmpStr2;
                ShortTmp2 := TmpStr3;
                NntpDateToString(TmpStr, ShortTmp, ShortTmp2);
                TmpStr2 := ShortTmp;
                TmpStr3 := ShortTmp2;

                {-- Now set the return values ---------------------------------}
                SetReturnString(TmpStack[1], TmpStr2, true);
                SetReturnString(TmpStack[2], TmpStr3, true);
              end; { NntpDateToString }

         {-- HistoryDate ------------------------------------------------------}
         37 : SetReturnString(FuncReturn, HistoryDate(TmpStack[0].I), false);

         {-- Gettime ----------------------------------------------------------}
         38 : begin
                TmpWord := TmpStack[0].I;
                TmpWord2 := TmpStack[1].I;
                TmpWord3 := TmpStack[2].I;
                TmpWord4 := TmpStack[3].I;

                GetTime(TmpWord, TmpWord2, TmpWord3, TmpWord4);

                TmpStack[0].I := TmpWord;
                TmpStack[1].I := TmpWord2;
                TmpStack[2].I := TmpWord3;
                TmpStack[3].I := TmpWord4;
              end; { 38 }
      end; { case }
   end { FuncNr < 40 }

    {-- 40 .. 49 --------------------------------------------------------------}
    else if (FuncNr - 200) < 50 then
     begin
       Case (FuncNr - 200) of
         {-- GetUserRecordNr --------------------------------------------------}
         40 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 8, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 12, 1))));

                {-- Call the actual procedure ---------------------------------}
                GetUserRecord(TmpStack[0].I,
                              TmpUser_1,
                              TmpUserExt_2,
                              TmpStack[1].B);

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 8, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 12, 1))));
              end; { GetUserRecordNr }

         {-- UpdateUserRecord -------------------------------------------------}
         41 : UpdateUserRecord;

         {-- WriteUserRecord --------------------------------------------------}
         42 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 8, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 12, 1))));

                {-- Call the actual procedure ---------------------------------}
                WriteUserRecord(TmpStack[0].I,
                                TmpUser_1,
                                TmpUserExt_2);

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 8, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 12, 1))));
              end; { WriteUserRecord }

         {-- SetNewUserDefaults -----------------------------------------------}
         43 : begin
                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));

                {-- Call the actual procedure ---------------------------------}
                SetNewUserDefaults(TmpUser_1, TmpUserExt_2);

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));
              end; { SetNewUserDefaults }

         {-- SearchUser -------------------------------------------------------}
         44 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- And now call the actual procedure -------------------------}
                FuncReturn.I := SearchUser(TmpStr);
              end; { SearchUser }

         {-- GetScreenLength --------------------------------------------------}
         45 : FuncReturn.I := LineCfg^.Exitinfo^.Userinfo.ScreenLength;

         {-- GetUserRecord ----------------------------------------------------}
         46 : begin
                {-- Call the actual procedure ---------------------------------}
                TmpUser_1 := LineCfg^.Exitinfo^.UserInfo;
                TmpUserExt_2 := LineCfg^.UserExtension^;

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUser_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Convert the record back to data ---------------------------}
                Buf2Stack(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes, 7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));
              end; { GetUserRecord }

         {-- GetRecSize -------------------------------------------------------}
         47 : begin
                {-- Return the size -------------------------------------------}
                with elx_Globals do
                  begin
                    TmpLong := IdentTable^[FVal(Copy(CallTypes, 2, 4))].Ref;
                    FuncReturn.I := BlockTable^[TmpLong].ByteSize
                  end; { with }
              end; { getRecSize }
      end; { case }
   end { FuncNr < 50 }

    {-- 50 .. 79 --------------------------------------------------------------}
    else if (FuncNr - 200) < 80 then
     begin
       Case (FuncNr - 200) of
         {-- FileCount .. UpToDateFile ----------------------------------------}
         50 .. 68
            : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now get the second string ---------------------------------}
                if (FuncNr - 200) > 63 then
                  begin
                    if CallTypes[2] = 'c' then
                      TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                        else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);
                  end; { if }

                {-- and use it ------------------------------------------------}
                Case (FuncNr - 200) of
                  50 : FuncReturn.I := FileCount(TmpStr);
                  51 : FuncReturn.I := GetPackedFileTime(TmpStr);
                  52 : FuncReturn.I := GetFileSize(TmpStr, TmpStack[1].I);
                  53 : FuncReturn.B := EraseFile(TmpStr);
                  54 : FuncReturn.B := FileExist(TmpStr);
                  55 : FuncReturn.B := SemaExist(TmpStr);
                  56 : FuncReturn.B := OpenFile(TmpStr, TmpStack[1].I);
                  57 : TmpStr := GetFileDate(TmpStr);
                  58 : TmpStr := GetVolumeLabel(TmpStr);
                  59 : TmpStr := OpenRaFile(TmpStr);
                  60 : TmpStr := CreateTempDir(TmpStr, TmpStack[1].I);
                  61 : TmpStr := GetLogFileName;
                  62 : MakeDir(TmpStr);
                  63 : { --- };
                  64 : FuncReturn.B := IsWildCard(TmpStr, TmpStr2);
                  65 : FuncReturn.B := RenameFile(TmpStr, TmpStr2);
                  66 : FuncReturn.B := FileCopy(TmpStr, TmpStr2, TmpStack[2].B, TmpStack[3].B);
                  67 : FuncReturn.B := SearchCtlFile(TmpStr, TmpStr2, TmpStack[2].B);
                  68 : FuncReturn.B := UpToDateFile(TmpStr, TmpStr2);
                end; { case }

                {-- and set a resulting string if necessary ------------------}
                if ((FuncNr - 200) < 62) AND ((FuncNr - 200) > 56) then
                  SetReturnString(FuncReturn, TmpStr, false);
              end; { FileCount .. UpToDateFile }

         {-- Groupname -------------------------------------------------------}
         69 : SetReturnString(FuncReturn, GroupName(TmpStack[0].I, TmpStack[1].B), false);

         {-- GetCurDir -------------------------------------------------------}
         70 : SetReturnString(FuncReturn, GetCurDir, false);

         {-- GetDiskFree -----------------------------------------------------}
         71 : FuncReturn.I := GetDiskFree(TmpStack[0].C);

         {-- Ra250Area -------------------------------------------------------}
         72 : FuncReturn.I := Ra250Area(TmpStack[0].i);

         {-- Ra250MsgArea ----------------------------------------------------}
         73 : FuncReturn.I := Ra250MsgArea(TmpStack[0].i);

         {-- SearchNextMsgArea -----------------------------------------------}
         {$IFDEF WITH_FULL}
           74 : FuncReturn.I := SearchNextMsgArea(TmpStack[0].i);
         {$ENDIF}

         {-- Ra250Group ------------------------------------------------------}
         75 : FuncReturn.I := Ra250Group(TmpStack[0].i, TmpStack[0].B);

         {-- FindFirst -------------------------------------------------------}
         76 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- initiate the search --------------------------------------}
                {$IFNDEF DELPHI}
                  Dos.FindFirst(TmpStr, TmpStack[2].I, objPtr^.elx_FindData[TmpStack[0].I]);
                {$ELSE}
                  GenDos.FindFirst(TmpStr, TmpStack[2].I, objPtr^.elx_FindData[TmpStack[0].I]);
                {$ENDIF}

                {-- and return the error -------------------------------------}
                FuncReturn.I := DosError;
              end; { FindFirst }

         {-- Findnext --------------------------------------------------------}
         77 : begin
                {-- initiate the search --------------------------------------}
                {$IFNDEF DELPHI}
                  Dos.FindNext(objPtr^.elx_FindData[TmpStack[0].I]);
                {$ELSE}
                  GenDos.FindNext(objPtr^.elx_FindData[TmpStack[0].I]);
                {$ENDIF}

                {-- and return the error -------------------------------------}
                FuncReturn.I := DosError;
              end; { FindNext }

         {-- FindClose -------------------------------------------------------}
         78 : begin
                {-- initiate the search --------------------------------------}
                {$IFNDEF MSDOS}
                  {$IFNDEF DELPHI}
                    Dos.FindClose(objPtr^.elx_FindData[TmpStack[0].I]);
                  {$ELSE}
                    {GenDos.FindClose(objPtr^.elx_FindData[TmpStack[0].I]);}
                  {$ENDIF}
                {$ENDIF}
              end; { FindNext }

         {-- FindGetName -----------------------------------------------------}
         79 : begin
                {-- initiate the search --------------------------------------}
                SetReturnString(FuncReturn, objPtr^.elx_FindData[TmpStack[0].i].Name, false);
              end; { FindNext }
      end; { case }
   end { FuncNr < 80 }

    {-- 80 .. 100 -------------------------------------------------------------}
    else if (FuncNr - 200) < 100 then
     begin
       Case (FuncNr - 200) of
         {-- WriteExitInfo ----------------------------------------------------}
         {$IFDEF WITH_FULL}
           80 : WriteExitInfo(LineCfg^.Exitinfo^);
         {$ENDIF}

         {-- UpdateStatistics -------------------------------------------------}
         81 : UpdateStatistics;

         {-- BuildFastIndex ---------------------------------------------------}
         82 : BuildFastIndex;

         {-- BuildLastRead ----------------------------------------------------}
         83 : BuildLastRead;

         {-- InitSystemNames --------------------------------------------------}
         84 : InitSystemNames;

         {-- DoRaBusy ---------------------------------------------------------}
         85 : DoRaBusy(TmpStack[0].B);

         {-- ReadLanguageRA ---------------------------------------------------}
         86 : ReadLanguageRA(TmpStack[0].I);

         {-- CreateDorInfoDef -------------------------------------------------}
         {$IFDEF WITH_FULL}
           87 : CreateDorInfoDef(TmpStack[0].B,
                                 TmpStack[1].B,
                                 TmpStack[2].I);
         {$ENDIF}

         {-- CreateDoorSys ----------------------------------------------------}
         {$IFDEF WITH_FULL}
           88 : CreateDoorSys(TmpStack[0].I);
         {$ENDIF}

         {-- CreateDoor32Sys --------------------------------------------------}
         {$IFDEF WITH_FULL}
           89 : CreateDoor32Sys(TmpStack[0].I);
         {$ENDIF}

         {-- RemoveSema .. CreateTemplateFile ---------------------------------}
         90 .. 95
            : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Now execute the actual procedure --------------------------}
                Case (FuncNr - 200) of
                  90 : RemoveSema(TmpStr);
                  91 : CreateSema(TmpStr);
                  92 : KillCompleteDir(TmpStr, TmpStack[1].B);
                  93 : EraseDir(TmpStr, TmpStack[1].B);
                  94 : SetFileTime(TmpStr);
                  {$IFDEF WITH_FULL}
                    95 : CreateTemplateFile(TmpStr);
                  {$ENDIF}
                end; { case }
              end; { RemoveSema .. CreateTemplateFile }

         {-- WriteMsgInf ------------------------------------------------------}
         96 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr3 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Get string four -------------------------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr4 := Copy(TmpStack[3].C, 1, 1)
                    else TmpStr4 := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

                {-- Get string five -------------------------------------------}
                if CallTypes[5] = 'c' then
                  TmpStr5 := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr5 := StrGetString(elx_Globals, TmpStack[4].S, 1, -1);

                {-- Now just call the procedure we want -----------------------}
                WriteMsgInf(TmpStr,                 { FromWho }
                            TmpStr2,                { ToWho }
                            TmpStr3,                { Subject }
                            TmpStr4,                { MsgNr }
                            TmpStr5,                { BoardName }
                            TmpStack[5].B);         { MsgPrivate }
              end; { WriteMsgInf }

         {-- EleCodeStr -------------------------------------------------------}
         97 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now convert it --------------------------------------------}
                termObj^.ParseRaCodeStr(TmpStr);

                {-- and set the function result -------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { EleCodeStr }

         {-- GetSysopName -----------------------------------------------------}
         98 : begin
                {-- and set the function result -------------------------------}
                SetReturnString(FuncReturn, GlobalCfg^.RaConfig^.Sysop, false);
              end; { GetSysopName }

         {-- GetSystemName ----------------------------------------------------}
         99 : begin
                {-- and set the function result -------------------------------}
                SetReturnString(FuncReturn, GlobalCfg^.RaConfig^.SystemName, false);
              end; { GetSystemName }
       end; { case }
     end; { if }
end; { proc. ToThreeHundred }


procedure ToFourHundred;
begin
  if (FuncNr - 300) < 20 then
    begin
      Case (FuncNr - 300) of
        {-- GetFilesRecord ----------------------------------------------------}
        1 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpFileArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              FuncReturn.B := GetFilesRecord(TmpFileArea_1,
                                             TmpStack[1].I,  { AreaNr }
                                             TmpStack[2].B); { CheckRDX }

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpFileArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

            end; { GetFilesRecord }

        {-- GetEleFilesRecord -------------------------------------------------}
        2 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpEleFileArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              FuncReturn.B := GetEleFilesRecord(TmpEleFileArea_1,
                                                TmpStack[1].I,  { AreaNr }
                                                TmpStack[2].B); { CheckRDX }

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpEleFileArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { GetEleFilesRecord }

        {-- GetMessageRecord --------------------------------------------------}
        3 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              FuncReturn.B := GetMessageRecord(TmpMsgArea_1,
                                               TmpStack[1].I,  { AreaNr }
                                               TmpStack[2].B); { CheckRDX }

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { GetMessageRecord }

        {-- GetEleMessageRecord -----------------------------------------------}
        4 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpEleMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              FuncReturn.B := GetEleMessageRecord(TmpEleMsgArea_1,
                                                  TmpStack[1].I,  { AreaNr }
                                                  TmpStack[2].B); { CheckRDX }

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpEleMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { GetEleMessageRecord }

        {-- GetGroupRecord ----------------------------------------------------}
        5 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpGroup_1, FVal(Copy(CallTypes,  5, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 9, 1))));

              {-- Now check it ------------------------------------------------}
              FuncReturn.B := GetGroupRecord(TmpStack[0].i,   { AreaNr }
                                             TmpStack[1].B,   { ? }
                                             TmpStack[2].B,   { ? }
                                             TmpGroup_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpGroup_1, FVal(Copy(CallTypes,  5, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 9, 1))));
            end; { GetGroupRecord }

        {$IFDEF WITH_FULL}
        {-- GetProtocolRecord -------------------------------------------------}
        6 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpProtocol_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              GetProtocolRecord(TmpStack[0].C, TmpProtocol_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpProtocol_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { GetProtocolRecord }
        {$ENDIF}

        {-- GetExitInfo -------------------------------------------------------}
        7 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpExitinfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              GetExitinfoInfo(TmpExitInfo_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpExitinfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { GetExitinfo }

        {-- ReadSysInfoBBS ----------------------------------------------------}
        8 : begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpSysInfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              ReadSysInfoBbs(TmpSysInfo_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpSysInfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { ReadSysinfoBBS }

        {-- SearchNextEvent ---------------------------------------------------}
        {$IFDEF WITH_FULL}
          9 : begin
                {-- First convert the data to a record --------------------------}
                Stack2Buf(elx_Globals, TmpEvent_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ------------------------------------------------}
                SearchNextEvent(TmpEvent_1);

                {-- First convert the data to a record --------------------------}
                Buf2Stack(elx_Globals, TmpEvent_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { SearchNextEvent }
        {$ENDIF}

        {-- ReadExitinfo ------------------------------------------------------}
        {$IFDEF WITH_FULL}
         10 : begin
                {-- First convert the data to a record --------------------------}
                Stack2Buf(elx_Globals, TmpExitinfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- Now check it ------------------------------------------------}
                ReadExitinfo(TmpExitInfo_1);

                {-- First convert the data to a record --------------------------}
                Buf2Stack(elx_Globals, TmpExitinfo_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { ReadExitinfo }
        {$ENDIF}

        {-- SelectAllCombined -------------------------------------------------}
        11: begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpCombined_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- Now check it ------------------------------------------------}
              SelectAllCombined(TmpCombined_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpCombined_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
            end; { SelectAllCombined }

        {-- GetAddress --------------------------------------------------------}
        12: begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpAddress_1, FVal(Copy(CallTypes,  3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

              {-- Now check it ------------------------------------------------}
              GetAddress(TmpStack[0].I, TmpAddress_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpAddress_1, FVal(Copy(CallTypes,  3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));
            end; { GetAddress }

        {-- SetAddress --------------------------------------------------------}
        13: begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpAddress_1, FVal(Copy(CallTypes,  3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

              {-- Now check it ------------------------------------------------}
              SetAddress(TmpStack[0].I, TmpAddress_1);

              {-- First convert the data to a record --------------------------}
              Buf2Stack(elx_Globals, TmpAddress_1, FVal(Copy(CallTypes,  3, 3)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));
            end; { GetAddress }

        {-- SetUserRecord -----------------------------------------------------}
        14: begin
              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpUser_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

              {-- First convert the data to a record --------------------------}
              Stack2Buf(elx_Globals, TmpUserExt_2, FVal(Copy(CallTypes,  7, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 11, 1))));

              {-- Now check it ------------------------------------------------}
              LineCfg^.Exitinfo^.Userinfo := Tmpuser_1;
              LineCfg^.UserExtension^ := TmpUserExt_2;
            end; { SetUserRecord }

        {-- AddToUserBase -----------------------------------------------------}
        15: begin
              AddToUserBase;
            end; { AddToUserBase }
      end; { case }
   end { FuncNr < 20 }

    {-- 20 .. 29 --------------------------------------------------------------}
    else if (FuncNr - 300) < 30 then
     begin
       Case (FuncNr - 300) of
         {-- GetTextAttr ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           20 : FuncReturn.I := TextAttr;
         {$ENDIF}

         {-- GetParameter -----------------------------------------------------}
         21 : SetReturnString(FuncReturn, ExtractWord(elx_Globals.elx_ParamStr,
                                                      TmpStack[0].I,
                                                      defExtractWord,
                                                      true,
                                                      false), false);

         {-- SetReturnValue ---------------------------------------------------}
         22 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[1] = 'c' then
                  ObjPtr^.elx_ReturnString := TmpStack[0].C
                    else objPtr^.elx_ReturnString := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);
              end; { SetReturnValue }
       end; { case }
    end { else }

    {-- 30 .. 70 --------------------------------------------------------------}
    else if (FuncNr - 300) < 70 then
     begin
       Case (FuncNr - 300) of
         {-- fb_OpenFileBase --------------------------------------------------}
         31 : begin
                {-- if the filebase is already open, close it first -----------}
                if ObjPtr^.elx_Filebase <> nil then
                  begin
                    Dispose(objPtr^.elx_Filebase, Done);
                    objPtr^.elx_Filebase := NIL;
                  end; { if }

                {-- Now create a new instance ---------------------------------}
                New(objPtr^.elx_Filebase, Init(TmpStack[0].I, true));

                {-- Get memory for the tagheader ------------------------------}
                MemMan.AllocMem(LineCfg^.Headerlist, SizeOf(TagHeaderList), 'TagHeaderList', 'ListFiles');
                FillChar(LineCfg^.HeaderList^, SizeOf(TagHeaderList), #00);

                {-- and return wether it succeeds -----------------------------}
                if objPtr^.elx_FileBase <> nil then
                  with objPtr^ do
                    begin
                      TmpBool := TRUE;

                      if elx_FileBase^.GetError <> 0 then
                        TmpBool := FALSE;

                      if LineCfg^.HeaderList = nil then
                        TmpBool := FALSE;
                    end
                      else TmpBool := FALSE;

                FuncReturn.B := TmpBool;
              end; { fb_OpenFileBase }

         {-- fb_Read ----------------------------------------------------------}
         32 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                  with objPtr^ do
                    begin
                      elx_FileBase^.Read(TmpStack[0].i);

                      FuncReturn.B := (elx_FileBase^.GetError = 0);
                    end { if }
                      else FuncReturn.B := FALSE;

              end; { fb_Read }

         {-- fb_Write ---------------------------------------------------------}
         33 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with ObjPtr^ do
                  begin
                    elx_FileBase^.WriteBoth(TmpStack[0].i, elx_FileBase^.CurHdr^);

                    FuncReturn.B := (elx_FileBase^.GetError = 0);
                  end { if }
                    else FuncReturn.B := FALSE;

              end; { fb_Write }

         {-- fb_TotalRecords --------------------------------------------------}
         34 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                  begin
                    FuncReturn.i := objPtr^.elx_FileBase^.TotalRecords;
                  end { if }
                    else FuncReturn.i := -1;
              end; { fb_TotalRecords }

         {-- fb_GetFileName ---------------------------------------------------}
         35 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with objPtr^ do
                  begin
                    SetReturnString(FuncReturn, elx_FileBase^.GetFileName, false);
                  end { if }
                    else FuncReturn.i := -1;
              end; { fb_GetFileName }

         {-- fb_GetShortName --------------------------------------------------}
         36 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with objPtr^ do
                  begin
                    SetReturnString(FuncReturn, elx_FileBase^.GetShortName, false);
                  end { if }
                    else FuncReturn.i := -1;
              end; { fb_GetShortName }

        {-- fb_GetHdrRecord ---------------------------------------------------}
        37 : begin
               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                 begin
                   TmpFileHdr_1 := objPtr^.elx_FileBase^.CurHdr^;
                 end
                   else FillChar(TmpFileHdr_1, SizeOf(TmpFileHdr_1), #0);

               {-- Convert the data to the stack -------------------------------}
               Buf2Stack(elx_Globals, TmpFileHdr_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
             end; { fb_GetHdrRecord }

        {-- fb_SetHdrRecord ---------------------------------------------------}
        38 : begin
               {-- Convert the data to a record --------------------------------}
               Stack2Buf(elx_Globals, TmpFileHdr_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                 begin
                   objPtr^.elx_FileBase^.CurHdr^ := TmpFileHdr_1;
                 end;
             end; { fb_SetHdrRecord }

        {-- fb_GetIdxRecord ---------------------------------------------------}
        39 : begin
               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with objPtr^ do
                 begin
                   TmpFileIdx_1 := elx_FileBase^.CurIdx^;
                 end
                   else FillChar(TmpFileIdx_1, SizeOf(TmpFileIdx_1), #0);

               {-- Convert the data to the stack -------------------------------}
               Buf2Stack(elx_Globals, TmpFileIdx_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
             end; { fb_GetIdxRecord }

        {-- fb_SetIdxRecord ---------------------------------------------------}
        40 : begin
               {-- Convert the data to a record --------------------------------}
               Stack2Buf(elx_Globals, TmpFileIdx_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with objPtr^ do
                 begin
                   elx_FileBase^.CurIdx^ := TmpFileIdx_1;
                 end; { if }
             end; { fb_SetIdxRecord }

        {-- fb_AddRecord ------------------------------------------------------}
        41 : begin
               {-- Convert the data to a record --------------------------------}
               Stack2Buf(elx_Globals, TmpFileHdr_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with objPtr^ do
                 begin
                   elx_FileBase^.AddHeader(TmpFileHdr_1);
                 end; { if }
             end; { fb_AddRecord }

        {-- fb_AddLfnPtr ------------------------------------------------------}
        42 : begin
               {-- First get the first string --------------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- now get the second string ---------------------------------}
               if CallTypes[2] = 'c' then
                 TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- and after all, get the third ------------------------------}
               if CallTypes[3] = 'c' then
                 TmpStr3 := Copy(TmpStack[2].C, 1, 1)
                   else TmpStr3 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with objPtr^ do
                 begin
                   ShortTmp := TmpStr3;
                   FuncReturn.I := elx_FileBase^.AddLfnPtr(TmpStr, TmpStr2, ShortTmp);
                   TmpStr3 := ShortTmp;
                 end
                   else FuncReturn.I := -1;
             end; { fb_AddLfnPtr }

        {-- fb_IsLfn ----------------------------------------------------------}
        43 : begin
               {-- First get the first string --------------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                 begin
                   FuncReturn.B := IsLfn(TmpStr);
                 end
                   else FuncReturn.B := FALSE;
             end; { fb_IsLfn }

        {-- fb_IsNewFile ------------------------------------------------------}
        44 : begin
               {-- First get the first string --------------------------------}
               if CallTypes[2] = 'c' then
                 TmpStr := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                 begin
                   FuncReturn.B := Is_NewFile(TmpStack[0].I, TmpStr);
                 end
                   else FuncReturn.B := FALSE;
             end; { fb_IsNewFile }

        {-- fb_MatchShowing ---------------------------------------------------}
        45 : begin
               {-- First get the first string --------------------------------}
               if CallTypes[2] = 'c' then
                 TmpStr := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with ObjPtr^ do
                 begin
                   if FileShowType(TmpStack[0].i) = fsKeyWord then
                     elx_FileBase^.ReadDescription(elx_FileBase^.TxtF, elx_FileBase^.GetLongDescPtr);

                   FuncReturn.B := ShowFile(FileShowType(TmpStack[0].I),
                                            elx_FileBase^.CurIdx^,
                                            elx_FileBase^.CurTxt^,
                                            TmpStr,
                                            false,
                                            elx_FileBase^.GetFileName);
                 end
                   else FuncReturn.B := FALSE;
             end; { fb_MatchShowing }

        {-- fb_IsComment ------------------------------------------------------}
        46 : FuncReturn.B := objPtr^.elx_FileBase^.IsComment;

        {-- fb_IsDeleted ------------------------------------------------------}
        47 : FuncReturn.B := objPtr^.elx_FileBase^.IsDeleted;

        {-- fb_IsUnListed -----------------------------------------------------}
        48 : FuncReturn.B := objPtr^.elx_FileBase^.IsUnListed;

        {-- fb_IsFree ---------------------------------------------------------}
        49 : FuncReturn.B := objPtr^.elx_FileBase^.IsFree;

        {-- fb_IsNotAvail -----------------------------------------------------}
        50 : FuncReturn.B := objPtr^.elx_FileBase^.IsNotAvail;

        {-- fb_IsLocked -------------------------------------------------------}
        51 : FuncReturn.B := objPtr^.elx_FileBase^.IsLocked;

        {-- fb_IsMissing ------------------------------------------------------}
        52 : FuncReturn.B := objPtr^.elx_FileBase^.IsMissing;

        {-- fb_IsNoTime -------------------------------------------------------}
        53 : FuncReturn.B := objPtr^.elx_FileBase^.IsNoTime;

        {-- fb_ReadDescription ------------------------------------------------}
        54 : begin
               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with ObjPtr^ do
                 begin
                   elx_FileBase^.ReadDescription(elx_FileBase^.TxtF, elx_FileBase^.GetLongDescPtr);
                 end; { elx_FileBase }
             end; { fb_ReadDescription }

        {-- fb_ResetDescriptionPtr --------------------------------------------}
        55 : begin
               {-- Try to gather the data --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                with ObjPtr^ do
                 begin
                   elx_FileBase^.ResetDescription;
                 end; { elx_FileBase }
             end; { fb_ResetDescriptionPtr }

         {-- fb_GetDescLine ---------------------------------------------------}
         56 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with ObjPtr^ do
                  begin
                    SetReturnString(FuncReturn, elx_FileBase^.GetDescLine(TmpStack[0].i), false);
                  end { if }
                    else FuncReturn.i := -1;
              end; { fb_GetDescLine }

         {-- fb_EndOfDesc -----------------------------------------------------}
         57 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with objPtr^ do
                  begin
                    FuncReturn.B := elx_FileBase^.EndOfDesc;
                  end { if }
                    else FuncReturn.B := TRUE;
              end; { fb_EndOfDesc }

         {-- fb_UnlistedFile --------------------------------------------------}
         59 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with objPtr^ do
                  begin
                    {-- Now check wether its unlisted -------------------------}
                    FuncReturn.B := NOT NoMissingFile(elx_FileBase^.CurHdr^);
                  end { if }
                    else FuncReturn.i := -1;
              end; { fb_IsMissingFile }

         {-- fb_AddToTagArray -------------------------------------------------}
         60 : begin
                {-- Try to read this record -----------------------------------}
                if objPtr^.elx_FileBase <> nil then
                 with objPtr^ do
                  begin
                    {-- Now add it to the header lists ------------------------}
                    AddToHeaderList(TmpStack[0].I,
                                    elx_FileBase^,
                                    TmpStack[1].I);
                  end; { if }
              end; { fb_AddToTagArray }

        {-- fb_GetError -------------------------------------------------------}
        61 : FuncReturn.I := objPtr^.elx_FileBase^.GetError;

        {-- fb_CloseFileBase --------------------------------------------------}
        62 : begin
               {-- Dispose of the object --------------------------------------}
               if objPtr^.elx_FileBase <> nil then
                 Dispose(objPtr^.elx_FileBase, Done);

               objPtr^.elx_FileBase := nil;

               {-- Release the tagheaderlist ----------------------------------}
               ReleaseMem(LineCfg^.HeaderList, SizeOf(TagHeaderList));
             end; { fb_CloseFileBase }

         {-- fb_TagFiles ------------------------------------------------------}
         {$IFDEF WITH_FULL}
           63 : TagFiles(LineCfg^.Headerlist^);
         {$ENDIF}

         {-- fb_ViewFiles -----------------------------------------------------}
         {$IFDEF WITH_FULL}
           64 : ViewTagFile(LineCfg^.Headerlist^, LineCfg^.Exitinfo^);
         {$ENDIF}

         {-- fb_EditTagList ---------------------------------------------------}
         {$IFDEF WITH_FULL}
           65 : EditTagList(TRUE,                     { From file list }
                            FALSE,                    { can tag any file }
                            FALSE,                    { global tagging }
                            FALSE,                    { group honouring }
                            LineCfg^.Exitinfo^.Userinfo.FileArea); { area nr }
         {$ENDIF}

         {-- fb_NewSinceLastLogon ---------------------------------------------}
         66 : FuncReturn.B := Is_NewFile(objPtr^.elx_FileBase^.GetUploadDate,
                                         LineCfg^.Exitinfo^.Userinfo.LastDate);

         {-- fb_DumpBinaryFile ------------------------------------------------}
         67 : begin
               {-- First get the first string --------------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- and send the file on its way ------------------------------}
               DumpBinaryFileToScreen(TmpStr);
              end; { fb_DumpBinaryFile }
       end; { case }
     end { else }

    {-- 70 .. 99 --------------------------------------------------------------}
    else if (FuncNr - 300) < 100 then
     begin
       Case (FuncNr - 300) of
         {-- web_GetUIN -------------------------------------------------------}
         {$IFDEF ISCGI}
           70 : FuncReturn.I := web_UIN;
         {$ENDIF}

         {-- web_GetUIR -------------------------------------------------------}
         {$IFDEF ISCGI}
           71 : FuncReturn.I := web_UIR;
         {$ENDIF}

         {-- web_GetUIP -------------------------------------------------------}
         {$IFDEF ISCGI}
           72 : FuncReturn.I := web_UIP;
         {$ENDIF}

         {-- web_ShowHTML -----------------------------------------------------}
         {$IFDEF ISCGI}
           73 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- Now display the file ------------------------------------}
                  FuncReturn.B := web_ShowHtmlFile(TmpStr);
                end; { if }
         {$ENDIF}

         {-- web_GetDateTime --------------------------------------------------}
         {$IFDEF ISCGI}
           74 : begin
                  {-- First get the actual value ------------------------------}
                  TmpStr := UnixToRfcDate(NowAsUnixDate, '');

                  {-- Now set the return value --------------------------------}
                  SetReturnString(FuncReturn, TmpStr, false);
                end; { if }
         {$ENDIF}

         {-- web_GetCookie ----------------------------------------------------}
         {$IFDEF ISCGI}
           75 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- get the cookie ------------------------------------------}
                  {$IFNDEF MSDOS}
                    {$IFNDEF DELPHI}
                      TmpStr := web_GetCookies(TmpStr,
                                               Dos.GetEnv('HTTP_COOKIE'));
                    {$ELSE}
                      TmpStr := web_GetCookies(TmpStr,
                                               GenDos.GetEnv('HTTP_COOKIE'));
                    {$ENDIF}
                  {$ENDIF}

                  {-- Now set the return value --------------------------------}
                  SetReturnString(FuncReturn, TmpStr, false);
                end; { if }
         {$ENDIF}

         {-- web_SetLoginData -------------------------------------------------}
         {$IFDEF ISCGI}
           76 : begin
                  {-- get the cookie ------------------------------------------}
                  web_UIN := TmpStack[0].i;
                  web_UIP := TmpStack[1].i;
                  web_UIR := TmpStack[2].i;
                  web_AlwSilent := TmpStack[3].B;
                end; { if }
         {$ENDIF}

         {-- web_GetFormData --------------------------------------------------}
         {$IFDEF ISCGI}
           77 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- get the post/get data -----------------------------------}
                  TmpStr := CgiObj^.GetFieldByName(TmpStr, true);

                  {-- Now set the return value --------------------------------}
                  SetReturnString(FuncReturn, TmpStr, false);
                end; { web_GetFormData }
         {$ENDIF}

         {-- web_FoundField ---------------------------------------------------}
         {$IFDEF ISCGI}
           78 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- Give back the result ------------------------------------}
                  FuncReturn.I := CgiObj^.FoundField(TmpStr);
                end; { if }
         {$ENDIF}


         {-- web_IsLoggedOn ---------------------------------------------------}
         {$IFDEF ISCGI}
           79 : begin
                  {-- Give back the result ------------------------------------}
                  FuncReturn.B := web_Access;
                end; { if }
         {$ENDIF}

         {-- web_RunErrorScript -----------------------------------------------}
         {$IFDEF ISCGI}
           80 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- Now run the error script --------------------------------}
                  web_FatalError(TmpStack[0].i, TmpStr);
                end; { if }
         {$ENDIF}

         {-- web_RunScript ----------------------------------------------------}
         {$IFDEF ISCGI}
           81 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  if CallTypes[2] = 'c' then
                    TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- Now run the error script --------------------------------}
                  FuncReturn.B := web_Scr.RunElexerScripts(TmpStr, TmpStr2, false, true, TmpLong,
                                                           TmpStr3);
                end; { if }
         {$ENDIF}

         {-- web_ConvertLink --------------------------------------------------}
         {$IFDEF ISCGI}
           82 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- convert the link to %xx codes (eg space to %20) ---------}
                  TmpStr := TransLink(TmpStr);

                  {-- Now set the return value --------------------------------}
                  SetReturnString(FuncReturn, TmpStr, false);
                end; { web_ConvertLink }
         {$ENDIF}

         {-- web_OpenOutput ---------------------------------------------------}
         {$IFDEF ISCGI}
           83 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- open the output and make sure the CGI object knows it ---}
                  {-- this way, we always display a content header and we -----}
                  {-- always leave one out when we dont need it ---------------}
                  CgiObj^.webOpenOutput(TmpStr);
                end; { web_OpenOutput }
         {$ENDIF}

         {-- web_EscapedToNonEscaped ------------------------------------------}
         {$IFDEF ISCGI}
           84 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- convert the link to &nbsp; to real spaces ---------------}
                  TmpStr := EscapedToNonEscaped(TmpStr);

                  {-- Now set the return value --------------------------------}
                  SetReturnString(FuncReturn, TmpStr, false);
                end; { web_EscapedToNonEscaped }
         {$ENDIF}

         {-- web_SendHeader ---------------------------------------------------}
         {$IFDEF ISCGI}
           85 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- First get the second string -----------------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                  {-- send the HTTP header ------------------------------------}
                  CgiObj^.webSendHeader(TmpStr, TmpStr2);
                end; { web_OpenOutput }
         {$ENDIF}

         {-- web_GetBigFormData -----------------------------------------------}
         {$IFDEF ISCGI}
           86 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- allocate memory for this array --------------------------}
                  New(MsgText_1);

                  {-- get all the data into an array --------------------------}
                  TmpLong := 0;                  { Counter into the char array }
                  { Need to know the value of MaxTxtLen: }
                  CgiObj^.BigFieldByName(TmpStr, MaxTxtLen);

                  Move(web_MsgTextRecord(CgiObj^.BigFieldByName(TmpStr, MaxTxtLen)^),
                      MsgText_1^, MaxTxtLen);

                  {-- convert all %xx codes to real values -------------------}
                  {-- this is an important call as it also ensures the string }
                  {-- is nul terminated --------------------------------------}
                  ConvertBuffer(MsgText_1^, MaxTxtLen);

                  {-- Now set the return value --------------------------------}
                  TmpStrA := SysUtils.StrPas(MsgText_1^);
                  SetReturnString(FuncReturn, TmpStrA, false);

                  {-- and dispose of the memory for this array ---------------}
                  Dispose(MsgText_1);
                end; { web_GetBigFormData }
         {$ENDIF}

         {-- web_SendCookie ---------------------------------------------------}
         {$IFDEF ISCGI}
           87 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- send the HTTP header ------------------------------------}
                  CgiObj^.webSendCookie(TmpStr);
                end; { web_SendCookie }
         {$ENDIF}

         {-- web_MatchCredentials---------------------------------------------}
         {$IFDEF ISCGI}
           88 : begin
                  {-- and try to login ---------------------------------------}
                  if web_MatchCredentials(TmpStack[0].I,  { Userrecord number }
                                          TmpStack[1].I,     { Crc32 password }
                                          TmpStack[2].I,         { Nodenumber }
                                          web_AlwSilent,{ Allow silent login? }
                                          TmpStack[3].I) then  { Error number }
                   begin
                     {-- Access granted --------------------------------------}
                     web_Access := TRUE;

                     {-- get a lot of the user info --------------------------}
                     web_GetUserData;
                   end
                    else begin
                           {-- deny the access -------------------------------}
                           web_Access := FALSE;
                         end; { else }

                   {-- and return the result to our caller -------------------}
                   FuncReturn.B := web_Access;
                end; { web_MatchCredentials }
         {$ENDIF}

         {-- web_CookieExist --------------------------------------------------}
         {$IFDEF ISCGI}
           89 : begin
                  {-- First get the first string ------------------------------}
                  if CallTypes[1] = 'c' then
                    TmpStr := Copy(TmpStack[0].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                  {-- get the cookie ------------------------------------------}
                  {$IFNDEF MSDOS}
                    {$IFNDEF DELPHI}
                      FuncReturn.B := web_CookieExist(TmpStr,
                                               Dos.GetEnv('HTTP_COOKIE'));
                    {$ELSE}
                      FuncReturn.B := web_CookieExist(TmpStr,
                                               GenDos.GetEnv('HTTP_COOKIE'));
                    {$ENDIF}
                  {$ENDIF}
                end; { if }
         {$ENDIF}
         {-- other ------------------------------------------------------------}
         99 : ;
       end; { case }
     end; { else }
end; { proc. ToFourHundred }

procedure ToFiveHundred;
begin
  if (FuncNr - 400) < 40 then
    begin
      Case (FuncNr - 400) of
        {-- GetPath_* ---------------------------------------------------------}
        1 .. 25
          : begin
              Case (FuncNr - 400) of
                01 : TmpStr := ProtocolFileName;
                02 : TmpStr := LanguageFileName;
                03 : TmpStr := LimitsFileName;
                04 : TmpStr := EventsFileName;
                05 : TmpStr := FGroupsFileName;
                06 : TmpStr := MGroupsFileName;
                07 : TmpStr := FilesFileName;
                08 : TmpStr := AddressFileName;
                09 : TmpStr := ModemFileName;
                10 : TmpStr := TelnetFileName;
                11 : TmpStr := PageStatFileName;
                12 : TmpStr := MessagesFileName;
                13 : TmpStr := TagListFileName;
                14 : TmpStr := LastcallFileName;
                15 : TmpStr := SysInfoFileName;
                16 : TmpStr := TimeLogFileName;
                17 : TmpStr := LogFileName;
                18 : TmpStr := MessageEleFileName;
                19 : TmpStr := EleFilesFileName;
                20 : TmpStr := NewsServerFileName;
                {$IFDEF ISCGI}
                  21 : TmpStr := web_HtmPath;
                  22 : TmpStr := web_ScrPath;
                {$ENDIF}
                23 : TmpStr := GlobalCfg^.RaConfig^.SysPath;
                24 : TmpStr := GlobalCfg^.RaConfig^.MsgBasePath;
                25 : TmpStr := GlobalCfg^.RaConfig^.FileBasePath;
              end; { case }

              {-- Now set the return value --------------------------------}
              SetReturnString(FuncReturn, TmpStr, false);
            end; { GetPaths }

      end; { case }
   end { FuncNr < 30 }

    {-- 40 .. 99 --------------------------------------------------------------}
    else if (FuncNr - 400) < 100 then
     begin
       Case (FuncNr - 400) of
         {-- mb_GetAreaAddress ------------------------------------------------}
         40 : begin
                {-- First actually get the address ----------------------------}
                TmpStr := GetAreaAddress(TmpStack[0].i);

                {-- now return the string -------------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetAreaAddress }

         {-- mb_InMarkedList --------------------------------------------------}
         41 : begin
                {-- First actually get the address ----------------------------}
                FuncReturn.B := InMarkList(TmpStack[0].i,
                                           TmpStack[0].i);
              end; { mb_InMarkedList }

         {-- mb_AddToMarkList -------------------------------------------------}
         42 : begin
                {-- actually add it to the marking list -----------------------}
                AddToMarkList(TmpStack[0].i,            { areanumber }
                              TmpStack[1].i);           { messagenumber }
              end; { mb_AddToMarkLsit }

         {-- mb_SetLastRead ---------------------------------------------------}
         43 : begin
                {-- set the users lastread pointer to this --------------------}
                {!!!!!!!!!!!!!!!!!!!!!!!}
                { tLastRead(TmpStack[0].i,   }        { areanumber }
                {           TmpStack[1].i);  }        { messagenumber }

              end; { mb_SetLastRead }

         {-- mb_OpenMsgBase ---------------------------------------------------}
         44 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- close the messagebase if necessary ------------------------}
                if objPtr^.elx_MessageBase <> nil then
                 with ObjPtr^ do
                  begin
                    CloseMsgArea(elx_MessageBase^.MsgPtr);
                    if elx_MessageBase^.EditorLines <> nil then
                      Dispose(elx_MessageBase^.EditorLines, Done);
                    Dispose(elx_MessageBase);
                    elx_MessageBase := nil;
                  end; { if }

                {-- get the corresponding Elemessagerecord --------------------}
                GetEleMessageRecord(TmpEleMsgArea_2, TmpMsgArea_1.AreaNum, false);

                {-- actually open the messagebase -----------------------------}
                TmpBool := FALSE;
                New(objPtr^.elx_MessageBase);
                objPtr^.elx_MessageBase^.EditorLines := nil;

                with objPtr^.elx_MessageBase^ do
                  TmpBool := OpenOrCreateMsgArea(MsgPtr,
                                              MakeMsgId(TmpMsgArea_1.AreaNum,
                                                        TmpMsgArea_1.JamBase,
                                                        TmpMsgArea_1.Attribute,
                                                        TmpEleMsgArea_2.Attribute));

                {-- release memory if open failed -----------------------------}
                if NOT TmpBool then
                 with objPtr^ do
                  begin
                    CloseMsgArea(elx_MessageBase^.MsgPtr);
                    if elx_MessageBase^.EditorLines <> nil then
                      Dispose(elx_MessageBase^.EditorLines, Done);
                    Dispose(elx_MessageBase);
                    elx_MessageBase := nil;
                  end; { if }

                {-- and set the function result -------------------------------}
                FuncReturn.B := TmpBool;
              end; { mb_OpenMsgbase }

         {-- mb_Read ----------------------------------------------------------}
         45 : begin
                {-- seek to the correct message -------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SeekFirst(TmpStack[0].i);

                {-- and read in the headers and the like ----------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.MsgStartup;
              end; { mb_Read }

         {-- mb_Write ---------------------------------------------------------}
         46 : begin
                {-- seek to the correct message -------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SeekFirst(TmpStack[0].i);

                {-- and read in the headers and the like ----------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.RewriteHdr;
              end; { mb_Write }

         {-- mb_MessageFound --------------------------------------------------}
         47 : begin
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.SeekFound;
              end; { mb_MessageFound }

         {-- mb_GetPrevious ---------------------------------------------------}
         48 : begin
                objPtr^.elx_MessageBase^.MsgPtr^.SeekPrior;

                {-- and read in the headers and the like ----------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.MsgStartup;
              end; { mb_GetPrevious }

         {-- mb_GetNext -------------------------------------------------------}
         49 : begin
                objPtr^.elx_MessageBase^.MsgPtr^.SeekNext;

                {-- and read in the headers and the like ----------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.MsgStartup;
              end; { mb_GetNext }

         {-- mb_GetRecord -----------------------------------------------------}
         50 : begin
                {---- !!!! -----}
              end; { mb_GetRecord }

         {-- mb_SetRecord -----------------------------------------------------}
         51 : begin
                {---- !!!! -----}
              end; { mb_SetRecord }

         {-- mb_AddMessage ----------------------------------------------------}
         52 : begin
                NewWriteMsg(objPtr^.elx_MessageBase^.MsgPtr);
              end; { mb_AddMessage }

         {-- mb_GetActiveMsgNum -----------------------------------------------}
         53 : begin
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.NumberOfMsgs;
              end; { mb_GetActiveMsgNum }

         {-- mb_GetHighMsgNum -------------------------------------------------}
         54 : begin
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.GetHighMsgNum;
              end; { mb_GetHighMsgNum }

         {-- mb_GetNetMailBoard -----------------------------------------------}
         55 : begin
                FuncReturn.I := SearchNetMailBoard;
              end; { mb_GetNetMailBoard }

         {-- mb_GetMsgAreaStats -----------------------------------------------}
         56 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- get the corresponding Elemessagerecord --------------------}
                GetEleMessageRecord(TmpEleMsgArea_2, TmpMsgArea_1.AreaNum, TRUE);

                {-- now actually get the data ---------------------------------}
                GetMsgAreaStats(TmpMsgArea_1,
                                TmpEleMsgArea_2,
                                TmpStack[0].i,        { active }
                                TmpStack[1].i,        { first }
                                TmpStack[2].i);       { high }
              end; { mb_GetMsgAreaStats }

         {-- mb_SilentDeleteMsg -----------------------------------------------}
         57 : begin
                {-- now actually get the data ---------------------------------}
                SilentDeleteMsg(TmpStack[0].i,          { Board }
                                TmpStack[1].i,          { Message number }
                                TmpStack[2].B);         { Check access }
              end; { mb_SilentDeleteMsg }

         {-- mb_MenuPost ------------------------------------------------------}
         58 : begin
                {-- First get the first string ------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                MenuPost(TmpStr);
              end; { mb_MenuPost }

         {-- mb_PostFile ------------------------------------------------------}
         59 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[3].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr3 := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

                {-- Get string four -------------------------------------------}
                if CallTypes[5] = 'c' then
                  TmpStr4 := Copy(TmpStack[5].C, 1, 1)
                    else TmpStr4 := StrGetString(elx_Globals, TmpStack[4].S, 1, -1);

                {-- Get string five -------------------------------------------}
                if CallTypes[6] = 'c' then
                  TmpStr5 := Copy(TmpStack[6].C, 1, 1)
                    else TmpStr5 := StrGetString(elx_Globals, TmpStack[5].S, 1, -1);

                {-- now actually call the routine -----------------------------}
                FilePost(TmpStack[0].i,        { AreaNr }
                         TmpStr,               { FromWho }
                         TmpStr2,              { ToWho }
                         TmpStr3,              { Subject }
                         TmpStr4,              { Filename }
                         TmpStr5);             { Add text (header) }
              end; { mb_PostFile }

         {-- mb_DoReadMessageText ---------------------------------------------}
         60 : begin
                {-- now actually get the data ---------------------------------}
                with objPtr^.elx_MessageBase^ do
                  MsgPtr^.MsgTxtStartup;
              end; { mb_DoReadMessageText }

         {-- mb_ResetMessagePtr -----------------------------------------------}
         61 : begin
                {-- now actually get the data ---------------------------------}
                with objPtr^.elx_MessageBase^ do
                  MsgPtr^.ClearMsgText;
              end; { mb_ResetMessagePtr }

         {-- mb_EndOfMessage --------------------------------------------------}
         62 : begin
                {-- now actually get the data ---------------------------------}
                with objPtr^.elx_MessageBase^ do
                  FuncReturn.B := Msgptr^.EOM;
              end; { mb_EndOfMessage }

         {-- mb_GetMessageLine ------------------------------------------------}
         63 : begin
                {-- now actually get the data ---------------------------------}
                if TmpStack[0].i <> -1 then
                  begin
                    ShortTmp := objPtr^.elx_MessageBase^.MsgPtr^.GetString(TmpStack[0].i);

                    {-- Set the content of the return var ---------------------}
                    SetReturnString(FuncReturn, ShortTmp, false);
                  end
                    else begin
                           TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetMsgBuffer;

                           {-- Set the content of the return var --------------}
                           SetReturnString(FuncReturn, TmpStr, false);
                         end; { else }
              end; { mb_GetMessageline }

         {$IFDEF ISCGI}
         {-- mb_HtmlString ----------------------------------------------------}
         64 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                TmpStr := TransSpaces(TransString(TmpStr, false, true));
                TmpStr := ToWww(TmpStr);

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_HtmlString }
         {$ENDIF}

         {-- mb_AddMessageLine ------------------------------------------------}
         65 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually add the data ---------------------------------}
                MailDoLongString(objPtr^.elx_MessageBase^.MsgPtr, TmpStr);
              end; { mb_AddMessageline }

         {-- mb_SetToWho ------------------------------------------------------}
         66 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetTo(TmpStr);
              end; { mb_SetToWho }

         {-- mb_SetFromWho ----------------------------------------------------}
         67 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetFrom(TmpStr);
              end; { mb_SetFromWho }

         {-- mb_SetSubject ----------------------------------------------------}
         68 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetSubj(TmpStr);
              end; { mb_SetSubject }

         {-- mb_SetReturnReceipt ----------------------------------------------}
         69 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetRetRct(TmpStack[0].b);
              end; { mb_SetReturnReceipt }

         {-- mb_SetPrivate ----------------------------------------------------}
         70 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetPriv(TmpStack[0].b);
              end; { mb_SetPrivate }

         {-- mb_SetReplyReceipt -----------------------------------------------}
         71 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetRetRct(TmpStack[0].b);
              end; { mb_SetReplyReceipt }

         {-- mb_SetKillSent ---------------------------------------------------}
         72 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetKillSent(TmpStack[0].b);
              end; { mb_SetKillSent }

         {-- mb_SetCrashMail --------------------------------------------------}
         73 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetCrash(TmpStack[0].b);
              end; { mb_SetCrashMail }

         {-- mb_SetAttachment -------------------------------------------------}
         74 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetFAttach(TmpStack[0].b);
              end; { mb_SetAttachment }

         {-- mb_SetMarkAsSent -------------------------------------------------}
         75 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetSent(TmpStack[0].b);
              end; { mb_SetMarkAsSent }

         {-- mb_GetMsgNumber --------------------------------------------------}
         76 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.GetMsgNum;
              end; { mb_GetMsgNumber }

         {-- mb_SetDestAddress ------------------------------------------------}
         77 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- convert the string to a valid address ---------------------}
                ShortTmp := TmpStr;
                with TmpAddress_1 do
                  StringToAddr(ShortTmp,
                               Zone,
                               Net,
                               Node,
                               Point);
                TmpStr := ShortTmp;

                {-- now actually set the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetDest(AddrType(TmpAddress_1));
              end; { mb_SetDestAddress }

         {-- mb_SetOrigAddress ------------------------------------------------}
         78 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- convert the string to a valid address ---------------------}
                ShortTmp := TmpStr;
                with TmpAddress_1 do
                  StringToAddr(ShortTmp,
                               Zone,
                               Net,
                               Node,
                               Point);
                TmpStr := ShortTmp;

                {-- now actually set the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetOrig(AddrType(TmpAddress_1));
              end; { mb_SetOrigAddress }

         {-- mb_HandleMessageRead ---------------------------------------------}
         79 : begin
                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  3, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- now actually get the data ---------------------------------}
                with ReadMsgObj^,objPtr^ do
                  HandleMessageRead(elx_MessageBase^.MsgPtr,
                                    TmpStack[0].B,          { From mailbox scan? }
                                    TmpBool,                    { Confirm accept }
                                    TmpMsgArea_1,                  { Area record }
                                    Ra250MsgArea(TmpMsgArea_1.AreaNum),  { Board }
                                    TmpLong);
              end; { mb_HandleMessageRead }

         {-- mb_SetReceived ---------------------------------------------------}
         80 : begin
                {-- now set the data ------------------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetRcvd(TmpStack[0].B);
              end; { mb_SetReceived }

         {-- mb_SetDateStr ----------------------------------------------------}
         81 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now set the data ------------------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetDate(TmpStr);
              end; { mb_SetDateStr }

         {-- mb_SetTimeStr ----------------------------------------------------}
         82 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now set the data ------------------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetTime(TmpStr);
              end; { mb_SetTimeStr }

         {-- mb_SetReplyNr ----------------------------------------------------}
         83 : begin
                SetReplyNr(TmpStack[0].i,       { board }
                           TmpStack[1].I,       { original }
                           TmpStack[2].I);      { reply }
              end; { ?? }

         {-- mb_GetToWho ------------------------------------------------------}
         84 : begin
                {-- now actually get the data ---------------------------------}
                TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetTo;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetToWho }

         {-- mb_GetFromWHo-----------------------------------------------------}
         85 : begin
                {-- now actually get the data ---------------------------------}
                if objPtr^.elx_MessageBase <> nil then
                  TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetFrom
                   else TmpStr := 'mb_GetFromWho';

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetFromWho }

         {-- mb_GetSubject ----------------------------------------------------}
         86 : begin
                {-- now actually get the data ---------------------------------}
                TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetSubj;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetSubject }

         {-- mb_GetReturnReceipt ----------------------------------------------}
         87 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsReqRct;
              end; { mb_GetReturnReceipt }

         {-- mb_GetPrivate ----------------------------------------------------}
         88 : begin
                {-- now actually get the data ---------------------------------}
                if objPtr^.elx_MessageBase <> nil then
                  FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsPriv;
              end; { mb_GetPrivate }

         {-- mb_GetReplyReceipt -----------------------------------------------}
         89 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsRetRct;
              end; { mb_GetReplyReceipt }

         {-- mb_GetKillSent ---------------------------------------------------}
         90 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsKillSent;
              end; { mb_GetKillSent }

         {-- mb_GetCrashMail --------------------------------------------------}
         91 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsCrash;
              end; { mb_GetCrashMail }

         {-- mb_GetAttachment -------------------------------------------------}
         92 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsFAttach;
              end; { mb_GetAttachment }

         {-- mb_GetMarkAsSent -------------------------------------------------}
         93 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsSent;
              end; { mb_GetMarkedAsSent }

         {-- mb_GetMsgLines ---------------------------------------------------}
         94 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MaxEditLines;
              end; { mb_GetMsgLines }

         {-- mb_GetDestAddress ------------------------------------------------}
         95 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.GetDest(AddrType(TmpAddress_1));

                {-- convert it to a string ------------------------------------}
                ShortTmp := TmpStr;
                with TmpAddress_1 do
                  StringToAddr(ShortTmp,
                               Zone,
                               Net,
                               Node,
                               Point);
                TmpStr := ShortTmp;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetDestAddress }

         {-- mb_GetOrigAddress ------------------------------------------------}
         96 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.GetOrig(AddrType(TmpAddress_1));

                {-- convert it to a string ------------------------------------}
                ShortTmp := TmpStr;
                with TmpAddress_1 do
                  StringToAddr(ShortTmp,
                               Zone,
                               Net,
                               Node,
                               Point);
                TmpStr := ShortTmp;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetOrigAddress }


         {-- mb_GetMarkAsReceived ---------------------------------------------}
         97 : begin
                {-- now actually get the data ---------------------------------}
                FuncReturn.B := objPtr^.elx_MessageBase^.MsgPtr^.IsRcvd;
              end; { mb_GetMarkAsReceived }

         {-- mb_GetDateStr ----------------------------------------------------}
         98 : begin
                {-- now actually get the data ---------------------------------}
                TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetDate;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetDateStr }

         {-- mb_GetTimeStr ----------------------------------------------------}
         99 : begin
                {-- now actually get the data ---------------------------------}
                TmpStr := objPtr^.elx_MessageBase^.MsgPtr^.GetTime;

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { mb_GetTimeStr }
       end; { case }
     end; { else }
end; { proc. ToFiveHundred }

procedure ToSixHundred;
begin
  if (FuncNr - 500) < 20 then
    begin
      Case (FuncNr - 500) of
        {-- mb_HasNewMail -----------------------------------------------------}
        00 : begin
               {-- First convert the data to a record -------------------------}
               Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Now return the data ----------------------------------------}
               FuncReturn.B := HasNewMail(TmpMsgArea_1);
             end; { mb_HasNewMail }

        {-- mb_GetLastRead ----------------------------------------------------}
        01 : begin
               {-- First convert the data to a record -------------------------}
               Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Now return the data ----------------------------------------}
               FuncReturn.I := GetLastReadNum(TmpMsgArea_1);
             end; { mb_GetLastRead }

        {-- mb_CloseMsgBase ---------------------------------------------------}
        02 : begin
               if objPtr^.elx_MessageBase <> nil then
                with objPtr^ do
                 begin
                   CloseMsgArea(elx_MessageBase^.MsgPtr);
                   if elx_MessageBase^.EditorLines <> nil then
                     Dispose(elx_MessageBase^.EditorLines, Done);
                   Dispose(elx_MessageBase);
                   elx_MessageBase := nil;
                 end; { if }
             end; { mb_CloseMsgBase }

        {-- CheckMsgDeleteAccess ----------------------------------------------}
        03 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- now set the function result -------------------------------}
                FuncReturn.B := CheckMsgDeleteAccess(TmpMsgArea_1,
                                                     TmpStr,
                                                     TmpStr2,
                                                     TmpStack[3].B,
                                                     LineCfg^.Exitinfo^.Userinfo);
             end; { CheckMsgDeleteAccess }

        {-- mb_PostMessage ----------------------------------------------------}
        04 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr3 := Copy(TmpStack[3].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

                {-- Get string four -------------------------------------------}
                if CallTypes[5] = 'c' then
                  TmpStr4 := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr4 := StrGetString(elx_Globals, TmpStack[4].S, 1, -1);

                {-- Get string five -------------------------------------------}
                if CallTypes[6] = 'c' then
                  TmpStr5 := Copy(TmpStack[5].C, 1, 1)
                    else TmpStr5 := StrGetString(elx_Globals, TmpStack[5].S, 1, -1);

                {-- Get string five -------------------------------------------}
                if CallTypes[7] = 'c' then
                  TmpStr6 := Copy(TmpStack[6].C, 1, 1)
                    else TmpStr6 := StrGetString(elx_Globals, TmpStack[6].S, 1, -1);

                {-- now PostMessage -------------------------------------------}
                with objPtr^ do
                PostMessage(TmpStack[0].I,                          { Boardnr }
                            TmpStr,                                   { ToWho }
                            TmpStr2,                                { FromWho }
                            TmpStr3,                                { Subject }
                            TmpStack[7].B,                   { Return receipt }
                            TmpStack[8].B,                         { Private? }
                            FALSE,             { This is not an reply receipt }
                            TmpStack[9].B,                        { Kill/sent }
                            TmpStack[10].B,                       { Crashmail }
                            TmpStack[11].B,                { Attachment flag? }
                            TmpStack[12].B,               { Mark this as sent }
                            elx_MessageBase^.MaxEditLines,  { Nr of msg lines }
                            elx_MessageBase^.EditorLines^,{ Editor string lst }
                            TmpStack[13].I,          { Message number written }
                            TmpStr4,                    { Destination address }
                            TmpStr5,                    { Originating address }
                            FALSE,                         { User interaction }
                            FALSE,                 { Mark message as received }
                            DateStr,                               { Date msg }
                            TimeStr(true, false),                  { Time msg }
                            TmpStr6,                           { Reply-kludge }
                            '',
                            nilAbsMsgPtr);                    { Cached MsgPtr }
             end; { mb_PostMessage }

        {-- mb_strbuf_init ----------------------------------------------------}
        05 : begin
               {-- if we already have one, dispose of it ----------------------}
               if objPtr^.elx_MessageBase^.EditorLines <> nil then
                 begin
                   Dispose(objPtr^.elx_MessageBase^.EditorLines, done);
                 end; { if }

               {-- and get the new one ----------------------------------------}
               New(objPtr^.elx_MessageBase^.EditorLines, Init(MaxMsgLines));
               objPtr^.elx_MessageBase^.MaxEditLines := 0;
             end; { mb_strbuf_init }

        {-- mb_strbuf_get -----------------------------------------------------}
        06 : begin
                {-- now actually get the data ---------------------------------}
                TmpStr := objPtr^.elx_MessageBase^.EditorLines^.Get(TmpStack[0].i);

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
             end; { mb_strbuf_get }

        {-- mb_strbuf_put -----------------------------------------------------}
        07 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.EditorLines^.Put(TmpStack[0].i, TmpStr);

                {-- correct the string count ----------------------------------}
                with objPtr^ do
                  if TmpStack[0].i > elx_MessageBase^.MaxEditLines then
                    elx_MessageBase^.MaxEditLines := TmpStack[0].i;
             end; { mb_strbuf_put }

        {-- mb_strbuf_delete --------------------------------------------------}
        08 : begin
                {-- now actually get the data ---------------------------------}
                objPtr^.elx_MessageBase^.EditorLines^.Delete(TmpStack[0].i);

                {-- correct the strng count -----------------------------------}
                Dec(objPtr^.elx_MessageBase^.MaxEditLines);
             end; { mb_strbuf_delete }

        {-- mb_strbuf_add -----------------------------------------------------}
        09 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now actually get the data ---------------------------------}
                with objPtr^.elx_messageBase^ do
                  EditorLines^.Put(maxEditLines + 1, TmpStr);

                {-- correct the strng count -----------------------------------}
                Inc(objPtr^.elx_MessageBase^.MaxEditLines);
             end; { mb_strbuf_add }

        {-- mb_strbuf_done ----------------------------------------------------}
        10 : begin
                {-- First get the first string --------------------------------}
                Dispose(objPtr^.elx_MessageBase^.EditorLines, Done);
                objPtr^.elx_MessageBase^.EditorLines := nil;
                objPtr^.elx_MessageBase^.MaxeditLines := 0;
             end; { mb_strbuf_done }

        {-- mb_strbuf_count ---------------------------------------------------}
        11 : begin
                {-- First get the first string --------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MaxEditLines;
             end; { mb_strbuf_count }

        {-- mb_strbuf_clear ---------------------------------------------------}
        12 : begin
                {-- First get the first string --------------------------------}
                objPtr^.elx_MessageBase^.MaxEditLines := 0;
                objPtr^.elx_MessageBase^.EditorLines^.Clear;
             end; { mb_strbuf_clear }

        {$IFDEF ISCGI}
        {-- mb_form_to_strbuf -------------------------------------------------}
        13 : begin
               {-- First get the first string ---------------------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

               {-- Initialize some variables ----------------------------------}
               objPtr^.elx_MessageBase^.MaxEditLines := 0;
               objPtr^.elx_MessageBase^.EditorLines^.Clear;

               {-- allocate memory for this array -----------------------------}
               New(MsgText_1);

               {-- get all the data into an array -----------------------------}
               TmpLong := 0;                  { Counter into the char array }
               CgiObj^.BigFieldByName(TmpStr, MaxTxtLen);

               Move(web_MsgTextRecord(CgiObj^.BigFieldByName(TmpStr, MaxTxtLen)^),
                    MsgText_1^, MaxTxtLen);

               {-- convert all %xx codes to real values -----------------------}
               {-- this is an important call as it also ensures the string ----}
               {-- is nul terminated ------------------------------------------}
               ConvertBuffer(MsgText_1^, MaxTxtLen);

               {-- recalculate the length of this string ----------------------}
               {$IFDEF DELPHI}
                 MaxTxtLen := Longint(SysUtils.StrLen(PChar(MsgText_1)));
               {$ELSE}
                 MaxTxtLen := StrLen(MsgText_1^);
               {$ENDIF}

               {-- now put the char-array into an usable format ---------------}
               if MaxTxtLen > 0 then
                with objPtr^ do
                 REPEAT
                   Inc(elx_MessageBase^.maxEditLines);

                   GetNextLine(MsgText_1^, TmpLong, TmpStr);

                   with elx_MessageBase^ do
                     begin
                       if NOT TmpStack[1].B then  { allow html }
                         InvalidateHtml(TmpStr);

                      EditorLines^.Put(MaxEditLines, TmpStr);
                     end; { with }

                 UNTIL (TmpLong >= MaxTxtLen) OR
                         (elx_MessageBase^.maxEditLines >= MaxMsgLines);

               {-- dispose of the memory --------------------------------------}
               Dispose(MsgText_1);
             end; { mb_form_to_strbuf }
        {$ENDIF}

        {$IFNDEF MSDOS}
        {$IFNDEF ELEUNIX}
        {$IFNDEF GO32V2}
        {-- GetNewsArticleHeader ----------------------------------------------}
        14 : begin
               {-- Now return the data ----------------------------------------}
               TmpArticle_1 := Articles.FilterArticle;

               {-- First convert the record to EleXer data --------------------}
               Buf2Stack(elx_Globals, TmpArticle_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
             end; { GetNewsArticleHeader }
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}

        {-- mb_GetReplyFirst --------------------------------------------------}
        15 : begin
                {-- First get the first string --------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.GetSeeAlso;
             end; { mb_GetReplyFirst }

        {-- mb_GetReplyNext ---------------------------------------------------}
        16 : begin
                {-- First get the first string --------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.GetNextSeeAlso;
             end; { mb_GetReplyNext }

        {-- mb_GetReplyTo -----------------------------------------------------}
        17 : begin
                {-- First get the first string --------------------------------}
                FuncReturn.I := objPtr^.elx_MessageBase^.MsgPtr^.GetRefer;
             end; { mb_GetReplyTo }

        {-- FormatDateStr -----------------------------------------------------}
        18 : begin
                {-- First get the first string --------------------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[0].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);

                {-- now format the string -------------------------------------}
                with LangObj^ do
                  TmpStr := RaFormatDate(TmpStr,                  { actual date }
                                         TmpStack[1].i,                { format }
                                         y2k_YearType(TmpStack[2].i),  { y2ktype }
                                         lineCfg^.Exitinfo^.Userinfo.DateFormat);

                {-- Set the content of the return var -------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
             end; { FormatDateStr }

        {-- mb_UpdateMsgText --------------------------------------------------}
        19 : begin
                {-- add the buffer to the text --------------------------------}
                TmpLong := 1;

                while TmpLong <= objPtr^.elx_MessageBase^.maxEditLines do
                  begin
                    TmpStr := objPtr^.elx_MessageBase^.EditorLines^.Get(TmpLong);
                    MailDoLongString(objPtr^.elx_MessageBase^.MsgPtr, TmpStr);
                    Inc(TmpLong);
                  end; { while }

                {-- First get the first string --------------------------------}
                TmpWord := FuncReturn.I;
                objPtr^.elx_MessageBase^.MsgPtr^.UpdateMsgText(TmpWord);
                FuncReturn.I := TmpWord;
             end; { mb_UpdateMsgText }

      end; { case }
    end { FuncNr < 20 }

    {-- 20 .. 30 --------------------------------------------------------------}
    else if (FuncNr - 500) < 31 then
     begin
       Case (FuncNr - 500) of
         {-- usr_GetExtensions ------------------------------------------------}
         20 : begin
                {-- Convert the data back to a record -------------------------}
                Buf2Stack(elx_Globals, lineCfg^.UserExtension^, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { usr_GetExtensionsBuf }

         {-- usr_SetExtensions ------------------------------------------------}
         21 : begin
                {-- Convert the record back to data ---------------------------}
                Stack2Buf(elx_Globals, lineCfg^.UserExtension^, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { usr_SetExtensionsBuf }

        {-- mb_AllowEdit ------------------------------------------------------}
        22 : begin
               {-- First convert the data to a record -------------------------}
               Stack2Buf(elx_Globals, TmpMsgArea_1, FVal(Copy(CallTypes,  2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

               {-- Get string two --------------------------------------------}
               if CallTypes[5] = 'c' then
                 TmpStr2 := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr2 := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- get the corresponding Elemessagerecord --------------------}
                GetEleMessageRecord(TmpEleMsgArea_2, TmpMsgArea_1.AreaNum, true);

                {-- Now return the data ---------------------------------------}
                FuncReturn.B := AllowEdit(LineCfg^.Exitinfo^,
                                          TmpMsgArea_1,
                                          TmpeleMsgArea_2,
                                          TmpStr2);
             end; { mb_AllowEdit }

        {-- mb_SetReplyFirst --------------------------------------------------}
        23 : begin
                {-- Update the reply pointer ----------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetSeeAlso(TmpStack[0].I);
             end; { mb_GetReplyFirst }

        {-- mb_SetReplyNext ---------------------------------------------------}
        24 : begin
                {-- Update the reply pointer ----------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetNextSeeAlso(TmpStack[0].I);
             end; { mb_GetReplyNext }

        {-- mb_SetReplyTo -----------------------------------------------------}
        25 : begin
                {-- Update the reply pointer ----------------------------------}
                objPtr^.elx_MessageBase^.MsgPtr^.SetRefer(TmpStack[0].I);
             end; { mb_SetReplyTo }

        {-- usr_ExtSetDefaults -----------------------------------------------}
        26 : begin
               usrExt_SetDefaults(TmpUserExt_1);

               {-- Convert the data back to a record -------------------------}
               Buf2Stack(elx_Globals, TmpUserExt_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
             end; { usr_ExtSetDefaults }

       end; { case }
    end { FuncNr < 31 }

    {-- 31 .. 99 --------------------------------------------------------------}
    else if (FuncNr - 500) > 30 then
     begin
       Case (FuncNr - 500) of
         {$IFDEF TCPIP}
         {-- sock_init --------------------------------------------------------}
         31 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- create the socket -----------------------------------------}
                ObjPtr^.elx_Sockets[TmpLong] := tTcpServer.Create;
              end; { sock_init }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_done --------------------------------------------------------}
         32 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- dispose of the socket -------------------------------------}
                objPtr^.elx_Sockets[TmpLong].Free;
              end; { sock_done }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_connectto ---------------------------------------------------}
         33 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- first we retrieve the server to connect to ----------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- now get the port to connect at ----------------------------}
                TmpLong2 := TmpStack[2].I;

                {-- Connect to the specified server ---------------------------}
                ShortTmp := TmpStr2;
                with objptr^ do
                  FuncReturn.B :=
                     elx_Sockets[TmpLong].ConnectToServer(TmpStr,
                                                          TmpLong2,
                                                          ShortTmp);
                TmpStr2 := ShortTmp;

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[3], TmpStr2, true);
              end; { sock_connectto }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_gethandle ---------------------------------------------------}
         34 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value ------------------------------------------}
                FuncReturn.I := objPtr^.elx_Sockets[TmpLong].SockHandle;
              end; { sock_gethandle }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_dataavailable------------------------------------------------}
         35 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value ------------------------------------------}
                FuncReturn.B := objPtr^.elx_Sockets[TmpLong].DataAvailable;
              end; { sock_dataavailable }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_connectionalive ---------------------------------------------}
         36 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value ------------------------------------------}
                FuncReturn.B := objPtr^.elx_Sockets[TmpLong].ConnectionAlive;
              end; { sock_connectionalive }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_sendstrln ---------------------------------------------------}
         37 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- first we retrieve what string to send ---------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- and send it -----------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].SendAnsiStrLn(TmpStr);
              end; { sock_sendstrln }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_sendstr -----------------------------------------------------}
         38 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- first we retrieve what string to send ---------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- and send it -----------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].SendAnsiStr(TmpStr);
              end; { sock_sendstr }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_sendbuf -----------------------------------------------------}
         39 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- First convert the data to a record ------------------------}
                Stack2Buf(elx_Globals, TmpBuf_1, FVal(Copy(CallTypes, 2, 4)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));

                {-- and send it -----------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].SendBuf(TmpBuf_1, TmpStack[1].I, TmpStack[2].I);
              end; { sock_sendbuf }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_recvbuf -----------------------------------------------------}
         40 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- and send it -----------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].RecvBuf(TmpBuf_1, TmpStack[1].I, TmpStack[2].I);

                {-- First convert the data to a record ------------------------}
                Buf2Stack(elx_Globals, TmpBuf_1, FVal(Copy(CallTypes, 2, 3)), 0, true,
                          Boolean(FVal(Copy(CallTypes, 6, 1))));
              end; { sock_recvbuf }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_disconnect --------------------------------------------------}
         41 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- and disconnect --------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].Disconnect;
              end; { sock_disconnect }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_waitfordata -------------------------------------------------}
         42 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- and wait for the data to arrive ---------------------------}
                objPtr^.elx_Sockets[TmpLong].WaitForData;
              end; { sock_waitfordata }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_flushdata ---------------------------------------------------}
         43 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- and flush all pending data --------------------------------}
                objPtr^.elx_Sockets[TmpLong].FlushData;
              end; { sock_flushdata }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_dosleep -----------------------------------------------------}
         44 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- and sleep the number of msecs -----------------------------}
                objPtr^.elx_Sockets[TmpLong].DoSleep(TmpStack[1].i);
              end; { sock_dosleep }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_recvstrln ---------------------------------------------------}
         45 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- Connect to the specified server ---------------------------}
                FuncReturn.B :=
                   objPtr^.elx_Sockets[TmpLong].RecvStrLn(TmpStrA,
                                                          TmpStack[2].B);

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[1], TmpStrA, true);
              end; { sock_recvstrln }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_recvstring --------------------------------------------------}
         46 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- Connect to the specified server ---------------------------}
                FuncReturn.B :=
                   objPtr^.elx_Sockets[TmpLong].RecvString(TmpStrA,
                                                           TmpStack[2].B);

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[1], TmpStrA, true);
              end; { sock_recvstring }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_resolveaddr -------------------------------------------------}
         47 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- first we retrieve what string to send ---------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Resolve this address to hostname --------------------------}
                ShortTmp := TmpStr2;
                FuncReturn.B :=
                   objPtr^.elx_Sockets[TmpLong].ResolveAddr(TmpStr,
                                                            ShortTmp);
                TmpStr2 := ShortTmp;

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[2], TmpStr2, true);
              end; { sock_resolveaddr }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_connectedip -------------------------------------------------}
         48 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- get the value ---------------------------------------------}
                TmpStr := objPtr^.elx_Sockets[TmpLong].ConnectedIp;

                {-- return the value ------------------------------------------}
                SetReturnString(FuncReturn, TmpStr, false);
              end; { sock_connectedip }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setkeepalive ------------------------------------------------}
         49 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- set the value ---------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].KeepAlive := TmpStack[1].b;
              end; { sock_setkeepalive }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_getkeepalive ------------------------------------------------}
         50 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value of the keep alive setting ----------------}
                FuncReturn.B := objPtr^.elx_Sockets[TmpLong].KeepAlive;
              end; { sock_getkeepalive }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setreuseaddr ------------------------------------------------}
         51 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- set the value ---------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].ReuseAddr := TmpStack[1].b;
              end; { sock_setreuseaddr }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_getreuseaddr ------------------------------------------------}
         52 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value of the reuseaddress setting --------------}
                FuncReturn.B := objPtr^.elx_Sockets[TmpLong].ReUseAddr;
              end; { sock_getreuseaddr }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setserverhandle ---------------------------------------------}
         53 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- set the server handle -------------------------------------}
                objPtr^.elx_Sockets[TmpLong].ServerHandle := TmpStack[1].i;
              end; { sock_setserverhandle }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_getserverhandle ---------------------------------------------}
         54 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value of the serverhandle ----------------------}
                FuncReturn.I := objPtr^.elx_Sockets[TmpLong].ServerHandle;
              end; { sock_getserverhandle }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setupserver -------------------------------------------------}
         55 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- now setup the server --------------------------------------}
                ShortTmp := TmpStr;
                FuncReturn.B :=
                  objPtr^.elx_Sockets[TmpLong].SetupServer(TmpStack[1].I,
                                                           ShortTmp,
                                                           TmpStack[3].I);
                TmpStr := ShortTmp;

                {-- Return the string -----------------------------------------}
                SetReturnString(TmpStack[3], TmpStr, true);
              end; { sock_setupserver }
         {$ENDIF}


         {$IFDEF TCPIP}
         {-- sock_accept ------------------------------------------------------}
         56 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- Place the address info into an array ----------------------}
                Stack2Buf(elx_Globals, SockAddrTemp, FVal(Copy(CallTypes, 3, 4)), 0, false,
                          Boolean(FVal(Copy(CallTypes, 7, 1))));

                {-- and start accecpting connections --------------------------}
                FuncReturn.I := SockAccept(objPtr^.elx_Sockets[TmpLong].ServerHandle,
                                           @SockAddrTemp,
                                           TmpStack[2].I);
              end; { sock_accept }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_GetError ----------------------------------------------------}
         57 : begin
                {-- and return the error --------------------------------------}
                FuncReturn.I := SockErrorNo;
              end; { sock_geterror }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setsockethandle ---------------------------------------------}
         58 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- set the server handle -------------------------------------}
                objPtr^.elx_Sockets[TmpLong].SockHandle := TmpStack[1].i;
              end; { sock_setsockethandle }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_setblocking -------------------------------------------------}
         59 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- set the value ---------------------------------------------}
                objPtr^.elx_Sockets[TmpLong].Blocking := TmpStack[1].b;
              end; { sock_setBlocking }
         {$ENDIF}

         {$IFDEF TCPIP}
         {-- sock_getblocking -------------------------------------------------}
         60 : begin
                {-- get the socket slot variable ------------------------------}
                TmpLong := TmpStack[0].i;

                {-- return the value of the keep alive setting ----------------}
                FuncReturn.B := objPtr^.elx_Sockets[TmpLong].Blocking;
              end; { sock_getblocking }
         {$ENDIF}

         {-- debug_StingFreeCount ---------------------------------------------}
         61 : begin
                {-- return the amount of rfee strngs, left --------------------}
                FuncReturn.I := StrGetFreeCount(elx_Globals);
              end; { debug_StringFreeCount }

         99 : ;
       end; { case }
     end; { else }
end; { proc. ToSixHundred }

procedure ToSevenHundred;
var Counter : Longint;
    TmpPChar: PChar;
begin
  if (FuncNr - 600) < 40 then
    begin
{$IFDEF ELX_MYSQL}
      Case (FuncNr - 600) of
        {-- mysql_num_rows ----------------------------------------------------}
        1  : begin
               FuncReturn.I := mysql_num_rows(PMYSQL_RES(TmpStack[0].I));
             end; { mysql_num_rows }

        {-- mysql_num_Fields---------------------------------------------------}
        2  : begin
               FuncReturn.I := mysql_num_fields(PMYSQL_RES(TmpStack[0].I));
             end; { mysql_num_fields }

        {-- mysql_eof ---------------------------------------------------------}
        3  : begin
               {FuncReturn.B := Boolean(mysql_eof(PMYSQL_RES(TmpStack[0].I)));}
               FuncReturn.B := mysql_eof(PMYSQL_RES(TmpStack[0].I)) = '1';
             end; { mysql_eof }

        {-- mysql_field_tell --------------------------------------------------}
        4  : begin
               FuncReturn.I := mysql_field_tell(PMYSQL_RES(TmpStack[0].I));
             end; { mysql_field_tell }

        {-- mysql_affected_rows -----------------------------------------------}
        5  : begin
               FuncReturn.I := mysql_affected_rows(PMYSQL(TmpStack[0].I));
             end; { mysql_affected_rows }

        {-- mysql_insert_id ---------------------------------------------------}
        6  : begin
               FuncReturn.I := mysql_insert_id(PMYSQL(TmpStack[0].I));
             end; { mysql_insert_id }

        {-- mysql_error_no ----------------------------------------------------}
        7  : begin
               FuncReturn.I := mysql_errno(PMYSQL(TmpStack[0].I));
             end; { mysql_error_no }

        {-- mysql_info --------------------------------------------------------}
        8  : begin
               TmpStr := mysql_info(PMYSQL(TmpStack[0].I));

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_info }

        {-- mysql_reload ------------------------------------------------------}
        9  : begin
               FuncReturn.I := mysql_reload(PMYSQL(TmpStack[0].I));
             end; { mysql_reload }

        {-- mysql_thread_id ---------------------------------------------------}
        10 : begin
               FuncReturn.I := mysql_thread_id(PMYSQL(TmpStack[0].I));
             end; { mysql_thread_id }

        {-- mysql_error -------------------------------------------------------}
        11 : begin
               TmpStr :=  mysql_error(PMYSQL(TmpStack[0].I));

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_error }

        {-- mysql_close -------------------------------------------------------}
        12 : begin
               {FuncReturn.I := mysql_close(PMYSQL(TmpStack[0].I));}
               mysql_close(PMYSQL(TmpStack[0].I));
             end; { mysql_close }

        {-- mysql_select_db ---------------------------------------------------}
        13 : begin
                {-- first we retrieve what string to send ---------------------}
                if CallTypes[1] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- and set the return value -----------------------------------}
               FuncReturn.I := mysql_select_db(PMYSQL(TmpStack[0].I),
                                               PChar(TmpStr));
             end; { mysql_select_db }

        {-- mysql_query -------------------------------------------------------}
        14 : begin
                {-- first we retrieve what string to send ---------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- and set the return value -----------------------------------}
               FuncReturn.I := mysql_query(PMYSQL(TmpStack[0].I), PChar(TmpStr));
             end; { mysql_query }

        {-- mysql_real_query --------------------------------------------------}
        15 : begin
                {-- first we retrieve what string to send ---------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[1].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- and set the return value -----------------------------------}
               FuncReturn.I := mysql_real_query(PMYSQL(TmpStack[0].I), PChar(TmpStr), TmpStack[2].I);
             end; { mysql_real_query }

        {-- mysql_create_db ---------------------------------------------------}
        16 : begin
         	(*
                  {-- first we retrieve what string to send ---------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                 {-- and set the return value -----------------------------------}
                 FuncReturn.I := mysql_create_db(PMYSQL(TmpStack[0].I), PChar(TmpStr));
                *)
             end; { mysql_create_db }

        {-- mysql_drop_db -----------------------------------------------------}
        17 : begin
        	(*
                  {-- first we retrieve what string to send ---------------------}
                  if CallTypes[2] = 'c' then
                    TmpStr := Copy(TmpStack[1].C, 1, 1)
                      else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                 {-- and set the return value -----------------------------------}
                 FuncReturn.I := mysql_drop_db(PMYSQL(TmpStack[0].I), PChar(TmpStr));
                *)
             end; { mysql_drop_db }

        {-- mysql_shutdown ----------------------------------------------------}
        18 : begin
               FuncReturn.I := mysql_shutdown(PMYSQL(TmpStack[0].I));
             end; { mysql_shutdown }

        {-- mysql_debug_info --------------------------------------------------}
        19 : begin
               FuncReturn.I := mysql_dump_debug_info(PMYSQL(TmpStack[0].I));
             end; { mysql_dump_debug_info }

        {-- mysql_refresh -----------------------------------------------------}
        20 : begin
               FuncReturn.I := mysql_refresh(PMYSQL(TmpStack[0].I), TmpStack[1].I);
             end; { mysql_refresh }

        {-- mysql_kill --------------------------------------------------------}
        21 : begin
               FuncReturn.I := mysql_kill(PMYSQL(TmpStack[0].I), TmpStack[1].I);
             end; { mysql_kill }

        {-- mysql_stat --------------------------------------------------------}
        22 : begin
               TmpStr := mysql_stat(PMYSQL(TmpStack[0].I));

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_stat }

        {-- mysql_get_server_info ---------------------------------------------}
        23 : begin
               TmpStr := mysql_get_server_info(PMYSQL(TmpStack[0].I));

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_get_server_info }

        {-- mysql_get_client_info ---------------------------------------------}
        24 : begin
               TmpStr := mysql_get_client_info;

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_get_client_info }

        {-- mysql_get_host_info -----------------------------------------------}
        25 : begin
               TmpStr := mysql_get_host_info(PMYSQL(TmpStack[0].I));

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, TmpStr, false);
             end; { mysql_get_host_info }

        {-- mysql_get_proto_info ----------------------------------------------}
        26 : begin
               FuncReturn.I := mysql_get_proto_info(PMYSQL(TmpStack[0].I));
             end; { mysql_get_proto_info }

        {-- mysql_free_result -------------------------------------------------}
        27 : begin
               mysql_free_result(PMYSQL_RES(TmpStack[0].I));
             end; { mysql_free_result }

        {-- mysql_data_seek ---------------------------------------------------}
        28 : begin
               mysql_data_seek(PMYSQL_RES(TmpStack[0].I), TmpStack[1].I);
             end; { mysql_data_seek }

        {-- mysql_escape_string -----------------------------------------------}
        29 : begin
               {-- first we retrieve what string to send ---------------------}
               if CallTypes[1] = 'c' then
                 TmpStr := Copy(TmpStack[0].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[0].S, 1, -1);
               TmpStr2 := TmpStr;

               {-- allocate memory --------------------------------------------}
               TmpLong := Length(TmpStr);
               GetMem(TmpPChar, (TmpLong * 2) + 1);
               FillChar(TmpPChar^, (TmpLong * 2) + 1, #0);

               {-- and run the function --------------------------------------}
               mysql_escape_string(TmpPChar, PChar(TmpStr), TmpLong);

               {-- Set the content of the return var -------------------------}
               SetReturnString(FuncReturn, SysUtils.StrPas(TmpPChar), false);

               {-- and dispose of that memory ---------------------------------}
               FreeMem(TmpPChar);
             end; { mysql_escape_string }

        {-- mysql_debug -------------------------------------------------------}
        30 : begin
               {-- first we retrieve what string to send ---------------------}
               if CallTypes[2] = 'c' then
                 TmpStr := Copy(TmpStack[1].C, 1, 1)
                   else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

               {-- and call the function -------------------------------------}
               mysql_debug(PChar(TmpStr));
             end; { mysql_debug }

        {-- mysql_fetch_field_direct ------------------------------------------}
        31 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_fetch_field_direct }

        {-- mysql_fetch_fields ------------------------------------------------}
        32 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_fetch_fields }

        {-- mysql_row_tell ----------------------------------------------------}
        33 : begin
               FuncReturn.I := Longint(mysql_row_tell(PMYSQL_RES(TmpStack[0].I)));
             end; { mysql_row_tell }

        {-- mysql_connect -----------------------------------------------------}
        34 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[3].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr3 := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

               {-- and give back the result -----------------------------------}
(*               
               FuncReturn.i := Longint(mysql_connect(PMYSQL(TmpStack[0].I),
                                             PChar(TmpStr),          { host }
                                             PChar(TmpStr2),         { user }
                                             PChar(TmpStr3)));        { pass }
*)
{!!!!}		WriteLn('mysql_connect() is not supported anymore');
{!!!!}		Halt;                                             
             end; { mysql_connect }

        {-- mysql_real_connect ------------------------------------------------}
        35 : begin
                {-- Get string one --------------------------------------------}
                if CallTypes[2] = 'c' then
                  TmpStr := Copy(TmpStack[2].C, 1, 1)
                    else TmpStr := StrGetString(elx_Globals, TmpStack[1].S, 1, -1);

                {-- Get string two --------------------------------------------}
                if CallTypes[3] = 'c' then
                  TmpStr2 := Copy(TmpStack[3].C, 1, 1)
                    else TmpStr2 := StrGetString(elx_Globals, TmpStack[2].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[4] = 'c' then
                  TmpStr3 := Copy(TmpStack[4].C, 1, 1)
                    else TmpStr3 := StrGetString(elx_Globals, TmpStack[3].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[5] = 'c' then
                  TmpStr4 := Copy(TmpStack[5].C, 1, 1)
                    else TmpStr4 := StrGetString(elx_Globals, TmpStack[4].S, 1, -1);

                {-- Get string three ------------------------------------------}
                if CallTypes[7] = 'c' then
                  TmpStr5 := Copy(TmpStack[7].C, 1, 1)
                    else TmpStr5 := StrGetString(elx_Globals, TmpStack[6].S, 1, -1);

               {-- and give back the result -----------------------------------}
               FuncReturn.i := Longint(mysql_real_connect(PMYSQL(TmpStack[0].I),
                                             PChar(TmpStr),          { host }
                                             PChar(TmpStr2),         { user }
                                             PChar(TmpStr3),         { pass }
                                             PCHar(TmpStr4),           { db }
                                             TmpStack[5].i,         { port# }
                                             PChar(TmpStr5),   { Unix socket}
                                             TmpStack[7].i));     { options }
             end; { mysql_real_connect }

        {-- mysql_list_dbs ----------------------------------------------------}
        36 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_list_dbs }

        {-- mysql_list_tables -------------------------------------------------}
        37 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_list_tables }

        {-- mysql_list_fields -------------------------------------------------}
        38 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_list_fields }

        {-- mysql_list_processes ----------------------------------------------}
        39 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_list_processes }
      end { case }
{$ENDIF}
    end { begin }

    {-- 40 .. 99 --------------------------------------------------------------}
    else if (FuncNr - 600) > 39 then
     begin
{$IFDEF ELX_MYSQL}
       Case FuncNr - 600 of
        {-- mysql_store_result ------------------------------------------------}
        40 : begin
               FuncReturn.I := Longint(mysql_store_result(PMYSQL(TmpStack[0].I)));
             end; { mysql_store_result }

        {-- mysql_use_result --------------------------------------------------}
        41 : begin
               FuncReturn.I := Longint(mysql_use_result(PMYSQL(TmpStack[0].I)));
             end; { mysql_use_result }

        {-- mysql_row_seek ----------------------------------------------------}
        42 : begin
               { the 2nd parameter is a pointer, which can be returned by }
               { mysql_row_tell }
               {FuncReturn.I := Longint(mysql_row_seek(PMYSQL_RES(TmpStack[0].i), TMYSQL_ROW_OFFSET(TmpStack[1].I)));}
               
{!!!}          WriteLn('mysql_row_seek not implemented');
{!!!}          Halt;
             end; { mysql_row_seek }

        {-- mysql_field_seek --------------------------------------------------}
        43 : begin
               FuncReturn.I := mysql_field_seek(PMYSQL_RES(TmpStack[0].i), TmpStack[1].I);
             end; { mysql_field_seek }

        {-- mysql_fetch_row ---------------------------------------------------}
        44,                                                  { mysql_fetch_row }
        47 : begin                                     { mysql_fetch_row_assoc }
               {-- get the number of fields returned --------------------------}
               TmpLong2 := mysql_num_fields(PMYSQL_RES(TmpStack[0].I));

               {-- get the exact array table index ----------------------------}
               TmpLong3 := FVal(Copy(CallTypes, 3, 4));
               TmpLong3 := elx_Globals.IdentTable^[TmpLong3].Ref;

               {-- actually get the values ------------------------------------}
               MyRow := mysql_fetch_row(PMYSQL_RES(TmpStack[0].i));
               MyFields := PMYSQL_RES(TmpStack[0].I)^.Fields;

               {-- and add the new keys to it ---------------------------------}
               if MyRow <> nil then
                 begin
                   FuncReturn.I := 0;

                   {-- now, lets check if the current dimensions of the array -}
                   {-- match our expectations. If it does, we will not recreate}
                   {-- the whole tree, but reuse all the items ----------------}
                   if elx_Globals.DynArrayTable^[TmpLong3].HighestItem <> (TmpLong2 - 1) then
                     begin
                       {-- now, clear the array -------------------------------}
                       DynArrayFreeIt(elx_Globals, TmpLong3, false);

                       for Counter := 0 to (TmpLong2 - 1) do
                         begin
                           {-- get the string in usable format ----------------}
                           TmpPChar := MyRow^;
                           Inc(MyRow);

                           {-- if fetch_array ---------------------------------}
                           {-- get the column name too, and add it ------------}
                           if (FuncNr - 600) = 47 then
                             begin
                               TmpStr2 := StrPas(PChar(MyFields^.Name));
                               Inc(MyFields);
                             end { if }
                               else TmpStr2 := '';

                           {-- and add it to the dynamic array ----------------}
                           SetDynArrayString(TmpLong3,              { ArrayRef }
                                             Counter,               { ArrayIdx }
                                             0,               { Current StrIdx }
                                             TmpPChar,         { actual string }
                                             true,         { update hash array }
                                             TmpStr2);           { Column name }
                         end; { for }
                     end
                       else begin
                              {-- the array we are getting exactly matches --------}
                              {-- the one we already use --------------------------}
                              for Counter := 0 to (TmpLong2 - 1) do
                               with elx_Globals.DynArrayTable^[TmpLong3] do
                                begin
                                  {-- get the actual string -----------------------}
                                  TmpLong4 := ArrayPtrRec^[Counter].S;

                                  {-- get the string in usable format -------------}
                                  TmpPChar := MyRow^;
                                  Inc(MyRow);

                                  {-- if fetch_array ------------------------------}
                                  {-- get the column name too, and add it ---------}
                                  if (FuncNr - 600) = 47 then
                                   begin
                                     TmpStr2 := StrPas(PChar(MyFields^.Name));
                                     Inc(MyFields);
                                   end { if }
                                     else TmpStr2 := '';


                                  {-- and update the array ------------------------}
                                  SetDynArrayString(TmpLong3,           { ArrayRef }
                                                    Counter,            { ArrayIdx }
                                                    TmpLong4,     { Current StrIdx }
                                                    TmpPChar,      { actual string }
                                                    (TmpLong4 = 0),{ update hash array }
                                                    TmpStr2);      { Column name }
                                end; { for }
                            end; { else }
                 end
                   else FuncReturn.I := -1;
             end; { mysql_fetch_row }

        {-- mysql_fetch_lengths -----------------------------------------------}
        45 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_fetch_lengths }

        {-- mysql_fetch_field -------------------------------------------------}
        46 : begin
               // NOT IMPLEMENTED YET
             end; { mysql_fetch_field }

        {-- mysql_init --------------------------------------------------------}
        48 : begin
               FuncReturn.I := Longint(mysql_init(PMYSQL(TmpStack[0].I)));
             end; { mysql_init }
      end; { case }
{$ENDIF}
    end; { if }
end; { proc. ToSevenHundred }


begin
  ObjPtr := @elxDataRecord(CallPtr^);

  if (FuncNr < 100) then OneToHundred
    else If (FuncNr < 200) then ToTwoHundred
     else if (FuncNr < 300) then ToThreeHundred
      else if (FuncNr < 400) then ToFourHundred
       else if (FuncNr < 500) then ToFiveHundred
        else if (FuncNr < 600) then ToSixHundred
         else if (FuncNr < 700) then ToSevenHundred;
end; { proc. elx_UserFuncs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetReadKey: String;
var TempCH: Char;
begin
  {-- Get the first key ----------------------------------------------------}
  {$IFDEF WITH_FULL}
    TempCH := InputObj^.ReadKey;
  {$ENDIF}

  {-- Lets see if its a known one ------------------------------------------}
  if TempCH = #127 then                                            { Delete }
    begin
      GetReadKey := #0 + #83;
      EXIT;
    end; { if }

  {-- perhaps it was insert ------------------------------------------------}
  if TempCH = #22 then
    begin
      {-- get the next key -------------------------------------------------}
      {$IFDEF WITH_FULL}
        TempCH := InputObj^.ReadKey;
      {$ENDIF}

      {-- if it was insert that was pressed exit ---------------------------}
      if TempCH = #09 then
        begin
          GetReadKey := #0 + #82;
          EXIT;
        end; { if }
    end; { if }

  {-- if it was Escape, it was probably an arrow key -----------------------}
  if TempCH = #27 then
    begin
      {-- get the arrow key ------------------------------------------------}
      {$IFDEF WITH_FULL}
        TempCH := GetArrowKeys;
      {$ENDIF}

      {-- judge depending upon the result ----------------------------------}
      case TempCH of
        'A' : GetReadKey := #0 + #72;
        'B' : GetReadKey := #0 + #80;
        'C' : GetReadKey := #0 + #77;
        'D' : GetReadKey := #0 + #75;
        'H' : GetReadKey := #0 + #71;
        'K' : GetReadKey := #0 + #79;
          else GetReadkey := #27;
      end; { case }

      {-- now exit ---------------------------------------------------------}
      EXIT;
    end; { if }

  {-- return the default ---------------------------------------------------}
  GetReadKey := TempCH;
end; { func. GetReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_SystemFuncs(const FuncNr: Longint;
                          var   elx_GlobalsBuf;
                          var   TmpStack: Array of StackType;
                          var   CallPtr: Pointer;
                          var   FuncReturn: StackType);

var elx_Globals: elx_GlobalsType ABSOLUTE elx_GlobalsBuf;
    ObjPtr     : ^elxDataRecord;
    TmpKey     : String;
begin
  ObjPtr := @elxDataRecord(CallPtr^);

    Case FuncNr of
  {$IFDEF WITH_FULL}
      39 : FuncReturn.B := InputObj^.KeyPressed;
      44 : OutputObj^.ClearScreen;
      45 : OutputObj^.DisplayXY(TmpStack[0].I, TmpStack[1].I);
      46 : Support.TextColor(TmpStack[0].I);
      47 : with objPtr^ do
            begin
             if elx_KeyPressed then
               begin
                 TmpKey := elx_KeysString[1];
                 elx_KeyPressed := FALSE;
               end
                 else begin
                        TmpKey := GetReadKey;

                        if Length(TmpKey) > 1 then
                          begin
                            elx_KeyPressed := TRUE;
                            elx_KeysString := Copy(TmpKey, 2, 255);
                          end; { if }
                      end; { else }

             {-- Now actually allocate the memory etc -----------------------}
             if StrAllocNew(elx_Globals, FuncReturn.S, 1) then
               begin
                 {$IFNDEF MSDOS}
                   elx_Globals.StringTable^[FuncReturn.S]^.Content^ := TmpKey[1];
                 {$ELSE}
                   elx_Globals.StringTable^[FuncReturn.S]^.Content := TmpKey[1];
                 {$ENDIF}
                 elx_Globals.StringTable^[FuncReturn.S]^.Len := 1;
               end; { if }
           end; { if }
      48 : FuncReturn.I := OutputObj^.WhereX;
      49 : FuncReturn.I := OutputObj^.WhereY;
  {$ENDIF}
      50 : begin
             {$IFDEF WITH_FULL}
               Support.Delay(TmpStack[0].I);
             {$ELSE}
               {$IFDEF Win32}
                 Sleep(TmpStack[0].i);
               {$ENDIF}
             {$ENDIF}
           end; { if }
  {$IFDEF WITH_FULL}
      51 : Support.TextBackGround(TmpStack[0].I);
  {$ENDIF}
    end; { case }
end; { proc. elx_SystemFuncs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tElxBbsObj.Init;
begin
  New(elxData);
  elxData^.web_CgiWrite := nil;
end; { constructor init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tElxBbsObj.Done;
begin
  Dispose(elxData);
  elxData := NIL;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tElxBbsObj.InterpretScript(Filename, Params: String;
                                    var elx_Globals: pelx_GlobalsType;
                                    PreserveObject: Boolean): Boolean;
var SaveRadu   : Boolean;
    Success    : Boolean;
    ReUseOld   : Boolean;
{$IFDEF ELX_PROFILE}    
    ick: array[0..100] of real;
    iii: integer;
{$ENDIF}    
begin
{$IFDEF ELX_PROFILE}	
{!!!} ick[0] := GetMicroSeconds;	
{$ENDIF}

  {-- Clear the run error ---------------------------------------------------}
  RunError := 0;
  elxErrorStr := '';
  ReUseOld := (elx_Globals <> nil);

  {-- initialize memory -----------------------------------------------------}
  if NOT ReUseOld then
    begin
      New(elx_Globals);
    end; { if }

{$IFDEF ELX_PROFILE}
{!!!} ick[1] := GetMicroSeconds;	
{$ENDIF}

  {-- Initialize some global variables --------------------------------------}
  with elx_Globals^ do
    begin
      CurSrcFile := 0;
      SourceFile[CurSrcFile].SrcName := FileName;
      InterpretScript := FALSE;
      Success := FALSE;
      elxData^.elx_KeyPressed := FALSE;
      elxData^.elx_KeysString := '';
      elxData^.elx_FileBase := NIL;
      elxData^.elx_MessageBase := NIL;
      SaveRadu := LineCfg^.RaduCodes;
      LineCfg^.RaduCodes := TRUE;
      LogErrors := FALSE;
      HadErrors := FALSE;
    end; { with }

{$IFDEF ELX_PROFILE}
{!!!} ick[2] := GetMicroSeconds;	
{$ENDIF}

  {-- Run the default initialize routines -----------------------------------}
  if NOT ReUseOld then
    begin
      if NOT GlobalInitialize(elx_Globals^) then
        begin
          RaLog('!', 'Unable to initialize compiler/interpreter');
          GlobalFreeMemory(elx_Globals^, true, true);
          Dispose(elx_Globals);

          elxErrorStr := 'Unable to initialize compiler/interpreter';
          RunError := 3;
          EXIT;
        end; { fatal error }

      {-- Add the user hook procedures ------------------------------------------}
      with elx_Globals^ do
        begin
          elx_AddUserFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_AddUserFuncs;
          elx_UserFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_UserFuncs;
          elx_SystemFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_SystemFuncs;

          if Assigned(elxData^.web_Cgiwrite) then
            elx_UserWriteHook := elxData^.web_CgiWrite
             else elx_UserWriteHook := {$IFDEF FPC}@{$ENDIF}elx_UserWrite;
          elx_UserReadHook := {$IFDEF FPC}@{$ENDIF}elx_UserRead;
        end; { with }
    end; { if }

{$IFDEF ELX_PROFILE}
{!!!} ick[3] := GetMicroSeconds;	
{$ENDIF}

  {-- Read the module file back from disk ---------------------------------}
  if NOT ReUseOld then
    if NOT elx_Globals^.HadErrors then
     with elx_Globals^ do
      begin
        {-- Read module back from disk ------------------------------------}
        if NOT ReadModuleFromDisk(elx_Globals^, ElxModName(SourceFile[CurSrcFile].SrcName)) then
          begin
            RaLog('!', 'Unable to read "' + ElxModName(SourceFile[CurSrcFile].SrcName) + '"');
            GlobalFreeMemory(elx_Globals^, true, true);

            elxErrorStr := 'Unable to read "' + {ElxModName(}SourceFile[CurSrcFile].SrcName{)} + '"';
            RunError := 4;
            EXIT;
          end; { if }
{$IFDEF ELX_PROFILE}
{!!!} ick[4] := GetMicroSeconds;	
{$ENDIF}

        if elx_Globals^.ModHdr.Version <> elxVersionID then
          begin
            RaLog('!', 'Incompatible version of EleXer module (please recompile)');
            GlobalFreeMemory(elx_Globals^, true, true);

            elxErrorStr := 'Incomaptible version of EleXer module and interpreter';
            RunError := 5;
            EXIT;
          end; { if }

        {-- And interpret it if wanted -------------------------------------}
        if NOT HadErrors then
          begin
            {-- Add a block for all user defined procedures ----------------}
            if Assigned(elx_AddUserFuncsHook) then
              elx_AddUserFuncsHook(elx_Globals^, true);
          end; { if }
          
{$IFDEF ELX_PROFILE}
{!!!} ick[5] := GetMicroSeconds;	
{$ENDIF}
          
      end; { with }

{$IFDEF ELX_PROFILE}
{!!!} ick[6] := GetMicroSeconds;	
{$ENDIF}


  {-- now actually interpret this ------------------------------------------}
  if NOT elx_Globals^.HadErrors then
    begin
      Success := Interpret(elx_Globals^, Params, Pointer(elxData));

      if NOT Success then
        begin
          elxErrorStr := elx_Globals^.int_ErrorStr;
          RunError := 6;
        end; { if }
    end; { FinishInit }

{$IFDEF ELX_PROFILE}
{!!!} ick[7] := GetMicroSeconds;	
{$ENDIF}

  {-- Update the screen -----------------------------------------------------}
  {$IFDEF WITH_FULL}
    InputObj^.UpdateScrn;
  {$ENDIF}

  {-- Dispose of all used memory --------------------------------------------}
  if elxData^.elx_FileBase <> nil then
    Dispose(elxData^.elx_FileBase, Done);

{$IFDEF ELX_PROFILE}
{!!!} ick[8] := GetMicroSeconds;	
{$ENDIF}

  if elxData^.elx_MessageBase <> nil then
   with elxData^ do
    begin
      if elx_MessageBase^.MsgPtr <> nil then
        CloseMsgArea(elx_MessageBase^.MsgPtr);

      if elx_MessageBase^.EditorLines <> nil then
         Dispose(elx_MessageBase^.EditorLines, Done);

      Dispose(elx_MessageBase);
    end; { if }

{$IFDEF ELX_PROFILE}
{!!!} ick[9] := GetMicroSeconds;	
{$ENDIF}

  {-- and clear the memory --------------------------------------------------}
  if PreserveObject then
    GlobalFreeMemory(elx_Globals^, false, false)
      else GlobalFreeMemory(elx_Globals^, true, true);

{$IFDEF ELX_PROFILE}
{!!!} ick[10] := GetMicroSeconds;	
{$ENDIF}

  {-- set the total result variable -----------------------------------------}
  InterpretScript := Success;

  {-- clear the filebase and messagebase pointers ---------------------------}
  elxData^.elx_FileBase := nil;
  elxData^.elx_MessageBase := nil;
  LineCfg^.RaduCodes := SaveRadu;

  {-- clear the globals -----------------------------------------------------}
  if NOT PreserveObject then
    begin
      Dispose(elx_Globals);
      elx_Globals := nil;
    end; { if }

{$IFDEF ELX_PROFILE}
{!!!} ick[11] := GetMicroSeconds;	

for iii := 11 downto 0 do
  {!!!} writeln(iii:2, ']. Seconds: ', real(ick[iii] - ick[0]):3:4, '<br />' );
{$ENDIF}  
end; { func. InterpretScript }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tElxBbsObj.RunElexerScript(FileName,
                                    MiscData: String;
                                    LogRun  : Boolean): boolean;
var elx_Globals: pelx_GlobalsType;
begin
  {-- Initialize some variables -----------------------------------------------}
  elxData^.elx_ReturnString := '';
  RunError := 0;
  elxErrorStr := '';

  {-- add a path if necessary -------------------------------------------------}
  if JustPath(FileName) = '' then
    FileName := lineCfg^.Language^.QuesPath + Filename;

  if JustExtension(FileName) = '' then
    FileName := FileName + '.elm';

  {-- check if this file exists -----------------------------------------------}
  if (NOT FileExist(FileName)) then
    begin
      if LogRun then
        RaLog('!', 'Unable to find EleXer script: ' + FileName);
      RunElexerScript := FALSE;
      RunError := 1;

      EXIT;
    end; { if }

  {-- If we should log this, we log this --------------------------------------}
  if LogRun then
    RaLog('>', 'Running EleXer script ' + FileName);

  {-- Now actually run the script ---------------------------------------------}
  elx_Globals := nil;
  if NOT InterpretScript(FileName, MiscData, elx_Globals, false) then
    begin
      elxData^.elx_ReturnString := '!! ERROR IN EXECUTION !!';
      RunElexerScript := FALSE;
    end { if }
      else RunElexerScript := TRUE;
end; { func. RunElexerScript }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetScriptType(FName: String): String;
begin
  if JustExtension(FName) = '' then
    begin
      if (NOT FileExist(lineCfg^.Language^.QuesPath + SlowCase(FName) + '.elm')) then
         begin
           if (FileExist(lineCfg^.Language^.QuesPath + SlowCase(FName) + '.q-a')) then
             FName := Slowcase(FName) + '.q-a'
              else FName := SlowCase(FName) + '.elm';
         end
          else begin
                 FName := Slowcase(FName) + '.elm';
               end; { else }
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logQuest, 'GetScriptype: Fname="'+Fname+'", returing: "'+SUpCase(JustExtension(FName))+'"');
  {$ENDIF}

  GetScriptType := SUpCase(JustExtension(FName));
end; { func. GetScriptType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tElxBbsObj.GetElxReturnString: String;
begin
  GetElxReturnString := elxData^.elx_ReturnString;
end; { func. GetElxReturnString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { ELX_BBS }
