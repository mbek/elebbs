unit WEB_SCR;
{$I compiler.inc}
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

(*
**
** Sripting routines for EleWEB
** Copyright (c) 1999-2001 by Maarten Bekers
**
** Created: 01-May-2001
** Last update : 01-May-2001
**
*)

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
  uses web_cgi,
        web_Glob,
         elx_bbs,
          elx_glob,
           unixdate,
            cases,
             cfgrec,
              strpath,
               global,
                web_abs;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RunElexerScripts(ScriptName, Parameters: String; FiltPath, AddPath: Boolean;
                          var ElxError: Integer;
                          var ElxErrorStr: AnsiString): Boolean;

procedure elxCache_RunModule(ModuleName: AnsiString;
                             Params    : AnsiString;
                             var ElxError: Longint;
                             var ElxErrorStr: AnsiString);
function PathFilter(ScriptName: String): String;
{$IFDEF MSDOS}
procedure web_WriteHook(const TmpStr: String; DoLn: Boolean); FAR;
{$ELSE}
procedure web_WriteHook(const TmpStr: AnsiString; DoLn: Boolean); 
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  maxCachedelxModules = 40;

var
  elxModuleCache   : Array[0..maxCachedelxModules] of
                       record
                         ScriptName: AnsiString;
                         TimesUsed : Longint;          { number of times used }
                         LastUsed  : Longint;{ last time used (unixtimestamp) }
                         Globals   : ^elx_GlobalsType;   { actual module info }
                       end; { record }
  FreeSlots         : Longint;
  elxCache_Hits     : Longint;
  elxCache_Misses   : Longint;

  elxObj            : pElxBbsObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure web_WriteHook(const TmpStr: String; DoLn: Boolean);
{$ELSE}
procedure web_WriteHook(const TmpStr: AnsiString; DoLn: Boolean);
{$ENDIF}
begin
  if DoLn then CgiObj^.webSendLn(TmpStr)
    else CgiObj^.webSend(TmpStr);
end; { proc. web_WriteHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PathFilter(ScriptName: String): String;
var TmpStr : String;
    Counter: Longint;
begin
  TmpStr := '';

  for Counter := 0 to Length(ScriptName) do
    if ScriptName[Counter] in ['A'..'Z', '.', 'a'..'z', '0'..'9', '-', '_'] then
      begin
        TmpStr := TmpStr + ScriptName[Counter];
      end; { if }

  PathFilter := TmpStr;
end; { func. Pathfilter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RunElexerScripts(ScriptName, Parameters: String; FiltPath, AddPath: Boolean;
                          var ElxError: Integer;
                          var ElxErrorStr: AnsiString): Boolean;
begin
  {-- fix the paths, if necessary -----------------------------------------}
  if FiltPath then
    begin
      ScriptName := PathFilter(ScriptName);
    end; { if }

  {-- and add a path if they want us too ----------------------------------}
  if AddPath then
    if JustPath(ScriptName) = '' then
      ScriptName := lineCfg^.Language^.QuesPath + ScriptName;

  if AddPath then 
   if JustExtension(ScriptName) = '' then
     ScriptName := ScriptName + '.elm';

  
  {-- run the elexer script -----------------------------------------------}
  elxCache_RunModule(ScriptName,
                     Parameters,
                     ElxError,
                     ElxErrorStr);

  {-- and see if all gone ok ----------------------------------------------}
  RunElexerScripts := (ElxError = 0);
  ElxError := elxObj^.RunError;
  ElxErrorStr := elxObj^.elxErrorStr;
end; { proc. RunElexerScripts }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elxCache_Init;
var Counter: Longint;
begin
  {-- initialize all slots to nil -------------------------------------------}
  for Counter := 0 to maxCachedElxModules do
   with elxModuleCache[Counter] do
    begin
      ScriptName := '';
      Globals := nil;
    end; { for }

  {-- and show us the total of free slots -----------------------------------}
  FreeSlots := maxcachedElxModules;
  elxCache_Hits := 0;
  elxCache_Misses := 0;
end; { proc. elxCache_Init }


function elxCache_FindNewSlot: Longint;
var Counter   : Longint;
    FoundSlot : Longint;
    TmpVar    : Longint;
begin
  {-- while there are free slots, we use them -------------------------------}
  if FreeSlots = 0 then
    begin
      FoundSlot := 0;
      TmpVar := elxModuleCache[maxCachedElxModules].LastUsed;

      {-- look for the oldest used script -----------------------------------}
      for Counter := (maxCachedElxModules - 1) downto 0 do
        if elxModuleCache[Counter].LastUsed < TmpVar then
          begin
            FoundSlot := Counter;
            TmpVar := elxModuleCache[maxCachedElxModules].LastUsed;
          end; { if }

      {-- and return our slot -----------------------------------------------}
      elxCache_FindNewSlot := FoundSlot;

      {-- dispose of the memory the elx_globals is using --------------------}
      Dispose(elxModuleCache[FoundSlot].Globals);
      elxModuleCache[FoundSlot].Globals := nil;
      elxModuleCache[FoundSlot].TimesUsed := 0;
    end
      else begin
             elxCache_FindNewSlot := (maxCachedElxModules - FreeSlots);
             Dec(FreeSlots);
           end; { else }
end; { func. elxCache_FindNewSlot }


function elxCache_FindModule(ModuleName: String): longint;
var Counter: Longint;
begin
  {-- default to not found --------------------------------------------------}
  elxCache_FindModule := -1;

  {-- try to see if the module is in the cache ------------------------------}
  for Counter := 0 to (maxCachedElxModules - FreeSlots) do
    if elxModuleCache[Counter].ScriptName = ModuleName then
      begin
        elxCache_FindModule := Counter;
        Inc(elxCache_Hits);
        BREAK;
      end; { if }

  if Result = -1 then
    Inc(elxCache_Misses);
end; { func. elxCache_FindModule }


procedure elxCache_RunModule(ModuleName: AnsiString;
                             Params    : AnsiString;
                             var ElxError: Longint;
                             var ElxErrorStr: AnsiString);
var FoundSlot: Longint;
begin
  {-- prepare the actual modulename ------------------------------------------}
  ModuleName := SLowCase(ModuleName);

  {-- first try to look up this module ---------------------------------------}
  FoundSlot := elxCache_FindModule(ModuleName);

  {-- if we dont find it, add a new entry ------------------------------------}
  if FoundSlot < 0  then
    begin
      FoundSlot := elxCache_FindNewSlot;

      {-- now update this cache position -------------------------------------}
      elxModuleCache[FoundSlot].ScriptName := ModuleName;
    end; { if }

  {-- update the current info ------------------------------------------------}
  elxModuleCache[FoundSlot].LastUsed := NowAsUnixDate;
  Inc(elxModuleCache[FoundSlot].TimesUsed);

  {-- and actually run the script --------------------------------------------}
  elxObj^.InterpretScript(ModuleName,
                          Params,
                          pelx_GlobalsType(elxModuleCache[FoundSlot].Globals),
                          false);
                          
  ElxError := elxObj^.RunError;
  ElxErrorStr := elxObj^.elxErrorStr;

  {-- if this suddenly generates an error, we can safely assume it was the ---}
  {-- last script, hence we just remove this entry ---------------------------}
  if ElxError > 0 then
    begin
      elxModuleCache[FoundSlot].ScriptName := '';
      elxModuleCache[FoundSlot].Globals := nil;
      Inc(FreeSlots);
    end; { if }
end; { proc. elxCache_RunModule }


initialization
  {-- initialize the scripting object ----------------------------------------}
  New(elxObj, Init);
  elxObj^.elxData^.web_CgiWrite := {$IFDEF FPC}@{$ENDIF}web_WriteHook;

  {-- initialize caching object ----------------------------------------------}
  elxCache_Init;

finalization
  Dispose(elxObj, Done);
end. { WEB_SCR }