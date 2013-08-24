unit ComUnit;
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
** Unit containing communication declarations for EleBBS
**
** Copyright (c) 1996, 1997, 1998 by Maarten Bekers
**
** Created : 29-Mar-1998
** Last update : 29-Mar-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

uses {$IFDEF WIN32}
       Windows,
       SockFunc,
       ElLog_u, W32Sngl, Telnet,
     {$ENDIF}

     {$IFDEF OS2}
       SockFunc,
       Os2Com,
       Telnet,
     {$ENDIF}

     {$IFDEF MSDOS}
       Fos_Com,
     {$ENDIF}

     {$IFDEF GO32V2}
       Fos_Com,
     {$ENDIF}

     {$IFDEF ELEUNIX}
       TtyCom,
     {$ENDIF}

      ApPort, OoCom,
      ObjDec;

Const InSize    : Word    = 16384;
      OutSize   : Word    = 16384;

{$IFDEF WITH_FULL}
var ComDevice         : ComNameType;
    ComPtr            : FossilPortPtr;
{$ENDIF}

procedure InitFossil(Lockmodem: Boolean; Baud: Longint);
procedure CloseComport;
procedure ResumeCom;
procedure PauseCom;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
  uses ApFossil, Debug_U, ApMisc, LongStr, Strings, Global;
{$ELSE}
  uses ApFossil, Debug_U, ApMisc, LongStr, SysUtils, Global;
{$ENDIF}


{$IFDEF WITH_DEBUG}
 procedure AsyncErrorProc(P: Pointer; var StatusCode: Word);
 var St: String;
 begin
   if StatusCode MOD 10000 <> 0 then
     begin
        Str(StatusCode, St);
        DebugObj.DebugLog(logAsync, 'ASYNC-ERROR: Erorr: Code('+St+'), Msg: '+StatusStr(GetAsyncStatus));
     end; { if }
  end; { if }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function Bool2Str(B: Boolean): String;
begin
  if b then Bool2Str := 'Yes'
    else Bool2Str := 'No';
end; { func. Bool2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Int_ComReadProc(var TempPtr: Pointer);
begin
  {$IFDEF WIN32}
    {$IFNDEF NOLINLOCAL}
      if NOT TelnetServ then W32Sngl.PWin32Obj(ComObj)^.Com_DataProc(TempPtr)
        else PTelnetObj(ComObj)^.Com_ReadProc(TempPtr);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSDOS}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF GO32V2}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF ELEUNIX}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF OS2}
    if NOT TelnetServ then POs2Obj(ComObj)^.Com_ReadProc(TempPtr)
                else PTelnetObj(ComObj)^.Com_ReadProc(TempPtr);
  {$ENDIF}
end; { proc. Int_ComReadProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Int_ComWriteProc(var TempPtr: Pointer);
begin
  {$IFDEF WIN32}
    {$IFNDEF NOLINLOCAL}
      if NOT TelnetServ then W32Sngl.PWin32Obj(ComObj)^.Com_DataProc(TempPtr)
        else PTelnetObj(ComObj)^.Com_WriteProc(TempPtr);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSDOS}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF GO32V2}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF ELEUNIX}
    { we dont have threaded read/write procs }
  {$ENDIF}

  {$IFDEF OS2}
    if NOT TelnetServ then POs2Obj(ComObj)^.Com_WriteProc(TempPtr)
                else PTelnetObj(ComObj)^.Com_WriteProc(TempPtr);
  {$ENDIF}
end; { proc. Int_ComWriteProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitFossil(Lockmodem: Boolean; Baud: Longint);
begin
  ComPtr := nil;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logAsync, 'Initializing fossil ...');
     if LockModem then
       DebugObj.DebugLog(logAsync, 'Locking the fossil')
        else DebugObj.DebugLog(logAsync, 'Not locking the fossil');
     DebugObj.DebugLog(logAsync, 'Opening comport at....: '+FStr(Baud));
     DebugObj.DebugLog(logAsync, 'InSize................: '+FStr(InSize));
     DebugObj.DebugLog(logAsync, 'OutSize...............: '+FStr(OutSize));
     DebugObj.DebugLog(logAsync, 'TelnetServ............: '+Bool2Str(TelnetServ));
     DebugObj.DebugLog(logAsync, 'inheritedhandle.......: '+FStr(LineCfg^.InheritedHandle));
     DebugObj.DebugLog(logAsync, 'MaxSpeed..............: '+FStr(LineCfg^.Modem^.MaxSpeed));
     {$IFDEF WITH_FULL}
       DebugObj.DebugLog(logAsync, 'ComDevice.............: '+FStr(Byte(ComDevice)));
     {$ENDIF}
  {$ENDIF}

  {$IFDEF WITH_FULL}

    Apro_Use_Old_Handle := LineCfg^.InheritedHandle;


    {$IFDEF WIN32}
      if NOT TelnetServ then ComObj := New(W32sngl.PWin32Obj, init)
               else ComObj := New(Telnet.PTelnetObj, Init);
    {$ENDIF}

    {$IFDEF MSDOS}
       ComObj := New(Fos_Com.PFossilObj, Init);
    {$ENDIF}

    {$IFDEF GO32V2}
       ComObj := New(Fos_Com.PFossilObj, Init);
    {$ENDIF}

    {$IFDEF ELEUNIX}
       ComObj := New(TtyCom.PTTyObj, Init);
    {$ENDIF}

    {$IFDEF OS2}
     if NOT TelnetServ then ComObj := New(Os2Com.POs2Obj, init)
       else ComObj := New(PTelnetObj, init);
    {$ENDIF}

     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logAsync, 'Object is initialized');
       DebugObj.DebugLog(logAsync, 'Object is initialized (2)');
     {$ENDIF}

    {$IFDEF ELEUNIX}
      {$IFDEF NOLINLOCAL}
        ComObj^.BlockAll := FALSE;
      {$ENDIF}
    {$ENDIF}


    if Baud=00 then
      Baud := LineCfg^.Modem^.MaxSpeed;

    ComObj^.Com_SetDataProc(@Int_ComReadProc, @Int_ComWriteProc);

    {$IFNDEF MSDOS}
      if LineCfg^.ComNoClose then
        ComObj^.DontClose := true;
    {$ENDIF}

     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logAsync, 'SetDataProc is done');
     {$ENDIF}

    if LockModem then
      New(FossilPortPtr(ComPtr), InitKeep(ComDevice, InSize, OutSize + 30))
       else New(FossilPortPtr(ComPtr), InitCustom(ComDevice, FixBaud(Baud),
                                                NoParity, 8, 1, InSize, OutSize + 30,
                                                defPortOptions OR defFossilOptions OR
                                                ptExecutePartialPuts));

    if ComPtr <> nil then
     begin
       FossilPortPtr(ComPtr)^.ptOptionsOff(ptPutCharWait);
       FossilPortPtr(ComPtr)^.ptOptionsOff(ptBufferGetChar);
       FossilPortPtr(ComPtr)^.ptOptionsOn(defPortOptions OR defFossilOptions);
       (*
       FossilPortPtr(ComPtr)^.HWFlowEnable(InSize - 512, 1024, hfUseRTS or hfRequireCTS);
       *)
       FossilPortPtr(ComPtr)^.HWFlowDisable;



       {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logAsync, 'Comport initialized........');
         FossilPortPtr(ComPtr)^.SetErrorProc({$IFDEF FPC}@{$ENDIF}AsyncErrorProc);
       {$ENDIF}

       FossilPortPtr(ComPtr)^.SetLine(0, NoParity, 8, 1);
       if LockModem then
         FossilPortPtr(ComPtr)^.SetLine(FixBaud(LineCfg^.Modem^.MaxSpeed), NoParity, 8, 1);
     end; { if }

     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logAsync, 'InitKeep() routine');
     {$ENDIF}

    {$IFDEF WITH_DEBUG}
       if  ComPtr<> nil then
         DebugObj.DebugLog(logAsync, 'Comname...............: '+ComNameString(ComPtr^.GetComName));
       DebugObj.DebugLog(logAsync, 'End of InitFossil (end)');
    {$ENDIF}
  {$ENDIF}

end; { proc. InitFossil }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseComport;
begin
  {$IFDEF WITH_FULL}
    Apro_Use_Old_Handle := -1;

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logAsync, 'Close-Comport.........: '+FStr(Byte(ComDevice)));
    {$ENDIF}

    if ComPtr <> nil then
      Dispose(FossilPortPtr(ComPtr), Done);
    ComPtr := nil;

    if ComObj <> nil then
      Dispose(ComObj, Done);
    ComObj := nil;

  {$ENDIF}
end; { proc. CloseComport }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PauseCom;
begin
  {$IFDEF WITH_FULL}
    if ComObj <> nil then
      ComObj^.Com_PauseCom(false);
  {$ENDIF}
end; { proc. PauseCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ResumeCom;
begin
  {$IFDEF WITH_FULL}
  {$IFNDEF ELEUNIX}
    if ComObj <> nil then
      ComObj^.Com_ResumeCom(false);

    {-- force socket into non-blocking upon return of a door -------------}
    {$IFDEF TCPIP}
    if TelnetServ then
     if ComObj <> nil then
       begin
         SockSetBlockingIo(PTelnetObj(ComObj)^.ClientRC, false);
       end; { if }
    {$ENDIF}
    
  {$ENDIF}
    { !! }
  {$ENDIF}
  
end; { proc. ResumeCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
