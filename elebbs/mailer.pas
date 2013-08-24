unit mailer;
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
** Run the mailer front-end routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 24-Jan-1999
** Last update : 24-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

procedure ExecuteMailer(TempStr: String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Debug_U, Global, StUtils, ExecFunc, LongStr, FastScrn,
      ObjDec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ExecuteMailer(TempStr: String);
var Counter: Word;
    Success: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Executing mailer: '+ TempStr + ' (begin)');
  {$ENDIF}
  if TempStr='' then EXIT;

  LocalScreenLn(SystemMsgPrefix + 'Shelling to front-end ('+ TempStr+ ')');

  TempStr := Under2Norm(TempStr);

  RaExec(TempStr, true, false, true, false, Counter, Success);

  LocalScreenLn(SystemMsgPrefix + 'Returning from front-end shell, errorlevel '+ FStr(Counter));

  if Counter in [1..5] then Inc(Counter, 10);

  With GlobalCfg^.RaConfig^, LineCfg^ do
   begin
     if Counter=ExitLocal then Exitinfo^.Baud := 00 else
     if Counter=Exit300 then Exitinfo^.Baud := 300 else
     if Counter=Exit1200 then Exitinfo^.Baud := 1200 else
     if Counter=Exit2400 then Exitinfo^.Baud := 2400 else
     if Counter=Exit4800 then Exitinfo^.Baud := 4800 else
     if Counter=Exit9600 then Exitinfo^.Baud := 9600 else
     if Counter=Exit19k then Exitinfo^.Baud := 19200 else
     if Counter=Exit38k then Exitinfo^.Baud := 38400 else
     if Counter=Exit7200 then Exitinfo^.Baud := 7200 else
     if Counter=Exit12000 then Exitinfo^.Baud := 12000 else
     if Counter=Exit14400 then Exitinfo^.Baud := 14400 else
     if Counter=Exit16k then Exitinfo^.Baud := 16800 else
    { if Counter=ExitFax then Exitinfo^.Baud := 00 else }
     if Counter=Exit21k then Exitinfo^.Baud := 21600 else
     if Counter=Exit24k then Exitinfo^.Baud := 24000 else
     if Counter=Exit26k then Exitinfo^.Baud := 26400 else
     if Counter=Exit28k then Exitinfo^.Baud := 28800 else
     if Counter=Exit57k then Exitinfo^.Baud := 57600 else
     if Counter=Exit64k then Exitinfo^.Baud := 64000 else
     if Counter=Exit115k then Exitinfo^.Baud := 11520 else
     if Counter=Exit31k then Exitinfo^.Baud := 31200 else
     if Counter=Exit33k then Exitinfo^.Baud := 33600
      else begin
             MailerEnd := TRUE;
             defExitCode := Counter;
             Halt(Counter);
           end; { else }
   end; { case }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Opening comport (shell-to-mailer): '+FStr(LineCfg^.Exitinfo^.Baud)+'..');
    DebugObj.DebugLog(logSupport, 'Executing mailer: '+ TempStr + ' ( end )');
  {$ENDIF}
end; { proc. ExecuteMailer }


end. { unit mailer }
