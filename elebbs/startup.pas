unit Startup;
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
** Startup routines routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 08-Mar-1999
** Last update : 08-Mar-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartupBBS;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, FileRout, ElLog_U, Exitprog, HelpUnit,
     Mailer, OpenMdm, WinTitle, LongStr, WFC,
     Control, Comunit, Aptimer, ApFossil, FastScrn, objDec
     {$IFDEF WINGUI}
       ,Forms
       ,Win_Main
       ,WaitingForCaller_U
     {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartUpBBS;
begin
  {---------------------------- Execute the mailer --------------------------}
  if MailerCmdLine<>'' then
    begin
      {$IFDEF WINGUI}
        Form1.Visible := false;
      {$ENDIF}

      ExecuteMailer(MailerCmdLine);

      {$IFDEF WINGUI}
        Form1.Visible := true;
        Application.ProcessMessages;
      {$ENDIF}
    end; { if }

  {---------------------- Initialize the modem accordingly ------------------}
  OpenModem;
  SetWindowTitle('EleBBS - Node #'+FStr(LineCfg^.RaNodeNr));

  {-- If this is Unix, avoid the use of -Bxxxxx commandline -----------------}
  {$IFDEF ELEUNIX}
    if LineCfg^.ConnectStr = '' then
      LineCfg^.ConnectStr := '115200';
    if LineCfg^.Exitinfo^.Baud = 0 then
      LineCfg^.Exitinfo^.Baud := 11520;

    LineCfg^.Exitinfo^.ErrorFreeConnect := TRUE;
  {$ENDIF}

  {----------------- Check if we need to execute the frontend mailer --------}
  if MailerCmdLine='' then
   if (NOT LineCfg^.LocalLogon) AND (LineCfg^.Exitinfo^.Baud<1) AND (NOT LineCfg^.ReLogOnBBS) then
     begin
       {$IFDEF WINGUI}
         Form1.Visible := false;
         WaitingForCallerform.WaitingForCallerTimer.Enabled := true;
       {$ENDIF}

       WfcObj^.WaitingForCaller;
       contrObj^.SetTimeout;

       {$IFDEF WINGUI}
         Form1.Visible := true;
       {$ENDIF}
     end; { Waiting For Caller }

  {------------------ Let the Sysop know what we're up to -------------------}
  {$IFNDEF ELEUNIX}
    if LineCfg^.Exitinfo^.Baud>00 then
      begin
        LocalScreenLn(SystemMsgPrefix + 'Incoming call at ' + FStr(LineCfg^.Exitinfo^.Baud) + ' BPS, node ' +
           FStr(LineCfg^.RaNodeNr));
      end
       else LocalScreenLn(SystemMsgPrefix + 'Local mode, node ' + FStr(LineCfg^.RaNodeNr));
  {$ENDIF}


  {---------------- Make sure we use the correct BPS rate -------------------}
{!  if ComObj <> NIL then }
{!   if NOT Modem^.LockModem then }
{!     ComObj^.Com_SetBaud(FixBaud(Exitinfo^.Baud)); }

  if (LineCfg^.Exitinfo^.Baud=00) OR (LineCfg^.LocalLogon) then
    begin
      LineCfg^.Exitinfo^.Baud := 00;
      LineCfg^.LocalLogon := true;
      LineCfg^.CarrierCheck := false;
    end; { if }

  {--------------- Setup everything we need to really get going -------------}
  contrObj^.SetTimeOut;                        { Reset the inactivity timer }
  LineCfg^.CheckInActivity := true;                       { Enables check inactivity }
  contrObj^.InCheckAll := false;                  { Enables the all checker }
  NewTimerSecs(contrObj^.ControlTimer, 1);  { Check each second for carrier }
end; { proc. StartupBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Startup }
