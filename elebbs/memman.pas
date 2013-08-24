unit MemMan;
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
** Memory allocation routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 04-Apr-1998
** Last update : 04-Apr-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses ElLog_U;
{$ENDIF}

function  AllocMem(var P; Size: Longint; VarName, ProcName: String): Boolean;
procedure ReleaseMem(var P; Size: Longint);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AllocMem(var P; Size: Longint; VarName, ProcName: String): Boolean;
var Temp  : String;
    MaxStr: String;
begin
{$IFDEF MSDOS}
  if MaxAvail > Size then
    begin
{$ENDIF}
      Pointer(P) := nil;      
      Getmem(Pointer(P), Size);
      AllocMem := true;
      
      if Pointer(p) = nil then
        begin
            AllocMem := false;
            {$IFDEF WITH_DEBUG}
             {$IFDEF WITH_FULL}
              {$IFNDEF WINGUI}
               Str(Size, Temp);
	
	           {$IFDEF MSDOS}
    	           Str(MaxAvail, MaxStr);
    	       {$ELSE}
    	       	   MaxStr := '(unlimited)';
    	       {$ENDIF}
    	       
    	       

               RaLog('!', 'Cannot allocate memory');
               RaLog('!', 'Requested is '+Temp+ ' bytes of memory, '+MaxStr+' available');
               RaLog('!', 'Memory is requested for ['+VarName+'] -- ('+ProcName+')');
              {$ENDIF}
             {$ENDIF}
            {$ENDIF}
	end; { if }
	
{$IFDEF MSDOS}
    end
     else begin
            Pointer(p) := nil;
            AllocMem := false;

            {$IFDEF WITH_DEBUG}
             {$IFDEF WITH_FULL}
               Str(Size, Temp);
               Str(MaxAvail, MaxStr);

               RaLog('!', 'Cannot allocate memory');
               RaLog('!', 'Requested is '+Temp+ ' bytes of memory, '+MaxStr+' available');
               RaLog('!', 'Memory is requested for ['+VarName+'] -- ('+ProcName+')');
             {$ENDIF}
            {$ENDIF}
          end;
{$ENDIF}
end; { proc. AllocMem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReleaseMem(var P; Size: Longint);
begin
  if Pointer(p) <> nil then
    begin
      FreeMem(Pointer(p), Size);
      Pointer(p) := nil;
    end; { if }
end; { proc. relasemem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
