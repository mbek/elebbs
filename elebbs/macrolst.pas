unit MacroLst;
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
** MACROLST.TPU, Macro's input routine for ELCONFIG
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 04-Jul-1999
** Last update : 04-Jul-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {Area_Lst,} ListSys, ScrnU, WordStr;

procedure Macro_List(var InputStr: String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var ChoosenMacro : Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Macro_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                        ItemNr: Longint; IsHiLight: Longint);

procedure SetInfo(Desc, Code: String);
begin
  Info1 := Desc;
  Info2 := Code;
end; { proc. Setinfo }

begin
  Info1 := '';
  Info2 := '';

  Case ItemNr of
    01 : SetInfo('Display another textfile                                ', '^K!');
    02 : SetInfo('Execute Q-A file                                        ', '^K@');
    03 : SetInfo('Number of callers today.                                ', '^K$');
    04 : SetInfo('Handle of last caller.                                  ', '^K%');
    05 : SetInfo('Number of files currently tagged.                       ', '^K&');
    06 : SetInfo('Total kilobytes of tagged files.                        ', '^K''');
    07 : SetInfo('Name of currently selected language.                    ', '^K(');
    08 : SetInfo('Number of messages in selected area.                    ', '^K0');
    09 : SetInfo('Number of the current template message area             ', '^K1');
    10 : SetInfo('Number of current template file area.                   ', '^K2');
    11 : SetInfo('Total system calls.                                     ', '^KA');
    12 : SetInfo('Name of last caller (any node).                         ', '^KB');
    13 : SetInfo('Starting message number of currently selected area      ', '^KD');
    14 : SetInfo('Ending message number of currently selected area        ', '^KE');
    15 : SetInfo('Number of times user has paged the Sysop in this session', '^KF');
    16 : SetInfo('Day of the week (full form).                            ', '^KG');
    17 : SetInfo('Number of users in the user file.                       ', '^KH');
    18 : SetInfo('Time in 24-hour format.                                 ', '^KI');
    19 : SetInfo('Today''s date.                                           ', '^KJ');
    20 : SetInfo('Minutes connected this call.                            ', '^KK');
    21 : SetInfo('Seconds connected (always returns zero).                ', '^KL');
    22 : SetInfo('Minutes used today.                                     ', '^KM');
    23 : SetInfo('Seconds used today (always returns zero).               ', '^KN');
    24 : SetInfo('Minutes remaining today.                                ', '^KO');
    25 : SetInfo('Seconds remaining today (always returns zero).          ', '^KP');
    26 : SetInfo('Daily time limit.                                       ', '^KQ');
    27 : SetInfo('Connect speed.                                          ', '^KR');
    28 : SetInfo('Day of the week (abbreviated form).                     ', '^KS');
    29 : SetInfo('Daily download limit (in Kb).                           ', '^KT');
    30 : SetInfo('Minutes until next system event.                        ', '^KU');
    31 : SetInfo('24 hour format time of the next event.                  ', '^KV');
    32 : SetInfo('Node number (as set on command line).                   ', '^KW');
    33 : SetInfo('Terminates the current call.                            ', '^KX');
    34 : SetInfo('Name of current template message area.                  ', '^KY');
    35 : SetInfo('Name of current template file area.                     ', '^KZ');
    36 : SetInfo('Clear to end of the current line.                       ', '^K/');
    37 : SetInfo('Displays the language prompt number nnn                 ', '^K]nnn');
    38 : SetInfo('Displays the defined semaphore path                     ', '^K#');
    39 : SetInfo('User''s default protocol setting.                        ', '^F!');
    40 : SetInfo('User''s exclude echomail areas from mail scan settings   ', '^F"');
    41 : SetInfo('User''s current file group.                              ', '^F#');
    42 : SetInfo('User''s address line 1.                                  ', '^F$');
    43 : SetInfo('User''s address line 2.                                  ', '^F%');
    44 : SetInfo('User''s address line 3.                                  ', '^F&');
    45 : SetInfo('User''s sex (Male/Female).                               ', '^F''');
    46 : SetInfo('User''s Post Billing flag setting (ON/OFF)               ', '^F(');
    47 : SetInfo('User''s current message group.                           ', '^F)');
    48 : SetInfo('Current file group number.                              ', '^F*');
    49 : SetInfo('Current message group number.                           ', '^F+');
    50 : SetInfo('User''s current full-screen editor setting (ON/OFF)      ', '^F0');
    51 : SetInfo('Current do not disturb setting (ON/OFF).                ', '^F1');
    52 : SetInfo('Current Hot-Key setting (ON/OFF).                       ', '^F2');
    53 : SetInfo('User''s handle.                                          ', '^F3');
    54 : SetInfo('Date of user''s first call to the system.                ', '^F4');
    55 : SetInfo('User''s date of birth.                                   ', '^F5');
    56 : SetInfo('Subscription expiration date.                           ', '^F6');
    57 : SetInfo('Days until subscription expiration.                     ', '^F7');
    58 : SetInfo('Current AVATAR setting (ON/OFF).                        ', '^F8');
    59 : SetInfo('File ratio (number of files).                           ', '^F9');
    60 : SetInfo('File ratio (kilobytes).                                 ', '^F:');
    61 : SetInfo('Current full-screen message viewer setting (ON/OFF)     ', '^F;');
    62 : SetInfo('Selected date format.                                   ', '^F<');
    63 : SetInfo('Current auto-message-forwarding setting (ON/OFF)        ', '^F=');
    64 : SetInfo('Name (if any) of message forwardee.                     ', '^F>');
    65 : SetInfo('User''s full name.                                       ', '^FA');
    66 : SetInfo('User''s location.                                        ', '^FB');
    67 : SetInfo('Business/Data telephone number.                         ', '^FD');
    68 : SetInfo('Voice/Home telephone number.                            ', '^FE');
    69 : SetInfo('Date of last call.                                      ', '^FF');
    70 : SetInfo('Time of last call.                                      ', '^FG');
    71 : SetInfo('A Flags setting.                                        ', '^FH');
    72 : SetInfo('B Flags setting.                                        ', '^FI');
    73 : SetInfo('C Flags setting.                                        ', '^FJ');
    74 : SetInfo('D Flags setting.                                        ', '^FK');
    75 : SetInfo('Credits remaining (cents).  The value                   ', '^FL');
    76 : SetInfo('Total messages posted.                                  ', '^FM');
    77 : SetInfo('Last message read.                                      ', '^FN');
    78 : SetInfo('Security level.                                         ', '^FO');
    79 : SetInfo('Total calls to the BBS.                                 ', '^FP');
    80 : SetInfo('Number of files uploaded.                               ', '^FQ');
    81 : SetInfo('Kilobytes uploaded.                                     ', '^FR');
    82 : SetInfo('Number of files downloaded.                             ', '^FS');
    83 : SetInfo('Kilobytes downloaded.                                   ', '^FT');
    84 : SetInfo('Minutes used today.                                     ', '^FU');
    85 : SetInfo('Current screen length setting.                          ', '^FV');
    86 : SetInfo('User''s first name only.                                 ', '^FW');
    87 : SetInfo('ANSI setting (ON/OFF).                                  ', '^FX');
    88 : SetInfo('Continue? prompt setting (ON/OFF).                      ', '^FY');
    89 : SetInfo('Screen clearing setting (ON/OFF).                       ', '^FZ');
    90 : SetInfo('Daily kilobytes remaining.                              ', '^F[');
    91 : SetInfo('User''s comment field.                                   ', '^F]');
    92 : SetInfo('Current session time limit.                             ', '^F^');
    93 : SetInfo('Time remaining this session.                            ', '^F_');
    94 : SetInfo('Organisation field                                      ', '^F~');
    95 : SetInfo('User''s age                                              ', '^F@');
  end; { case }
end; { proc. Macro_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Macro_Activate(HiLight: Longint; HiBarPos: Longint): Longint;
begin
  ChoosenMacro := HiLight;
  Macro_Activate := -1;
end; { func. Macro_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Macro_Seek(S: String): Longint;
begin
  Macro_Seek := -1;
end; { func. Macro_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Macro_GetItems: Longint;
begin
  Macro_GetItems := 95;
end; { func. Macro_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Macro_CreateNewFile: Boolean;
begin
  Macro_CreateNewFile := false;
end; { func. Macro_CreateNewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Copy_KeyPress(CH: Char; HiLight: LongInt; var HiBarPos, TopOfScrn: Longint;
                       var StartRange, EndRange: LongInt): LongInt;
begin
 Copy_KeyPress := -1;
end; { func. Copy_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Function Copy_AbortCheck(CH: Char): Boolean;
begin
  If (Ch in [#13, #27]) then Copy_AbortCheck := True
   else Copy_AbortCheck := False;
end; { func. Copy_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Macro_List(var InputStr: String);
var SaveScrn: Pointer;
    TempStr : String;
begin
  InputStr := '';
  ChoosenMacro := -1;
  CursorOff;
  SaveScreen(SaveScrn);

  DoList(true,
         05,
         65, 7, 0, 0, 0, 0, 0, 0, 0,
         04, 04, 78, 19,
         ' Macro''s ',
         'Select the macro you want to insert and press enter to add it',
         {$IFDEF FPC}@{$ENDIF}Macro_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Macro_Activate,
         {$IFDEF FPC}@{$ENDIF}Macro_Seek,
         {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}Macro_GetItems,
         {$IFDEF FPC}@{$ENDIF}Macro_CreateNewFile,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         false,
         01, 01,
         true, false);

  if ChoosenMacro <> -1 then
    begin
      Macro_GetInfo(TempStr, InputStr, TempStr, TempStr, TempStr, TempStr,
                    TempStr, TempStr, TempStr,
                    ChoosenMacro, ChoosenMacro);

      Replace('^F', ^F, InputStr);
      Replace('^K', ^K, InputStr);
    end; { if }

  RestoreScreen(SaveScrn);
  CursorOn;
end; { proc. Macro_List }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit MACROLST }
