unit DispMnu;
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
{$S-}
(*
**
** Display menus of ELCONFIG, Limits.RA listee and editor
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 30-Jan-1999
** Last update : 30-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

procedure DoColours;
procedure DoDisplay;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global, GenCfg, ListSys, LongStr, Sort_Un,
     MenuSys, MemMan, ScrnU, StrEdit, RecDif, Cases, GenFile,
     AreaDef, Area_Lst, StrPath, CentrStr, Colors;

procedure DoDisplay;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoDisplay');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Monochrome   ', 1301, #00, 'Set this option to Yes if the system has a monochrome display',1);
 AddPullItem(Menu.PullInf^[02], 'Direct write ', 1302, #00, 'Use fast direct screen writes?', 1);
 AddPullItem(Menu.PullInf^[03], 'Snow check   ', 1303, #00, 'Set to Yes to eliminate screen ''snow''', 1);
 AddPullItem(Menu.PullInf^[04], 'Display lines', 1304, #00, 'Switch the screen length, or leave on Auto', 1);

 Menu.Items   := 4;
 Menu.Width   := 22;
 Menu.Length  := 4;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Display ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(18, 06, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.MonoMode));
  WriteAT(18, 07, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.DirectWrite));
  WriteAT(18, 08, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.SnowCheck));

  Case GlobalCfg^.RaConfig^.VideoMode of
    Byte(Short) : WriteAT(18, 09, mnuNormColor, '25   ');
    Byte(Long)  : WriteAT(18, 09, mnuNormColor, '43/50');
    Byte(Auto)  : WriteAT(18, 09, mnuNormColor, 'Auto ');
  End; { Case }
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    1301 : Edit_Boolean(GlobalCfg^.RaConfig^.MonoMode, 18, 06, mnuNormColor);
    1302 : Edit_Boolean(GlobalCfg^.RaConfig^.DirectWrite, 18, 06, mnuNormColor);
    1303 : Edit_Boolean(GlobalCfg^.RaConfig^.SnowCheck, 18, 06, mnuNormColor);
    1304 : If Byte(GlobalCfg^.RaConfig^.VideoMode)<2 then Inc(GlobalCfg^.RaConfig^.VideoMode) else
            Byte(GlobalCfg^.RaConfig^.VideoMode) := 00;
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoDisplay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoColours;
var Menu   : PullRecord;
    Choice : Word;
    CH     : Char;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoColours');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Text    ', 1401, #00, 'Default text colour', 1);
 AddPullItem(Menu.PullInf^[02], 'Statbar ', 1402, #00, 'Status bar colour', 1);
 AddPullItem(Menu.PullInf^[03], 'Hilight ', 1403, #00, 'Window hilight colour', 1);
 AddPullItem(Menu.PullInf^[04], 'Window  ', 1404, #00, 'Popup window text colour', 1);
 AddPullItem(Menu.PullInf^[05], 'Border  ', 1405, #00, 'Popup window border colour', 1);
 AddPullItem(Menu.PullInf^[06], 'Hiprompt', 1406, #00, 'Prompt highlight colour', 1);
 AddPullItem(Menu.PullInf^[07], 'CRprompt', 1407, #00, 'Colour for the ''Press Return to continue'' prompt', 1);
 AddPullItem(Menu.PullInf^[08], 'Input   ', 1408, #00, 'Colour for highlight input fields (black to disable)', 1);
 AddPullItem(Menu.PullInf^[09], 'Tag     ', 1409, #00, 'Colour to display tag numbers in file list', 1);

 Menu.Items   := 9;
 Menu.Width   := 11;
 Menu.Length  := 9;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Colours ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  Choice := DoPullMenu(Menu, CH, False, False);

  with GlobalCfg^ do
  Case Choice of
    1401 : Edit_Color(MakeAttr(RaConfig^.NormFore, RaConfig^.NormBack), RaConfig^.NormFore, RaConfig^.NormBack, Menu, True);
    1402 : Edit_Color(MakeAttr(RaConfig^.StatFore, RaConfig^.StatBack), RaConfig^.StatFore, RaConfig^.StatBack, Menu, True);
    1403 : Edit_Color(MakeAttr(RaConfig^.HiFore, RaConfig^.HiBack), RaConfig^.HiFore, RaConfig^.HiBack, Menu, True);
    1404 : Edit_Color(MakeAttr(RaConfig^.WindFore, RaConfig^.WindBack), RaConfig^.WindFore, RaConfig^.WindBack, Menu, True);
    1405 : Edit_Color(MakeAttr(RaConfig^.Borderfore, RaConfig^.BorderBack), RaConfig^.BorderFore, RaConfig^.BorderBack, Menu,
                      True);
    1406 : Edit_Color(MakeAttr(RaConfig^.BarFore, RaConfig^.BarBack), RaConfig^.BarFore, RaConfig^.BarBack, Menu, True);
    1407 : Edit_Color(MakeAttr(RaConfig^.CrFore, RaConfig^.CrBack), RaConfig^.CrFore, RaConfig^.CrBack, Menu, True);
    1408 : Edit_Color(MakeAttr(RaConfig^.HiLitePromptFore, RaConfig^.HiLitePromptBack), RaConfig^.HiLitePromptFore,
           RaConfig^.HiLitePromptBack, Menu, True);
    1409 : Edit_Color(MakeAttr(RaConfig^.TagFore, RaConfig^.TagBack), RaConfig^.TagFore, RaConfig^.TagBack, Menu, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoColours }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit DISPMNU }
