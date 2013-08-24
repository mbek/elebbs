unit LimLst;
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
** LIMLST.TPU, Limits.RA listee and editor
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

procedure DoLimits;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global, GenCfg, ListSys, LongStr, Sort_Un,
     MenuSys, MemMan, ScrnU, StrEdit, RecDif, Cases, GenFile,
     AreaDef, Area_Lst, StrPath, CentrStr, FileObj, Ranges;

var Limits_F       : pFileObj;
    LimitsInf      : ^LimitsRecord;
    Limits_IndexPtr: Array[0..250] of Longint;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Limits_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                         ItemNr: LongInt; HiLight: LongInt);
var LimitsInf: LimitsRecord;

function LimitStr(S: String): String;
begin
  if S = FStr(UnlimitedValue) then
    LimitStr := 'Unlm.'
      else LimitStr := S;
end; { func. LimitStr }

begin
  Limits_F^.Seek(Pred(ItemNr)*SizeOf(LimitsRecord));         { FileSize checking is done bij 'DoList' }
  Limits_F^.BlkRead(LimitsInf, SizeOf(LimitsRecord));

  Info1 := FStr(LimitsInf.Security);

  Info2 := LimitStr(FStr(LimitsInf.LTime));
  Info3 := LimitStr(FStr(LimitsInf.L1200));
  Info4 := LimitStr(FStr(LimitsInf.L2400));
  Info5 := LimitStr(FStr(LimitsInf.L9600));
  Info6 := LimitStr(FStr(LimitsInf.L14400));
  Info7 := LimitStr(FStr(LimitsInf.L16800));
  Info8 := LimitStr(FStr(LimitsInf.RatioNum));
  Info9 := LimitStr(FStr(LimitsInf.RatioK));

  If Info1='' then Info1 := '[Unused]';
end; { proc. Limits_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Period2Str(B: Byte): String;
begin
  Period2Str := '';

  Case ResetType(B) of
    Never : Period2Str := 'Never  ';
    Week  : Period2Str := 'Weekly ';
    Month : Period2Str := 'Monthly';
    Year  : Period2Str := 'Yearly ';
  end; { case }

end; { func. Period2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure EditLimits(var Limits: LimitsRecord);
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  With Limits do
    begin
      Ltime := Min(UnlimitedValue, lTime);
      L300 := Min(UnlimitedValue, l300);
      L1200 := Min(UnlimitedValue, l1200);
      L2400 := Min(UnlimitedValue, l2400);
      L4800 := Min(UnlimitedValue, l4800);
      L7200 := Min(UnlimitedValue, l7200);
      L9600 := Min(UnlimitedValue, l9600);
      L12000 := Min(UnlimitedValue, l12000);
      L14400 := Min(UnlimitedValue, l14400);
      L16800 := Min(UnlimitedValue, l16800);
      L19200 := Min(UnlimitedValue, l19200);
      L38400 := Min(UnlimitedValue, l38400);
      Llocal := Min(UnlimitedValue, lLocal);
      L21600 := Min(UnlimitedValue, l21600);
      L24000 := Min(UnlimitedValue, l24000);
      L26400 := Min(UnlimitedValue, l26400);
      L28800 := Min(UnlimitedValue, l28800);
      L57600 := Min(UnlimitedValue, l57600);
      L64000 := Min(UnlimitedValue, l64000);
      L31200 := Min(UnlimitedValue, l31200);
      L33600 := Min(UnlimitedValue, l33600);
      L115200  := Min(UnlimitedValue, l115200);
    end; { with }

  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditLimits');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Security', 2801, #00, 'Security level for this entry', 01);
  AddPullItem(Menu.PullInf^[02], 'Time    ', 2802, #00, 'Daily time limit (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[03], '300     ', 2803, #00, 'Download limit (k) for 300 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[04], '1200    ', 2804, #00, 'Download limit (k) for 1200 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[05], '2400    ', 2805, #00, 'Download limit (k) for 2400 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[06], '4800    ', 2806, #00, 'Download limit (k) for 4800 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[07], '7200    ', 2807, #00, 'Download limit (k) for 7200 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[08], '9600    ', 2808, #00, 'Download limit (k) for 9600 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[09], '12000   ', 2809, #00, 'Download limit (k) for 12000 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[10], '14400   ', 2810, #00, 'Download limit (k) for 14400 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[11], '16800   ', 2811, #00, 'Download limit (k) for 16800 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[12], '19200   ', 2812, #00, 'Download limit (k) for 19200 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[13], '21600   ', 2813, #00, 'Download limit (k) for 21600 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[14], '24000   ', 2814, #00, 'Download limit (k) for 24000 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[15], '26400   ', 2815, #00, 'Download limit (k) for 26400 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[16], '28800   ', 2816, #00, 'Download limit (k) for 28800 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[17], '31200   ', 2817, #00, 'Download limit (k) for 31200 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[18], '33600   ', 2818, #00, 'Download limit (k) for 33600 bps (32767 = unlimited)', 01);
  AddPullItem(Menu.PullInf^[19], '38400   ', 2819, #00, 'Download limit (k) for 38400 bps (32767 = unlimited)', 01);

  AddPullItem(Menu.PullInf^[20], '57600   ', 2820, #00, 'Download limit (k) for 57600 bps (32767 = unlimited)', 02);
  AddPullItem(Menu.PullInf^[21], '64000   ', 2821, #00, 'Download limit (k) for 64000 bps (32767 = unlimited)', 02);
  AddPullItem(Menu.PullInf^[22], '115200  ', 2822, #00, 'Download limit (k) for 115200 bps (32767 = unlimited)', 02);
  AddPullItem(Menu.PullInf^[23], 'Local   ', 2823, #00, 'Download limit (k) for a local session (32767 = unlimited)', 02);
  AddPullItem(Menu.PullInf^[24], 'RatioNum', 2824, #00, 'Download ratio limit (number of files)', 02);
  AddPullItem(Menu.PullInf^[25], 'RatioK  ', 2825, #00, 'Download ratio limit (k)', 02);
  AddPullItem(Menu.PullInf^[26], 'PerMin  ', 2826, #00, 'Per minute logon cost (deducted from credits)', 02);
  AddPullItem(Menu.PullInf^[27], 'FlexTime', 2827, #00, 'Credits to deduct per minute after timelimit is exceeded', 02);
  AddPullItem(Menu.PullInf^[28], 'Session ', 2828, #00, 'Maximum timelimit PER SESSION (0=Disable)', 02);
  AddPullItem(Menu.PullInf^[29], 'Reset   ', 2829, #00, 'How often to reset credits', 02);
  AddPullItem(Menu.PullInf^[30], 'ResetOfs', 2830, #00, 'When to reset credits in above period (day number)', 02);
  AddPullItem(Menu.PullInf^[31], 'ResetAmt', 2831, #00, 'Credit amount to adjust users account by', 02);

  Menu.Items   := 31;
  Menu.Width   := 38;
  Menu.Length  := 19;
  Menu.X       := 40;
  Menu.Y       := 2;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := '';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;
  Menu.PosArray[2].XInc := 18;
  Menu.PosArray[2].YDec := 19;

  ShowMenu(Menu, False);

  repeat;
     SaveDirect := DirectScrnUpdate;
     DirectScrnUpdate := false;
     WriteAT(51, 04, mnuNormColor, FStr(Limits.Security) );
     WriteAT(51, 05, mnuNormColor, FStr(Limits.LTime) );
     WriteAT(51, 06, mnuNormColor, FStr(Limits.L300) );
     WriteAT(51, 07, mnuNormColor, FStr(Limits.L1200) );
     WriteAT(51, 08, mnuNormColor, FStr(Limits.L2400) );
     WriteAT(51, 09, mnuNormColor, FStr(Limits.L4800) );
     WriteAT(51, 10, mnuNormColor, FStr(Limits.L7200) );
     WriteAT(51, 11, mnuNormColor, FStr(Limits.L9600) );
     WriteAT(51, 12, mnuNormColor, FStr(Limits.L12000) );
     WriteAT(51, 13, mnuNormColor, FStr(Limits.L14400) );
     WriteAT(51, 14, mnuNormColor, FStr(Limits.L16800) );
     WriteAT(51, 15, mnuNormColor, FStr(Limits.L19200) );
     WriteAT(51, 16, mnuNormColor, FStr(Limits.L21600) );
     WriteAT(51, 17, mnuNormColor, FStr(Limits.L24000) );
     WriteAT(51, 18, mnuNormColor, FStr(Limits.L26400) );
     WriteAT(51, 19, mnuNormColor, FStr(Limits.L28800) );
     WriteAT(51, 20, mnuNormColor, FStr(Limits.L31200) );
     WriteAT(51, 21, mnuNormColor, FStr(Limits.L33600) );
     WriteAT(51, 22, mnuNormColor, FStr(Limits.L38400) );

     WriteAT(69, 04, mnuNormColor, FStr(Limits.L57600) );
     WriteAT(69, 05, mnuNormColor, FStr(Limits.L64000) );
     WriteAT(69, 06, mnuNormColor, FStr(Limits.L115200) );
     WriteAT(69, 07, mnuNormColor, FStr(Limits.LLocal) );
     WriteAT(69, 08, mnuNormColor, FStr(Limits.RatioNum) );
     WriteAT(69, 09, mnuNormColor, FStr(Limits.RatioK) );
   {$IFNDEF VER1_0_4}
    {$IFNDEF FPC}
     {$IFDEF FPC}
       {$WARNING Reals arent supported yet }
     {$ENDIF}
     WriteAT(69, 10, mnuNormColor, Real2Str(Limits.PerMinCost, 0, 3) );
     WriteAT(69, 11, mnuNormColor, Real2Str(Limits.FlexiTime, 0, 3) );
    {$ENDIF}
   {$ENDIF}
     WriteAT(69, 12, mnuNormColor, FStr(Limits.LSessionTime) );
     WriteAT(69, 13, mnuNormColor, Period2Str(Byte(Limits.ResetPeriod)));
     WriteAT(69, 14, mnuNormColor, FStr(Limits.ResetOffSet) );
     WriteAT(69, 15, mnuNormColor, FStr(Limits.ResetAmt) );
     DirectScrnUpdate := SaveDirect;

     Choice := DoPullMenu(Menu, CH, False, False);

     Case Choice of
        2801 : EditWord(Limits.Security, 51, 04, mnuEditColor, True);
        2802 : EditWord(Limits.LTime   , 51, 05, mnuEditColor, True);
        2803 : begin
                 EditWord(Limits.L300    , 51, 06, mnuEditColor, True);

                 if rdLastKey = #13 then
                   begin
                     if DoSaveChanges('Set all download kilobytes (y/N)? °', false, false) then
                       begin
                         PartClear(51, 04, 57, 22, mnuNormColor, #32);
                         PartClear(69, 04, 76, 15, mnuNormColor, #32);

                         Limits.L1200 := Limits.L300;
                         Limits.L2400 := Limits.L300;
                         Limits.L4800 := Limits.L300;
                         Limits.L7200 := Limits.L300;
                         Limits.L9600 := Limits.L300;
                         Limits.L12000 := Limits.L300;
                         Limits.L14400 := Limits.L300;
                         Limits.L16800 := Limits.L300;
                         Limits.L19200 := Limits.L300;
                         Limits.L21600 := Limits.L300;
                         Limits.L24000 := Limits.L300;
                         Limits.L26400 := Limits.L300;
                         Limits.L28800 := Limits.L300;
                         Limits.L31200 := Limits.L300;
                         Limits.L33600 := Limits.L300;
                         Limits.L38400 := Limits.L300;
                         Limits.L57600 := Limits.L300;
                         Limits.L64000 := Limits.L300;
                         Limits.L115200 := Limits.L300;
                         Limits.LLocal := Limits.L300;
                       end; { if }
                   end; { if }
               end; { if }
        2804 : EditWord(Limits.L1200   , 51, 07, mnuEditColor, True);
        2805 : EditWord(Limits.L2400   , 51, 08, mnuEditColor, True);
        2806 : EditWord(Limits.L4800   , 51, 09, mnuEditColor, True);
        2807 : EditWord(Limits.L7200   , 51, 10, mnuEditColor, True);
        2808 : EditWord(Limits.L9600   , 51, 11, mnuEditColor, True);
        2809 : EditWord(Limits.L12000  , 51, 12, mnuEditColor, True);
        2810 : EditWord(Limits.L14400  , 51, 13, mnuEditColor, True);
        2811 : EditWord(Limits.L16800  , 51, 14, mnuEditColor, True);
        2812 : EditWord(Limits.L19200  , 51, 15, mnuEditColor, True);
        2813 : EditWord(Limits.L21600  , 51, 16, mnuEditColor, True);
        2814 : EditWord(Limits.L24000  , 51, 17, mnuEditColor, True);
        2815 : EditWord(Limits.L26400  , 51, 18, mnuEditColor, True);
        2816 : EditWord(Limits.L28800  , 51, 19, mnuEditColor, True);
        2817 : EditWord(Limits.L31200  , 51, 20, mnuEditColor, True);
        2818 : EditWord(Limits.L33600  , 51, 21, mnuEditColor, True);
        2819 : EditWord(Limits.L38400  , 51, 22, mnuEditColor, True);

        2820 : EditWord(Limits.L57600    , 69, 04, mnuEditColor, True);
        2821 : EditWord(Limits.L64000    , 69, 05, mnuEditColor, True);
        2822 : EditWord(Limits.L115200   , 69, 06, mnuEditColor, True);
        2823 : EditWord(Limits.LLocal    , 69, 07, mnuEditColor, True);
        2824 : EditWord(Limits.RatioNum  , 69, 08, mnuEditColor, True);
        2825 : EditWord(Limits.RatioK    , 69, 09, mnuEditColor, True);
   {$IFNDEF VER1_0_2}
    {$IFNDEF FPC}
        2826 : EditReal(Limits.PerMinCost, 69, 10, mnuEditColor, True);
        2827 : EditReal(Limits.FlexiTime , 69, 11, mnuEditColor, True);
    {$ENDIF}
   {$ENDIF}
        2828 : EditWord(Limits.LSessionTime, 69, 12, mnuEditColor, True);
        2829 : If Byte(Limits.ResetPeriod)<Byte(Year) then Inc(Byte(Limits.Resetperiod))
                  else Limits.ResetPeriod := Never;
        2830 : EditWord(Limits.ResetOffSet, 69, 14, mnuEditColor, True);
        2831 : EditWord(Limits.ResetAmt,    69, 15, mnuEditColor, True);
     End; { case }
  Until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditLimits }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Limits_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var OldLimitsInf: LimitsRecord;
begin;
  Limits_Activate := -1;

  Limits_F^.Seek( (HiLight - 1) *SizeOf(LimitsRecord));         { FileSize checking is done bij 'DoList' }
  Limits_F^.BlkRead(LimitsInf^, SizeOf(LimitsRecord));

  OldLimitsInf:= LimitsInf^;

  EditLimits(LimitsInf^);

  If RecordsDifferent(LimitsInf^, OldLimitsInf, SizeOf(LimitsRecord)) then
        If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
              begin
                Limits_F^.Seek((HiLight-1) * SizeOf(LimitsRecord));
                Limits_F^.BlkWrite(LimitsInf^, SizeOf(LimitsRecord));
              end { SaveChanges }
               else LimitsInf^ := OldLimitsInf;

end; { proc. Limits_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Limits_Seek(Str: String): LongInt;
var LimitsInf: LimitsRecord;
begin
  Limits_F^.Seek(0);

  Limits_Seek := -1;

  While NOT Limits_F^.EOF do
   begin
     Limits_F^.BlkRead(LimitsInf, SizeOf(LimitsRecord));

     If SUpcase(Str) = SupCase(Copy(FStr(LimitsInf.Security), 1, Length(Str)))
      then begin;
              Limits_Seek := Limits_F^.FilePos div SizeOf(LimitsRecord);
              Break;
           end; { Found one }

   end; { While NOT eof }

end; { func. Limits_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Limits_GetItems: LongInt;
begin
  Limits_GetItems := Limits_F^.FileSize div SizeOf(LimitsRecord);
end; { func. Limits_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Limits_KeyPress(CH: Char; HiLight: LongInt;
                        var HiBarPos, TopOfScrn: Longint;
                        var StartRange, EndRange: LongInt): LongInt;
var LimitsInf: LimitsRecord;
begin
  Case CH of
    { Delete } #83 :begin
                      Limits_F^.Seek((HiLight-1) * SizeOf(LimitsRecord));
                      Limits_F^.BlkRead(LimitsInf, SizeOf(LimitsRecord));

                      LimitsInf.Security := 00;

                      Limits_F^.Seek((HiLight-1) * SizeOf(LimitsRecord));
                      Limits_F^.BlkWrite(LimitsInf, SizeOf(LimitsRecord));
                    end; { begin }
    { Insert } #82 :begin
                      Limits_F^.Seek(Limits_F^.FileSize);

                      FillChar(LimitsInf, SizeOf(limitsRecord), #00);

                      Limits_F^.BlkWrite(LimitsInf, SizeOf(LimitsRecord));
                    end; { Insert }
  end; { case }

  Limits_KeyPress := -1;
end; { func. Limits_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$F+,R-,S-}
Function _SortLimits(El1, El2: Pointer):Boolean;
var LimitsInf : Array[1..2] of LimitsRecord;
begin
  if IOResult>00 then;

  {$i-}
    Limits_F^.Seek( (Integer(El1^)) * SizeOf(LimitsRecord));
    Limits_F^.BlkRead(LimitsInf[1], SizeOf(LimitsRecord));
    Limits_F^.Seek( (Integer(El2^)) * SizeOf(LimitsRecord));
    Limits_F^.BlkRead(LimitsInf[2], SizeOf(LimitsRecord));
  {$i+}
  If IOResult>00 then;

  _SortLimits := LimitsInf[1].Security > LimitsInf[2].Security;
  if LimitsInf[1].Security = LimitsInf[2].Security then _SortLimits := false;
end; { func. _SortLimits }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoLimits;
var OldFMode    : Longint;
    NewLimits_F : pFileObj;
    LimitsFile  : String;
    Counter     : Byte;
    SaveScrn    : Pointer;

procedure FlushLimitsFile(DoSort: Boolean);
var Counter: Byte;
begin
  New(NewLimits_F, Init);
  NewLimits_F^.Assign(LimitsFile);
  NewLimits_F^.FileMode := ReadWriteMode + DenyAll;
  NewLImits_F^.Create(1);

  for Counter := 01 to (Limits_F^.FileSize div SizeOf(LimitsRecord)) do
    begin
      if DoSort then
        Limits_F^.Seek(Limits_IndexPtr[Counter-1] * SizeOf(LimitsRecord))
         else Limits_F^.Seek(Pred(Counter) * SizeOf(LimitsRecord));
      Limits_F^.BlkRead(LimitsInf^, SizeOf(LimitsRecord));

      If LimitsInf^.Security>00 then
        begin
          NewLimits_F^.BlkWrite(LimitsInf^, SizeOf(LimitsRecord));
        end; { if }
    end; { while }

    if NewLimits_F^.FileSize = 00 then
     begin
       FillChar(LimitsInf^, SizeOf(LimitsRecord), #00);
       NewLimits_F^.BlkWrite(LimitsInf^, SizeOf(LimitsRecord));
     end; { if }
  {$I+}

  {$i-}
    Limits_F^.Close;
    Limits_F^.Erase;
    Dispose(NewLimits_F, Done);
  {$I+}
  If IOResult>00 then;
end; { proc. DoSort }

begin
  If NOT FileExist(LimitsFileName) then
    if NOT CreateNewLimitsRa then
     begin
       CantCreate(JustName(LimitsFileName), False);
       EXIT;
     end; { if }

  LimitsFile := LimitsFileName;

  if NOT OpenFile(LimitsFile, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(LimitsFile, True);
        EXIT;
      end; { if }

  New(Limits_F, Init);
  Limits_F^.Assign(LimitsFile);
  Limits_F^.FileMode := ReadWriteMode + DenyWrite;

  if NOT Limits_F^.Open(1) then
    begin
      CantCreate(JustName(LimitsFile), True);
      EXIT;
    end; { if }

  AllocMem(LimitsInf, SizeOf(LimitsRecord), 'LimitsRecord', 'DoLimits');
  ShadFillBoxTitle(02, 04, 58, mnuScrnLen - 3, mnuBoxColor, mnuStyle, True, ' Select a security level ');
  WriteAT(03, 05, mnuMsgColor, ' Level Time  1200  2400  9600  14400 16800 R#    RK');
  WriteAT(03, 06, mnuMsgColor, Dup(mnuSngWide, 58 - 04));

  DoList(False, 03, 06, 06, 06, 06, 06, 06, 06, 06, 06,                 { ShowLen }
         02, 06, 58, mnuScrnLen - 03,                   { Window Coordinates }
         ' Select a security level ',
         '',
         {$IFDEF FPC}@{$ENDIF}Limits_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Limits_Activate,
         {$IFDEF FPC}@{$ENDIF}Limits_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Limits_KeyPress,
         {$IFDEF FPC}@{$ENDIF}Limits_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewLimitsRa,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         True, false);

  {$I-}
    Limits_F^.Close;
    RenameObjWithDrive(Limits_F, 'LINKNEW.RA');

    Limits_F^.Open(1);
  {$I+}
  If IOResult>00 then exit;

  For Counter := 00 to 250 do
    Limits_IndexPtr[Counter] := Counter;

  if (Limits_F^.FileSize div SizeOf(LimitsRecord)) < 250 then
    begin
      QuickSort(Limits_IndexPtr,
                Limits_F^.FileSize div SizeOf(LimitsRecord),
                SizeOf(Limits_IndexPtr[1]),
                {$IFDEF FPC}@{$ENDIF}_SortLimits);
    end { if }
      else begin
             MsgBox('Too many records in file to sort!', true, SaveScrn);
             GetInput;
             MsgBox('Too many records in file to sort!', false, Savescrn);
             EXIT;
           end; { if }

  FlushLimitsFile(true);

  ReleaseMem(LimitsInf, SizeOf(LimitsRecord));

  Dispose(Limits_F, Done);
end; { proc. DoLimits }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit LIMLST }
