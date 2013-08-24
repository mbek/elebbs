unit AreaSW;
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
** AreaSwitch Routines for EleBBS
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

procedure AreaSwitch(var TempStr: String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, Filerout, GenFile, Access_U,
      Ranges, Cases, LongStr, WordStr, StUtils;

procedure GiveNextArea(Files: Boolean; CheckGroup: Boolean);
var FilesInf    : FilesRecord;
    MessageInf  : MessageRecord;
    StartPos    : Longint;
    TotalItems  : Longint;
    Counter     : Longint;
    AreaFound   : Boolean;
begin
  {------------------------------ File areas ----------------------------------}
  if Files then
    begin
       TotalItems := GetFileSize(FilesFileName, SizeOf(FilesRecord));
       StartPos := Ra250Area(LineCfg^.Exitinfo^.Userinfo.FileArea) + 01;

       AreaFound := False;
       if LineCfg^.Exitinfo^.Userinfo.FileArea = 00 then
            StartPos := 01;

       {----------------- Start from the first area to the top ----------------}
       for Counter := StartPos to TotalItems do
         begin
           GetFilesRecord(FilesInf, Counter, False);

           if (FilesInf.Name <> '') then
            if (CheckFileAreaAccess(FilesINF, false, CheckGroup, false, true, 00,
                                     LineCfg^.Exitinfo^)) then
                 begin
                   LineCfg^.Exitinfo^.Userinfo.FileArea := FilesInf.AreaNum;
                   AreaFound := True;
                   BREAK;
                 end; { found an area }
         end; { for counter }

       {----- If we didn't find anything, start from zero to the prev area ----}
       if NOT AreaFound then
         for Counter := 00 to StartPos do
           begin
             GetFilesRecord(FilesInf, Counter, false);

             if (FilesInf.Name <> '') then
              if (CheckFileAreaAccess(FilesInf, false, CheckGroup, false, true, 00,
                                       LineCfg^.Exitinfo^)) then
                  begin
                    LineCfg^.Exitinfo^.Userinfo.FileArea := FilesInf.AreaNum;
                    AreaFound := true;

                    BREAK;
                  end; { found an area }
           end; { for counter }
    end; { if files }


  {--------------------------- Message areas ----------------------------------}
  if NOT Files then
    begin
      TotalItems := GetFileSize(MessagesFileName, SizeOf(MessageRecord));
      StartPos := Ra250MsgArea(LineCfg^.Exitinfo^.Userinfo.MsgArea) + 01;

      AreaFound := false;
      if LineCfg^.Exitinfo^.Userinfo.MsgArea = 00 then
          StartPos := 01;

       {---------------- Start from the first area to the top -----------------}
      for Counter := StartPos to TotalItems do
        begin
          GetMessageRecord(MessageInf, Counter, False);

          if (MessageInf.Name <> '') then
           if (CheckMsgAreaAccess(MessageInf, CheckGroup, false, 00, LineCfg^.Exitinfo^.userinfo)) then
                begin
                  LineCfg^.Exitinfo^.Userinfo.MsgArea := MessageInf.AreaNum;
                  AreaFound := TRUE;

                  BREAK;
                end; { Found an area }
        end; { for counter }

       {----- If we didn't find anything, start from zero to the prev area ----}
      if NOT AreaFound then
        for Counter := 00 to StartPos do
          begin
            GetMessageRecord(MessageInf, Counter, false);

            if (MessageInf.Name <> '') then
             if (CheckMsgAreaAccess(MessageInf, CheckGroup, false, 00, LineCfg^.Exitinfo^.userinfo)) then
                  begin
                    LineCfg^.Exitinfo^.Userinfo.MsgArea := MessageInf.AreaNum;
                    AreaFound := TRUE;

                    BREAK;
                  end; { Found an area }
          end; { for counter }
    end; { if }
end; { proc. GiveNextArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GivePreviousArea(Files: Boolean; CheckGroup: Boolean);
var FilesInf    : FilesRecord;
    MessageInf  : MessageRecord;
    StartPos    : Longint;
    TotalItems  : Longint;
    Counter     : Longint;
    AreaFound   : Boolean;
begin
  {------------------------------ File areas ----------------------------------}
  if Files then
    begin
      TotalItems := GetFileSize(FilesFileName, SizeOf(FilesRecord));
      StartPos := Ra250Area(LineCfg^.Exitinfo^.Userinfo.FileArea) - 01;

      AreaFound := false;
      if LineCfg^.Exitinfo^.Userinfo.FileArea = 00 then StartPos := 01;

      {------------------ Start from the last area to the top -----------------}
      for Counter := StartPos downto 00 do
        begin
          GetFilesRecord(FilesInf, Counter, False);

          if (FilesInf.Name <> '') then
           if (CheckFileAreaAccess(FilesInf, false, CheckGroup, false, true, 00,
                                    LineCfg^.Exitinfo^)) then
                begin
                  LineCfg^.Exitinfo^.Userinfo.FileArea := FilesInf.AreaNum;
                  AreaFound := TRUE;

                  BREAK;
                end; { Found an area }
        end; { for counter }

      {------ If we didn't find anything, start from zero to the prev area ----}
      if NOT AreaFound then
        for Counter := TotalItems downto StartPos do
          begin
            GetFilesRecord(FilesInf, Counter, false);

            if (FilesInf.Name <> '') then
             if (CheckFileAreaAccess(FilesInf, false, CheckGroup, false, true, 00,
                                      LineCfg^.Exitinfo^)) then
                 begin
                   LineCfg^.Exitinfo^.Userinfo.FileArea := FilesInf.AreaNum;
                   AreaFound := True;

                   BREAK;
                 end; { found an area }
          end; { for counter }
    end; { files }


  {--------------------------- Message areas ----------------------------------}
  if NOT Files then
    begin
      TotalItems := GetFileSize(MessagesFileName, SizeOf(MessageRecord));
      StartPos := Ra250MsgArea(LineCfg^.Exitinfo^.Userinfo.MsgArea) - 01;

      AreaFound := FALSE;
      if LineCfg^.Exitinfo^.Userinfo.MsgArea = 00 then StartPos := 01;

      {------------------ Start from the last area to the top -----------------}
      for Counter := StartPos downto 00 do
        begin
          GetMessageRecord(MessageInf, Counter, false);

          if (MessageInf.Name <> '') then
           if (CheckMsgAreaAccess(MessageInf, CheckGroup, false, 0, LineCfg^.Exitinfo^.userinfo)) then
               begin
                 LineCfg^.Exitinfo^.Userinfo.MsgArea := MessageInf.AreaNum;
                 AreaFound := true;

                 BREAK;
               end; { Found area }
        end; { for counter }

      {------ If we didn't find anything, start from zero to the prev area ----}
      if NOT AreaFound then
        for Counter := TotalItems downto StartPos do
          begin
            GetMessageRecord(MessageInf, Counter, false);

            if (MessageInf.Name <> '') then
             if (CheckMsgAreaAccess(MessageInf, CheckGroup, false, 00, LineCfg^.Exitinfo^.userinfo)) then
               begin
                 LineCfg^.Exitinfo^.Userinfo.MsgArea := MessageInf.AreaNum;
                 AreaFound := True;

                 BREAK;
               end; { found an area }
          end; { for counter }
    end; { if NOT files }

end; { proc. GivePreviousArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GivePreviousGroup(FileBase: Boolean; var Start: System.Word);
var TempName  : String;
    StartPos  : Longint;
    TotalItems: Longint;
    Counter   : Longint;
    AreaFound : Boolean;
    GroupInf  : GroupRecord;
begin
  if FileBase then TempName := FGroupsFileName
    else TempName := MGroupsFileName;

  TotalItems := GetFileSize(TempName, SizeOf(GroupRecord));
  StartPos := Ra250Group(Start, NOT FileBase) - 01;
  AreaFound := false;
  if Start = 00 then StartPos := 01;

  if StartPos >= 00 then
    for Counter := StartPos downto 00 do
      begin
        GetGroupRecord(Counter, false, NOT FileBase, GroupInf);

        if (GroupInf.Name <> '') AND (CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
          begin
            Start := GroupInf.AreaNum;
            AreaFound := True;
            BREAK;
          end; { Found area }
      end; { for counter }

  if NOT AreaFound then
    for Counter := TotalItems downto Max(StartPos, 0) do
      begin
         GetGroupRecord(Counter, false, NOT FileBase, GroupInf);

         if (GroupInf.Name <> '') AND (CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
           begin
             Start := GroupInf.AreaNum;
             AreaFound := true;
             BREAK;
           end; { Found area }
      end; { for counter }
end; { proc. GivePreviousGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GiveNextGroup(FileBase: Boolean; var Start: System.Word);
var TempName  : String;
    StartPos  : Longint;
    TotalItems: Longint;
    Counter   : Longint;
    AreaFound : Boolean;
    GroupInf  : GroupRecord;
begin
  if FileBase then TempName := FGroupsFileName
    else TempName := MGroupsFileName;

  TotalItems := GetFileSize(TempName, SizeOf(GroupRecord));
  StartPos := Ra250Group(Start, NOT FileBase) + 01;
  AreaFound := False;
  if Start = 00 then StartPos := 01;

  if StartPos >= 00 then
    for Counter := StartPos to TotalItems do
      begin
        GetGroupRecord(Counter, false, NOT FileBase, GroupInf);

        if (GroupInf.Name <> '') AND (CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
          begin
            Start := GroupInf.AreaNum;
            AreaFound := True;

            BREAK;
          end; { Found area }
      end; { for counter }

  if NOT AreaFound then
    for Counter := 00 to Max(StartPos, 0) do
      begin
        GetGroupRecord(Counter, false, NOT FileBase, GroupInf);

        if (GroupInf.Name <> '') AND (CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
           begin
             Start := GroupInf.AreaNum;
             AreaFound := True;
             BREAK;
           end; { Found area }
      end; { for counter }
end; { proc. GiveNextGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SearchAreaInGroup(Message: Boolean);
begin
  if Message then LineCfg^.Exitinfo^.Userinfo.MsgArea := 00
    else LineCfg^.Exitinfo^.Userinfo.FileArea := 00;

  GiveNextArea(NOT Message, true);
end; { proc. SearchAreaInGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AreaSwitch(var TempStr: String);
var Counter : Longint;
    FilesInf: FilesRecord;
    MsgInf  : MessageRecord;
begin
  if TempStr[1] <> '/' then TempStr := '/' + TempStr;
  TempStr := SUpcase(TempStr);

  for Counter := 01 to Length(TempStr) do
    if TempStr[Counter] = '/' then
      Case UpCase(TempStr[Counter + 01]) of
         'F' : begin                                          { File changing }
                 if TempStr[Counter + 2] = '=' then
                  begin
                    Case UpCase(TempStr[Counter + 3]) of
                       '+' : GiveNextArea(true, false);
                       '-' : GivePreviousArea(true, false);
                       '>' : GiveNextArea(true, true);
                       '<' : GivePreviousArea(true, true);
                         else LineCfg^.Exitinfo^.Userinfo.FileArea := Word(FVal(GetValue('/F=', TempStr, false)));
                    end; { Case }

                    GetFilesRecord(FilesInf, LineCfg^.Exitinfo^.Userinfo.FileArea, true);
                    LineCfg^.Exitinfo^.Userinfo.Filegroup := FilesInf.Group;
                  end; { if setting area (not group) }

                  if TempStr[Counter + 2] = 'G' then
                    begin
                      Case UpCase(TempStr[Counter + 4]) of
                        '>', '+' : GiveNextGroup(True, LineCfg^.Exitinfo^.Userinfo.FileGroup);
                        '<', '-' : GivePreviousGroup(True, LineCfg^.Exitinfo^.Userinfo.FileGroup);
                           else LineCfg^.Exitinfo^.Userinfo.FileGroup := Word(FVal(GetValue('/FG=', TempStr, False)));
                      end; { Case }

                      SearchAreaInGroup(false);
                    end; { FileGroup }

                  GetValue('/F', TempStr, true);
               end; { if 'F' }
         'M' : begin                                       { Message changing }
                 If TempStr[Counter + 2]='=' then
                  begin
                    case UpCase(TempStr[Counter + 3]) of
                       '+' : GiveNextArea(false, false);
                       '-' : GivePreviousArea(false, false);
                       '>' : GiveNextArea(false, true);
                       '<' : GivePreviousArea(false, true)
                         else LineCfg^.Exitinfo^.Userinfo.MsgArea := Word(FVal(GetValue('/M=', TempStr, False)));
                    end; { Case }

                    GetMessageRecord(MsgInf, LineCfg^.Exitinfo^.Userinfo.MsgArea, true);
                    LineCfg^.Exitinfo^.Userinfo.MsgGroup := MsgInf.Group;
                  end; { if }

                  if TempStr[Counter + 2]='G' then
                    begin
                      Case UpCase(TempStr[Counter + 4]) of
                        '>', '+' : GiveNextGroup(False, LineCfg^.Exitinfo^.Userinfo.MsgGroup);
                        '<', '-' : GivePreviousGroup(False, LineCfg^.Exitinfo^.Userinfo.MsgGroup);
                           else LineCfg^.Exitinfo^.Userinfo.MsgGroup := Word(FVal(GetValue('/MG=', TempStr, False)));
                      end; { Case }

                      SearchAreaInGroup(true);
                    end; { FileGroup }

                  GetValue('/M', TempStr, True);
               end; { if 'M' }
      end; { Case }
end; { proc. AreaSwitch }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit AREASW }
