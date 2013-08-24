program MakeErr;
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
** MAKEERR - Creates the GETERR.INC file to create the GETERR.EXE file
**
** Copyright (c) 1996,97,98 by Maarten Bekers
**
** Created : 26-Dec-1998
** Last update : 26-Dec-1998
**
**
*)

uses Dos;

var F     : Text;
    Data  : File;
    Total : Longint;
    TotalX: longint;
    C     : Char;
begin
  Assign(F, 'GETERR.INC');
  Rewrite(F);

  Assign(Data, 'GETERR.EXE');
  Reset(Data, 1);

  WriteLn(F, '{** Created by MAKEERR.PAS - from GETERR.EXE}');

  WriteLn(F, 'Const GetErrorArray : Array[0..', FileSize(Data) - 1, '] of Char = ');

  Total := 00;
  Write(F, '                       (');

  While Total < FileSize(Data) do
    begin
       TotalX := 0;

       Write(F, '                        ');
       While ((TotalX + Total) < (FileSize(Data))) AND (TotalX < 20) do
         begin
           BlockRead(Data, C, SizeOf(C));

           if ((TotalX+1 + Total) < FIleSize(Data)) then
             Write(F, '#', Ord(c), ', ')
              else Write(F, '#', Ord(C));

           Inc(TotalX);
         end; { while }

       WriteLn(f);
       Inc(Total, TotalX);
    end; { while }

   WriteLn(F, ');');
  Close(Data);
  Close(F);

  WriteLn('Done...');
end.
