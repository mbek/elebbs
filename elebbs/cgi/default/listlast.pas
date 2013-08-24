program ListLast;
(*
**
** EleWEB EleXer source.
** List all lastcallers of this data
** Note2: How smaller LLSTDATA.HTM is (less lines) how faster this will run,
**        offcourse.
**
** Created: 24-may-2001
** Last update: 24-may-2001
** Written by: Maarten Bekers
**
*)

type
  SmallWord      = Integer[2];
  Longint        = Integer[4];
  Byte           = Integer[1];

  MSGTOIDXrecord = String[35];

  LASTCALLrecord = record
                     Line           : Byte;
                     Name,
                     Handle         : MSGTOIDXrecord;
                     City           : String[25];
                     Baud           : SmallWord;
                     Times          : LongInt;
                     LogOn          : String[5];
                     LogOff         : String[5];
                     Attribute      : Byte;
                   end;

                { Attribute - Bit 0 : Hidden }


var FileHandle  : Integer;
    Error       : Integer;
    LastcallInf : LastcallRecord;
    DoShow      : Boolean;

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

procedure ShowError(TmpStr: String);
begin
  WriteLn;
  WriteLn;
  WriteLn('<HTML> <BODY> <B> <H1>');
  WriteLn('Unable to display file!<BR><BR><BR>');
  WriteLn(TmpStr, '<BR><BR><BR>');
  WriteLn('</H> </B> </BODY> </HTML>');
end; { proc. ShowError }


procedure ShowLastcallEntry;
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(GetPath_htmlPath + 'llstdata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to find llstdata.htm');
    end; { if }

  if (Error = 0) AND (DoShow) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);


         {-- now convert all "internal" codes to the data we have -------}
         Count := 0;
         TmpInt := Pos('|Z', DataStr);

         while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
           begin
             if ((TmpInt + 1) < Length(DataStr)) then
               begin
                 TmpChar := Copy(DataStr, TmpInt + 2, 1);

                 Case TmpChar of
                   'A' : Replace('|ZA', LastcallInf.Name, DataStr, false);
                   'B' : Replace('|ZB', LastcallInf.Handle, DataStr, false);
                   'C' : Replace('|ZC', FStr(LastcallInf.Line), DataStr, false);
                   'D' : Replace('|ZD', FStr(LastcallInf.Baud), DataStr, false);
                   'E' : Replace('|ZE', LastcallInf.City, DataStr, false);
                   'F' : Replace('|ZF', LastcallInf.Times, DataStr, false);
                   'G' : Replace('|ZG', LastCallInf.Logon, DataStr, false);
                   'H' : Replace('|ZH', web_ConvertLink(LastcallInf.Name), DataStr, false);
                     {-- and you can add more ;-) }
                     else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                 end; { case }
               end; { case }

             Count := Count + 1;
             TmpInt := Pos('|Z', DataStr);
           end; { while }

         {-- Convert any EleBBS codes to user data ----------------------}
         DataStr := EleCodeStr(DataStr);
    
         {-- and display it to the user ---------------------------------}
         WriteLn(DataStr);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowLastcallEntry }

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;
  ShowHeader;

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      web_RunErrorScript(4, '');              { Show an "Access denied" error }
      Halt;                                           { Terminate this script }
    end; { if }

  {-- Assign the file to the proper handle etc ------------------------------}
  FileHandle := FileAssign(GetPath_Syspath + 'lastcall.bbs', 0, 3);
  Error := FileGetError(FileHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  if Error = 0 then
   begin
     {-- Show the list-user header ------------------------------------------}
     if NOT web_ShowHtmlFile('llsthead.htm') then
       ShowError('unable to find llsthead.htm');

     {-- Lets loop through the whole list of users --------------------------}
     While (Error = 0) do
       begin
         {-- Get the next record --------------------------------------------}
         FileRead(FileHandle, LastcallInf);
         Error := FileGetError(FileHandle);

         {-- And display the users name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := TRUE;

             if ReadBit(LastcallInf.Attribute, 0) then  { Hidden from userlist }
               DoShow := FALSE;

             if LastcallInf.Line <= 0 then              { Nobodys on this node }
               DoShow := FALSE;

             if LastcallInf.Name = '' then              { Nobodys on this node }
               DoShow := FALSE;

             {-- and show the entry -----------------------------------------}
             if DoShow then
               ShowLastcallEntry;
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if NOT web_ShowHtmlFile('llstfoot.htm') then
       ShowError('Unable to find llstfoot.htm');
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }

   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end.
