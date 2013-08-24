program ListOnl;
(*
**
** EleWEB EleXer source.
** List all online users
** Note2: How smaller OLSTDATA.HTM is (less lines) how faster this will run,
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

  USERONrecord   = record
                     Name,
                     Handle         : MSGTOIDXrecord;
                     Line           : Byte;
                     Baud           : SmallWord;
                     City           : String[25];
                     Status,
                     Attribute      : Byte;
                     StatDesc       : String[10];
                     FreeSpace      : Array[1..90] of Byte;
                     NoCalls        : SmallWord;
                     NodeNumber     : Longint; {!}
                     LastUpdate     : Longint; {unixtimestamp, EleWEB only }
                   end;

                   { Status byte - 0 : Browsing (in a menu)
                                   1 : Uploading/downloading
                                   2 : Reading/posting messages
                                   3 : In a door/external utility
                                   4 : Chatting with sysop
                                   5 : Answering questionnaire
                                   6 : RTC
                                   7 : New user logon
                                 255 : User-defined - display StatDesc

                     Attribute   - Bit 0 : Hidden
                                       1 : Wants chat
                                       2 : Reserved for RANETMGR
                                       3 : Do not disturb flag
                                       6 : Ready (0=busy) }


var FileHandle: Integer;
    Error     : Integer;
    UserOnInf : UserOnRecord;
    DoShow    : Boolean;

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


procedure ShowOnlineEntry;
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(GetPath_HtmlPath + 'olstdata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to find olstdata.htm');
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
                   'A' : Replace('|ZA', UserOnInf.Name, DataStr, false);
                   'B' : Replace('|ZB', UserOnInf.Handle, DataStr, false);
                   'C' : Replace('|ZC', FStr(UserOnInf.NodeNumber), DataStr, false);
                   'D' : Replace('|ZD', FStr(UserOnInf.Baud), DataStr, false);
                   'E' : Replace('|ZE', UserOnInf.City, DataStr, false);
                   'F' : Replace('|ZF', UserOnInf.NoCalls, DataStr, false);
                   'G' : Replace('|ZG', web_ConvertLink(UseronInf.Name), DataStr, false);
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
end; { proc. ShowOnlineEntry }

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
  FileHandle := FileAssign(GetPath_SysPath + 'useron.bbs', 0, 3);
  Error := FileGetError(FileHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  if Error = 0 then
   begin
     {-- Show the list-user header ------------------------------------------}
     if NOT web_ShowHtmlFile('olsthead.htm') then
       ShowError('Unable to find olsthead.htm');

     {-- Lets loop through the whole list of users --------------------------}
     While (Error = 0) do
       begin
         {-- Get the next record --------------------------------------------}
         FileRead(FileHandle, UserOnInf);
         Error := FileGetError(FileHandle);

         {-- And display the users name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := TRUE;

             if ReadBit(UserOnInf.Attribute, 0) then  { Hidden from userlist }
               DoShow := FALSE;

             if UserOnInf.NodeNumber <= 0 then        { Nobodys on this node }
               DoShow := FALSE;

             if UserOnInf.Name = '' then              { Nobodys on this node }
               DoShow := FALSE;

             {-- and show the entry -----------------------------------------}
             if DoShow then
               ShowOnlineEntry;
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if NOT web_ShowHtmlFile('olstfoot.htm') then
       ShowError('Unable to find olstfoot.htm');
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
