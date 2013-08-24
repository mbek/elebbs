program ListUSR;
(*
**
** EleWEB EleXer source.
** List users in users file
**
** Created: 24-may-2001
** Last update: 08-Aug-2002
** Written by: Maarten Bekers
**
*)



{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- User record data structures ---------------------------------------------}
{$I usrrec.inc}


var FileHandle: Integer;
    Error     : Integer;
    DoShow    : Boolean;
    UserInf   : UsersRecord;

    timing_StartTime : Real;
    timing_Finished  : Real;

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


{-- Include timing routines --------------------------------------------------}
{$I timefnc.inc}

procedure ShowUserEntry;
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(GetPath_HtmlPath + 'ulstdata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to find ulstdata.htm');
    end; { if }

  if Error = 0 then
   begin
     {-- Show the list-user header ------------------------------------------}
     while (error = 0) do
       begin
         {-- Now loop through the whole file --------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- now convert all "internal" codes to the data we have -----------}
         Count := 0;
         TmpInt := Pos('|Z', DataStr);

         while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
           begin
             if ((TmpInt + 1) < Length(DataStr)) then
               begin
                 TmpChar := Copy(DataStr, TmpInt + 2, 1);

                 Case TmpChar of
                   'A' : Replace('|ZA', UserInf.Name, DataStr, false);
                   'B' : Replace('|ZB', UserInf.Location, DataStr, false);
                   'C' : Replace('|ZC', UserInf.Organisation, DataStr, false);
                   'D' : Replace('|ZD', UserInf.Address1, DataStr, false);
                   'E' : Replace('|ZE', UserInf.Address2, DataStr, false);
                   'F' : Replace('|ZF', UserInf.Address3, DataStr, false);
                   'G' : Replace('|ZG', UserInf.Handle, DataStr, false);
                   'H' : Replace('|ZH', UserInf.LastTime, DataStr, false);
                   'I' : Replace('|ZI', UserInf.LastDate, DataStr, false);
                   'J' : Replace('|ZJ', UserInf.BirthDate, DataStr, false);
                   'K' : Replace('|ZK', web_ConvertLink(UserInf.Name), DataStr, false);
                     {-- and you can add more ;-) }
                     else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                 end; { case }
               end; { case }

             Count := Count + 1;
             TmpInt := Pos('|Z', DataStr);
           end; { while }

         {-- Convert any EleBBS codes to user data --------------------------}
         DataStr := EleCodeStr(DataStr);

         {-- and display it to the user -------------------------------------}
         WriteLn(DataStr);

       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowUserEntry }

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;
  ShowHeader;

  {-- Save the current time --------------------------------------------------}
  timing_Initialize(timing_StartTime);

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      web_RunErrorScript(4, '');              { Show an "Access denied" error }
      Halt;                                           { Terminate this script }
    end; { if }

  {-- Assign the file to the proper handle etc ------------------------------}
  FileHandle := FileAssign(GetPath_MsgBasePath + 'users.bbs', 0, 3);
  Error := FileGetError(FileHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  if Error = 0 then
   begin
     {-- Show the list-user header ------------------------------------------}
     if NOT web_ShowHtmlFile('ulsthead.htm') then
       ShowError('Unable to display ulsthead.htm');

     {-- Lets loop through the whole userlist -------------------------------}
     While (Error = 0) do
       begin
         {-- Get the next record --------------------------------------------}
         FileRead(FileHandle, UserInf);
         Error := FileGetError(FileHandle);


         {-- Is it allowed to show this user --------------------------------}
         if Error = 0 then
          begin
            DoShow := TRUE;

            if ReadBit(UserInf.Attribute2, 3) then   { Hidden from userlist }
              DoShow := FALSE;
          end
            else DoShow := FALSE;


         {-- And display the users name -------------------------------------}
         if Error = 0 then
          if DoShow then
           begin
             ShowUserEntry;
           end; { if }
       end; { while }

     {-- Show the list-user footer ------------------------------------------}
     if NOT web_ShowHtmlFile('ulstfoot.htm') then
       ShowError('Unable to display ulstfoot.htm');
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }

   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);


  {-- and stop the timer -----------------------------------------------------}
  timing_Done(timing_Finished);
  timing_ShowTimeFooter(timing_StartTime, timing_Finished);
end.
